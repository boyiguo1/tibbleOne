

#' Tidy characteristics data
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' Table one is a tabular description of
#'  characteristics, e.g., demographics of patients in a clinical trial,
#'  presented overall and also stratified by a categorical variable, e.g.
#'  treatment group.
#'
#' @return a [tibble][tibble::tibble-package] containing summary values
#'   that describe characteristics of observations in `data` , which can
#'   subsequently be sent to different modes of output
#'   (see [to_word] and [to_kable]).
#'
#' @param data a data frame
#'
#' @param meta_data a meta data frame. If unspecified, a meta data frame
#'   will be created using `data`.
#'
#' @param formula an optional formula object. The left hand side of the
#'   formula should be blank. The right hand side of the formula should
#'   contain row variables for the table. The '|' symbol can be used to
#'   include stratifying variables. If this option is used, no more than
#'   two stratifying variables should be used, and they must be separated
#'    by a * symbol. If formula is used, the strat, by, and row_vars inputs
#'    are ignored.
#'
#' @param strat a character value indicating the column name in data that
#'   will be used to stratify the table
#'
#' @param by a character value indicating the column name in data that
#'   will be used to split the table into groups, prior to stratification.
#'
#' @param row_vars a character vector indicating column names of row
#'   variables in the table. If unspecified, all columns are used.
#'
#' @param specs_table_vals named vector of character values.
#'   Names should be variables, while values should be specs.
#'   Valid specs are 'mean' and 'median' (see examples).
#'
#' @param specs_table_tests named vector of character values.
#'   Names should be variables, while values should be specs.
#'   Valid specs are 'params' or 'noparm' (see examples).
#'
#' @param include_pval T/F, should the table include a column for p-values?
#'   If p-values are included, factor variables are handled using
#'   chi-square tests, continuous variables are handled using t-tests
#'   or ANOVA, depending on the number of categories in the table
#'   stratification.
#'
#' @param expand_binary_catgs T/F, should all categories be included for
#'   binary categorical variables? (This only applies to binary variables.)
#'
#' @param include_freq T/F, should frequency values be included for
#'   categorical variables?
#'
#' @param add_perc_to_cats T/F, should categorical variable labels
#'   be appended with a percent sign?
#'
#' @export
#'
#' @examples
#' library(tidyverse)
#'
#' data("pbc_tbl1")
#'
#' pbc_tbl1 %>% filter(!is.na(trt)) %>%
#'  left_join(
#'      data.frame(trt = levels(.$trt),
#'         prob = c(0.9, 0.2)
#'      )
#'  ) -> new.dat
#'
#' dgn <- survey::svydesign(~1, probs = ~prob, strata = ~trt, data = new.dat)
#'
#' # dgn$strata
#' # dgn$prob
#' # the variable to remove from the dataset is names(allprob)
#'
#' # report median albumin instead of mean
#' # use kruskal wallis test for albumin
#' tmp <- tibble_one.svydesign(
#'   svy_data = dgn,
#'   formula = ~ . | trt,
#'   expand_binary_catgs = FALSE,
#'   include_pval = FALSE
#' )
#'

# dgn <- readRDS("C:/Users/boyiguo1/Documents/GitHub/dgn.rds")
# svy_data = dgn
# formula = ~ . | trt
# formula = ~ . | Race * Gender
# meta_data = NULL
# row_vars = NULL
# strat = NULL
# by = NULL
# row_vars = NULL
# specs_table_vals = NULL
# specs_table_tests = NULL
# expand_binary_catgs = FALSE
# include_pval = FALSE
# include_freq = FALSE
# add_perc_to_cats=T

# tmp <- tibble_one.svydesign(
#     svy_data = dgn,
#     formula = ~ . | Race * Gender,
#     expand_binary_catgs = FALSE,
#     include_pval = FALSE
#   )

tibble_one.svydesign <- function(
  svy_data,
  formula = NULL,
  meta_data = NULL,
  row_vars = NULL,
  strat = NULL,
  by = NULL,
  specs_table_vals = NULL,
  specs_table_tests = NULL,
  include_pval=FALSE,
  expand_binary_catgs = FALSE,
  include_freq = FALSE,
  add_perc_to_cats = TRUE
){

  #TODO: Error prevention: if svy_data isnot a svydesign object, reprot error

  # fetch the raw data from the suvdesign object
  data <- svy_data$variables
  # Removing survey design information (sampling prob) from original dataset
  # TODO: consider to remove it from the rowvars rather from the data, it would be saver if user
  # minus this survey sampling variable in the formula
  if(!is.null(svy_data$allprob)) var_to_remove <- names(svy_data$allprob)
  if(var_to_remove %in% names(data)) data %<>% select(-!!enquo(var_to_remove))

  # save up the strata name in the used in the survey design
  # This may coincide with the strata argument
  # It is possible this is null, be cautious
  # TODO: it is possible
  wgt_strata_name <- svy_data$strata %>% names

  # Identify row, stratification, and by variables
  if( !is.null(formula) ){

    tb1_vars <- parse_tb1_formula(formula, data)

  } else {

    tb1_vars <- list(
      row_vars = vars_select(colnames(data), !!enquo(row_vars)),
      strat = vars_select(colnames(data), !!enquo(strat)),
      by = vars_select(colnames(data), !!enquo(by))
    ) %>%
      map(
        .f = function(x){
          if(length(x)==0) return(NULL)
          set_names(x, NULL)
        }
      )

    if(is.null(tb1_vars$row_vars)) stop(
      "formula or row_vars must be specified",
      call. = FALSE
    )
  }

  row_vars <- tb1_vars$row_vars
  strat <- tb1_vars$strat
  by <- tb1_vars$by

  # TODO: Error prevention: check if strat is one of the row_vars, even in the non-weighte version

  if(vec_is_empty(row_vars)){
    stop("There should be at least 1 row_var")
  }

  # Error prevention to exam if all values are missing
  for(variable in c(row_vars, strat, by)){
    # TODO: probably needs more, such as variance = 0
    if(all(is.na(data[[variable]]))){
      stop(
        glue("All values of {variable} are missing."),
        call. = FALSE
      )
    }

  }

  # make it easier to read my if-then code:
  stratified_table <- !is.null(strat)

  # in case a user misinterprets strat/by arguments
  if( !stratified_table & !is.null(by) ){
    strat = by
    by = NULL
  }

  by_table <- !is.null(by)

  # strat cannot be numeric
  if(stratified_table){
    if(is.numeric(data[[strat]])){
      stop("stratification variable must be a factor")
    }
  }

  # by cannot be numeric
  if(by_table){
    if(is.numeric(data[[by]])){
      stop("by variable must be a factor")
    }
  }

  if( !stratified_table & include_pval ){
    stop(
      "You have included p-values but have not",
      " indicated which groups to compare.",
      "\nPlease specify strat in your formula, e.g.",
      " 'formula = ~ . | strat', if you want p-values",
      call. = FALSE
    )
  }

  # Ordered factors need to be re-factored as unordered.
  # (This has to do with the default contrast method in R)

  fctrs <- purrr::map_lgl(data, is.factor) %>%
    enframe(value = 'factor') %>%
    filter(factor == TRUE) %>%
    mutate(ordered = map_lgl(name, ~is.ordered(data[[.x]])))

  if(any(fctrs$ordered)){

    for(f in fctrs$name[fctrs$ordered]){

      data[[f]] %<>% factor(ordered = FALSE, levels = levels(data[[f]]))

    }

  }

  meta <- meta_data

  if(is.null(meta)){
    # meta data set is compiled if needed
    meta <- data %>%
      mutate_if(is.character, as.factor) %>%
      select_at(vars(strat, by, row_vars)) %>%
      build_meta(
        expand_binary_catgs = expand_binary_catgs,
        add_perc_to_cats = add_perc_to_cats
      ) %>%
      check_meta()
  }



  # initialize default specification for table values / tests
  default_spec <- rep(
    x = 'default',
    times = length(row_vars)
  ) %>%
    set_names(row_vars)

  # Handle table value and test specs
  specs_table_vals <- parse_specs(default_spec, specs_table_vals)
  specs_table_tests <- parse_specs(default_spec, specs_table_tests)

  # determine what is actually in these specs
  spec_means <- any(c('default','mean') %in% specs_table_vals)
  spec_medns <- any('median' %in% specs_table_vals)

  # determine how to describe these specs
  table_value_description <-
    if ( spec_means && spec_medns ) {

      paste(
        # Note: change the wroding from "(standard deviation)" to "(standard error)"
        "Table values for continuous variables are mean (standard deviation)",
        "or median [interquartile range]. Table values for categorical",
        "variables are", if(include_freq) "count (percent)." else "percent."
      )

    } else if (spec_means && !spec_medns) {

      paste(
        # Note: change the wroding from "(standard deviation)" to "(standard error)"
        "Table values are estimated population mean (standard deviation) and",
        if(include_freq) "count (percent)" else "percent",
        "for continuous and categorical variables, respectively."
      )

    } else if (!spec_means && spec_medns) {

      paste(
        "Table values are median [interquartile range] and",
        if(include_freq) "count (percent)" else "percent",
        "for continuous and categorical variables, respectively."
      )

    }

  # Create sample size values
  n_obs <- c(
    group = 'None',
    variable = 'descr',
    labels = 'No. of observations',
    Overall = nrow(data)
  )

  # Create weighted sample size values
  # TODO: find a more reliable way to calculate the weighted total sample size
  # At least check if strat is not null
  n_obs_wgt <- c(
    group = 'None',
    variable = 'N_weight',
    labels = 'Weighted N',
    Overall = ifelse(!is.null(svy_data$prob), sum(1/svy_data$prob) %>% floor(),
                     stop("Survey design doesn't have sampling probability. Please use non-weighted version of TibbleOne"))
      # survey::svytable(survey::make.formula(wgt_strata_name), svy_data) %>% sum %>% round
  )

  # make adjustments to table parameters
  # based on whether or not a stratification
  # variable was specified. Initialize empty
  # container for stratification data

  strat_data <- NULL
  select_vec <- row_vars

  if(stratified_table){

    # if strat is specified, we know there is at least
    # one level of stratification. Therefore, we
    # identify the label of the stratification variable

    if(strat %in% meta$data$variable){
      strat_labs <- meta$data %>%
        filter(variable == strat) %>%
        pull(label)
    } else {
      strat_labs <- capitalize(strat)
    }

    # We also specify that there is one by group and
    # and initialize by_table, which we will modify if there
    # is a second stratifying variable (i.e., by != NULL)
    n_by <- 1L
    by_table <- NULL

    if(!is.null(by)){

      # For this type of table, two headers are needed.
      # To set this up, we modify the strat variable in data and
      # designate the number of participants in each by category

      if(by %in% meta$data$variable){
        by_lab <- meta$data %>%
          filter(variable == by) %>%
          pull(label)
        strat_labs <- c(strat_labs, by_lab)
      } else {
        strat_labs <- c(strat_labs, capitalize(by))
      }

      by_table <- table(data[[by]])
      n_by <- length(by_table)
      data[[strat]] %<>% interaction(data[[by]], sep='_._')

    }

    # count the number of participants in each strata
    strat_table <- table(data[[strat]])


    # add the counts of participants in each strata to the n_obs vector
    n_obs %<>% c(strat_table)



    # formalize information about strata with a list
    strat_data = list(
      # total no. of groups
      n_groups = length(strat_table) / n_by,
      # total no. of by groups
      n_by = n_by,
      # counts in the by groups
      by_table = by_table,
      # label vector
      label = strat_labs
    )

    # Create a data frame with edited strat col
    # rename the strat col so that downstream
    # code can be consistent for multiple table types

    rename_vec <- set_names(strat, ".strat")
    select_vec <- c(rename_vec, row_vars)

  }



  # the original data is modified for computing table values
  # .strat is the stratifying variable
  tbl_data <- select(data, !!!select_vec)

  # abbreviations are organized into one string
  table_abbrs <- meta$data$abbr %>%
    purrr::keep(~any(!is.na(.x))) %>%
    purrr::flatten() %>%
    purrr::map2_chr(names(.), ~ paste(.y, .x, sep = ' = ')) %>%
    sort() %>%
    paste(collapse = ', ')

  # notes are left as a named list
  table_notes <- meta$data$note %>%
    set_names(meta$data$variable) %>%
    purrr::keep(~any(!is.na(.x)))

  svy_data$variables <- tbl_data
  strat_table_wgt <- survey::svytable(survey::make.formula(".strat"), svy_data) %>% round()
  n_obs_wgt %<>% c(strat_table_wgt)

  table_data <- meta$data %>%
    select(-c(abbr,note)) %>%
    {
      # if(stratified_table){
      #   filter(., !variable %in% c(strat, by))
      # } else {
      #   .
      # }

      filter(., variable %in% row_vars)
    } %>%
    left_join(
      enframe(
        specs_table_vals,
        name = 'variable',
        value = 'fun_descr'
      ),
      by = 'variable'
    ) %>%
    left_join(
      enframe(
        specs_table_tests,
        name = 'variable',
        value = 'test_descr'
      ),
      by = 'variable'
    ) %>%
    mutate(
      tbl_val = pmap(
        .l = list(variable, type, fun_descr, test_descr),
        .f = function(.variable, .var_type, .fun_type, .test_type){
          # create a svydesigned version of gen_tbl_value_svydesign
          gen_tbl_value.svydesign(
            svy = svy_data,
            variable = .variable,
            var_type = .var_type,
            fun_type = .fun_type,
            test_type = .test_type,
            include_pval = include_pval,
            include_freq = include_freq,
            stratified_table = stratified_table,
            expand_binary_catgs = expand_binary_catgs
          )
        }
      )
    ) %>%
    select(variable, unit, labels, tbl_val, group) %>%
    tidyr::unnest(cols = c(labels, tbl_val))

  if(nrow(table_data) > 1){

    # Fix percents for factors with >=3 categories
    # This only needs to be run if there are 2+ rows in the data

    for(i in 2:nrow(table_data)){
      if(table_data$variable[i] == table_data$variable[i-1]){
        table_data$unit[i] <- NA_character_
      }
    }
  }

  # The reason that we move this from eariler to here is because I need use ".strat" variable to careate the weighted row
  # modify n_obs vector to include p-value label if needed
  if( include_pval ) {
    n_obs %<>% c("P-value" = '')
    n_obs_wgt %<>% c("P-value" = '')
  }

  # the top row of the table is initialized here (descr = descriptive)
  descr_row <- vibble(n_obs)
  n_wgt_row <- vibble(n_obs_wgt)

 # TODO: move n_wgt_row the first row in the table
 table_data %<>% bind_rows(descr_row, n_wgt_row, .)

  if( meta$add_perc_to_cats && include_freq ){

    table_data$unit %<>% gsub(
      pattern = '%',
      replacement = 'n (%)',
      x = .
    )

  }

  table_data %<>%
    mutate(
      variable = factor(
        x = variable,
        levels = c(
          'descr',
          'N_weight',
          unique(
            c(
              setdiff(row_vars, meta$var_levels),
              # order for variables without group assignment
              meta$var_levels,
              # Order for variables with group assignment
              row_vars
            )
          )
        )
      ),
      group = factor(
        x = group,
        levels = unique(
          c(
            "None",
            meta$group_levels
          )
        )
      ),
      #group = fct_relevel(group, 'None'),
      labels = case_when(!is.na(unit) ~ paste(labels, unit, sep = ', '),
                         TRUE ~ labels
      )
    ) %>%
    arrange(group, variable) %>%
    select(-unit)

  # Set table attributes

  # what type of table is this?
  # If there is no stratification at all: single_header
  # If there is one stratification variable: double_header
  # If there is a stratification and a by variable: triple_header

  table_num <- 1 + as.numeric(!is.null(strat)) + as.numeric(!is.null(by))

  table_type <- switch(
    EXPR = as.character(table_num),
    "1" = 'single_decker',
    "2" = 'double_decker',
    "3" = 'triple_decker',
    "unkown table type"
  )

  # These are read in and used in the to_yyy functions
  attr(table_data, 'type')  <- table_type
  attr(table_data, 'strat') <- strat_data
  attr(table_data, 'byvar') <- by
  attr(table_data, 'pvals') <- include_pval
  attr(table_data, 'abbrs') <- table_abbrs
  attr(table_data, 'notes') <- table_notes
  attr(table_data, 'descr') <- table_value_description
  attr(table_data, 'allcats') <- expand_binary_catgs

  # set class to include tibble_one
  # class(table_data) %<>% c('tibble_one')

  table_data

}



