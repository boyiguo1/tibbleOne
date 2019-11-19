

gen_tbl_value.svydesign <- function(
  svy,
  variable,
  var_type,
  fun_type,
  test_type,
  stratified_table,
  include_pval,
  include_freq,
  expand_binary_catgs
){

  if(var_type %in% c('numeric','integer')){

    ctns_fun <- switch(
      EXPR = fun_type,
      "mean" = mean_sd.svy,
      "median" = median_iqr,
      mean_sd.svy
    )

    pval_fun <- switch(
      EXPR = test_type,
      "params" = cmp_pval_params,
      "noparm" = cmp_pval_noparm,
      cmp_pval_params
    )

    output <- ctns_tbl_value.svydesign(
      svy = svy,
      variable = variable,
      ctns_fun = ctns_fun,
      pval_fun = pval_fun,
      include_pval = include_pval,
      stratified_table = stratified_table
    )

    return(output)

  }
  # TODO: havn't modified for categorical data
  if(var_type %in% c('factor')){

    output <- catg_tbl_value(
      variable = variable,
      data = data,
      stratified_table = stratified_table,
      include_pval=include_pval,
      include_freq = include_freq,
      expand_binary_catgs=expand_binary_catgs
    )

    return(output)

  }

}


ctns_tbl_value.svydesign <- function(
  svy,
  variable,
  ctns_fun,
  pval_fun,
  stratified_table,
  include_pval
){

  vals_overall = ctns_fun(svy, variable)

  tmp <- 0
  if(stratified_table){
    # vals_by_group = tapply(
    #   data[[variable]],
    #   data[['.strat']],
    #   ctns_fun
    # )
    strata <- svy$strata %>% names()
    vals_by_group <- survey::svyby(survey::make.formula(variable),
                                   survey::make.formula(strata),
                                   svy, svymean) %>%
                      dplyr::rename( mean = !!quo(variable)) %>%
                      dplyr::transmute(trt,
                                       output = paste0(adapt_round(mean), ' (',
                                                    adapt_round(se), ')')) %>%
      spread(trt, output)
    #%>% array
  }

  if(stratified_table & include_pval){

    pval <- pval_fun(
      data = data,
      variable = variable,
      ngrps = length(vals_by_group)
    )

    vals <- c(Overall=vals_overall, vals_by_group, 'P-value' = pval)

  } else if(stratified_table & !include_pval) {

    vals <- c(Overall=vals_overall, vals_by_group)

  } else {

    vals <- c(Overall=vals_overall)

  }

  vibble(vals %>% unlist())

}

median_iqr <- function(variable){

  vals <- quantile(variable, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

  paste0(
    adapt_round(vals[2]), " [",
    adapt_round(vals[1]), "-",
    adapt_round(vals[3]), "]"
  )

}

mean_sd.svy<-function(svy, variable_name){

  res <- try(survey::svymean(survey::make.formula(variable_name), svy, na.rm = TRUE))
  # Cautious: no error prevention here
  #  if(class(.mn)[1] == 'try-error') .mn <- "NA"

  # .se <- try(sd(variable, na.rm = TRUE))
  # if(class(.sd)[1] == 'try-error') .sd <- "NA"

  paste0(
    adapt_round(res), ' (', adapt_round(survey::SE(res)), ')'
  )

}

cmp_pval_params <- function(data, variable, ngrps) {

  if( ngrps == 2 ){

    t.test(data[[variable]] ~ data[['.strat']]) %>%
      use_series("p.value") %>%
      edit_pval()

  } else {

    as.formula(paste(variable,'~ .strat'))%>%
      lm(data=data) %>%
      anova() %>%
      .[1,ncol(.)] %>%
      edit_pval()

  }
}

cmp_pval_noparm <- function(data, variable, ngrps){

  if( ngrps == 2 ){

    wilcox.test(data[[variable]] ~ data[['.strat']]) %>%
      magrittr::use_series("p.value") %>%
      edit_pval()

  } else {

    paste(variable,'~ .strat') %>%
      as.formula()%>%
      kruskal.test(data=data) %>%
      use_series("p.value") %>%
      edit_pval()

  }

}

catg_tbl_value <- function(
  variable,
  data,
  stratified_table,
  include_pval=TRUE,
  include_freq=FALSE,
  expand_binary_catgs=FALSE,
  include.missinf=FALSE
){

  counts_overall = table(data[[variable]])
  propts_overall = adapt_round(100*prop.table(counts_overall))

  n_groups <- length(counts_overall)

  if(stratified_table){

    counts_by_group = table(
      data[[variable]],
      data[['.strat']]
    )

    propts_by_group = adapt_round(
      100 * prop.table(counts_by_group, margin = 2)
    )

  }

  if(include_freq){

    cells_overall <- paste0(
      counts_overall, ' (',
      propts_overall, ')'
    ) %>%
      matrix(ncol=1) %>%
      magrittr::set_colnames('Overall')

    if(stratified_table){

      cells_by_group <- matrix(
        paste0(
          counts_by_group, ' (',
          propts_by_group, ')'
        ),
        nrow = nrow(counts_by_group),
        ncol = ncol(counts_by_group)
      ) %>%
        magrittr::set_colnames(
          colnames(counts_by_group)
        )
    }

  } else {

    cells_overall <- paste0(propts_overall) %>%
      matrix(ncol=1) %>%
      magrittr::set_colnames('Overall')

    if(stratified_table){
      cells_by_group <- matrix(
        paste0(
          propts_by_group
        ),
        nrow = nrow(counts_by_group),
        ncol = ncol(counts_by_group)
      ) %>%
        magrittr::set_colnames(
          colnames(counts_by_group)
        )
    }


  }

  if(expand_binary_catgs | n_groups > 2){
    cells_overall %<>% rbind("", .)
    if(stratified_table){
      cells_by_group %<>% rbind("",.)
    }
  }

  if(stratified_table & include_pval){

    n_reps <-
      if(expand_binary_catgs){
        n_groups
      } else {
        if(n_groups == 2){
          n_groups - 2
        } else {
          n_groups
        }
      }

    blanks <- rep("", n_reps)

    chi_tst <- suppressWarnings(try(chisq.test(counts_by_group)))

    if(class(chi_tst)[1]=='try-error'){
      pval = 'NA'
    } else {
      pval = edit_pval(chi_tst$p.value)
    }

    pval <- c(pval, blanks) %>%
      matrix(ncol=1) %>%
      set_colnames('P-value')

    if(expand_binary_catgs | n_groups > 2){

      cbind(
        cells_overall,
        cells_by_group,
        pval
      ) %>%
        tibble::as_tibble() %>%
        mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(
          Overall = cells_overall[-1],
          cells_by_group[-1,],
          'P-value' = pval
        )
      )

    }

  } else if(stratified_table & !include_pval) {

    if(expand_binary_catgs | n_groups > 2){

      cbind(
        cells_overall,
        cells_by_group
      ) %>%
        tibble::as_tibble() %>%
        mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(
          Overall = cells_overall[-1],
          cells_by_group[-1,]
        )
      )

    }

  } else {

    if(expand_binary_catgs | n_groups > 2){

      cbind(
        cells_overall
      ) %>%
        tibble::as_tibble() %>%
        mutate(
          label=rownames(cells_overall)
        )

    } else {

      vibble(
        c(Overall = cells_overall[-1])
      )

    }

  }

}