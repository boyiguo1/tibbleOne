
# TODO: use stratified_table rather create strata from now.
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
  # stop("need to fix the strat now, using strat table rather than survey strata")


  if(var_type %in% c('numeric','integer')){

    ctns_fun <- switch(
      EXPR = fun_type,
      "mean" = mean_sd.svy,
      "median" = median_iqr.svy,
      mean_sd.svy
    )

    pval_fun <- switch(
      EXPR = test_type,
      "params" = cmp_pval_params.svy,
      "noparm" = cmp_pval_noparm.svy,
      cmp_pval_params.svy
    )

    output <- ctns_tbl_value.svydesign(
      svy = svy,
      variable = variable,
      ctns_fun = ctns_fun,
      fun_type = fun_type,
      pval_fun = pval_fun,
      include_pval = include_pval,
      stratified_table = stratified_table
    )

    return(output)

  }

  if(var_type %in% c('factor')){

    # warning("Do not finish yet")
    output <- catg_tbl_value.svydesign(
      svy = svy,
      variable = variable,
      # data = data,
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
  fun_type,
  pval_fun,
  stratified_table,
  include_pval
){

  vals_overall = ctns_fun(svy, variable)

  if(stratified_table){
    # vals_by_group = tapply(
    #   data[[variable]],
    #   data[['.strat']],
    #   ctns_fun
    # )
    # strata <- svy$strata %>% names()

    vals_by_group <- switch(
      EXPR = fun_type,
      "mean" =
        {survey::svyby(survey::make.formula(variable),
                       survey::make.formula(".strat"),
                       svy, survey::svymean, na.rm = T) %>%
            dplyr::rename(
              #.strat,
              mean = !!rlang::quo(variable)
              ) %>%
            left_join(
              survey::svyby(survey::make.formula(variable),
                            survey::make.formula(".strat"),
                            svy, survey::svyvar, na.rm = T) %>%
                dplyr::rename(
                  #.strat,
                  sd = !!rlang::quo(variable)
                  ),
              by = ".strat"
            ) %>%
            # TODO: make sure the trt is part of the data or it should be strata
            dplyr::transmute(.strat,
                             output = paste0(adapt_round(mean), ' (',
                                             adapt_round(sd %>% sqrt), ')')
            ) %>%
            spread(.strat, output)
        },
      "median" = {
        # TODO: suppress warning message, figure out why ci=T is necessary
        survey::svyby(survey::make.formula(variable),
                      survey::make.formula(".strat"),
                      svy, survey::svyquantile, quantiles=c(0.25,0.5, 0.75), ci=T, na.rm = T) %>%
          dplyr::rename( median = "0.5",
                         q25 = "0.25",
                         q75 = "0.75"
          ) %>%
          dplyr::transmute(.strat,
                           output = paste0(
                             adapt_round(median), ' [',
                             adapt_round(q25),"-",
                             adapt_round(q75) ,']')
          ) %>%
          spread(.strat, output)
      },
      {# Default
        # survey::svyby(survey::make.formula(variable),
        #               survey::make.formula(".strat"),
        #               svy, survey::svymean, na.rm = T) %>%
        #   dplyr::rename( mean = !!rlang::quo(variable)) %>%
        #   dplyr::transmute(.strat,
        #                    output = paste0(adapt_round(mean), ' (',
        #                                    adapt_round(se), ')')
        #   ) %>%
        #   spread(.strat, output)
        survey::svyby(survey::make.formula(variable),
                      survey::make.formula(".strat"),
                      svy, survey::svymean, na.rm = T) %>%
          dplyr::rename(
            #.strat,
            mean = !!rlang::quo(variable)
          ) %>%
          left_join(
            survey::svyby(survey::make.formula(variable),
                          survey::make.formula(".strat"),
                          svy, survey::svyvar, na.rm = T) %>%
              dplyr::rename(
                #.strat,
                sd = !!rlang::quo(variable)
              ),
            by = ".strat"
          ) %>%
          # TODO: make sure the trt is part of the data or it should be strata
          dplyr::transmute(.strat,
                           output = paste0(adapt_round(mean), ' (',
                                           adapt_round(sd %>% sqrt), ')')
          ) %>%
          spread(.strat, output)
      }
    )
    #%>% array
  }

  if(stratified_table & include_pval){
    # TODO: Implement calculation for pvalue
    stop("include_pval for weighted version has not implemented")
    pval <- pval_fun(
      data = svy, #data,
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

median_iqr.svy <- function(svy, variable_name){


  vals <- survey::svyquantile(survey::make.formula(variable_name), svy,
                              quantiles = c(0.25, 0.50, 0.75), na.rm = TRUE)

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
    adapt_round(res), ' (',
    survey::svyvar(survey::make.formula(variable_name), svy, na.rm = TRUE) %>% sqrt %>% adapt_round, ')'
  )

}

cmp_pval_params.svy <- function(data, variable, ngrps) {
  stop("'cmp_pval_params.svy' has not been implemented yet")

  # if( ngrps == 2 ){
  #
  #   t.test(data[[variable]] ~ data[['.strat']]) %>%
  #     use_series("p.value") %>%
  #     edit_pval()
  #
  # } else {
  #
  #   as.formula(paste(variable,'~ .strat'))%>%
  #     lm(data=data) %>%
  #     anova() %>%
  #     .[1,ncol(.)] %>%
  #     edit_pval()
  #
  # }
}

cmp_pval_noparm.svy <- function(data, variable, ngrps){
  stop("'cmp_pval_noparm.svy' has not been implemented yet")

  # if( ngrps == 2 ){
  #
  #   wilcox.test(data[[variable]] ~ data[['.strat']]) %>%
  #     magrittr::use_series("p.value") %>%
  #     edit_pval()
  #
  # } else {
  #
  #   paste(variable,'~ .strat') %>%
  #     as.formula()%>%
  #     kruskal.test(data=data) %>%
  #     use_series("p.value") %>%
  #     edit_pval()
  #
  # }

}

catg_tbl_value.svydesign <- function(
  svy,
  variable,
  # data,
  stratified_table,
  include_pval=TRUE,
  include_freq=FALSE,
  expand_binary_catgs=FALSE,
  include.missinf=FALSE
){

  # TODO: improve this to accomadate non weight strat
  #strata <- svy$strata %>% names()

  # TODO: round the estimate to integer
  counts_overall <- survey::svytable(survey::make.formula(variable), svy) %>% round()
  propts_overall <- adapt_round(100*prop.table(counts_overall))

  n_groups <- length(counts_overall)

  if(stratified_table){

    counts_by_group <- survey::svytable(
      survey::make.formula(c(variable, ".strat")),
      # survey::make.formula(strata),
      svy
    ) %>% round()

    propts_by_group <- adapt_round(
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
    stop("include_pval for weighted version has not implemented")
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
