# please refer to survey's documentation

build_design <- function(strata,
                         fpc,
                         weights){
    # fpc could be NULL
  if(!(length(strata)==length(weights)))
    stop("Length for the three should be the same length")
  if(!is.null(fpc))
    if(length(strata) != length(fpc))
      stop("length of fpc should be the same as length of strata")

  cbind(strata, fpc, weights)
}

build_design <- function(svydesign){
  return(svydesign)
}
