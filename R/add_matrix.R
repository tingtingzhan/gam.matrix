
#' @title add_matrix
#' 
#' @param call. a *pseudo* \link[mgcv]{gam} \link[base]{call}
#' 
#' @param formula ..
#' 
#' @param nonlinear ..
#' 
#' @param envir ..
#' 
#' @param ... ..
#' 
#' @examples
#' # see ?`gam.matrix-package`
#' 
#' @importFrom mgcv gam s ti
#' @importFrom stats update.formula
#' @export
add_matrix <- function(
    call., 
    formula, 
    nonlinear = FALSE,
    envir = parent.frame(), 
    ...
) {
  
  if (!(deparse1(call.[[1L]]) %in% c('gam', 'mgcv::gam', 'mgcv:::gam'))) stop('start model must be mgcv::gam')
  if (!all(nzchar(names(call.)[-1L]))) stop('all arguments in `call.` must be named')
  
  if (formula[[1L]] != '~' || length(formula) != 2L) stop('`formula` must be one-sided formula')
  xname <- formula[[2L]] # right-hand-side
  if (!is.symbol(xname)) stop('right-hand-side of `formula` must be a symbol')
  
  if (!length(call.$data)) stop('`call.` must contain `data`')
  data_aug <- eval(expr = call.$data, envir = envir) |>
    augdata(xname = xname)
  
  trm_ <- if (nonlinear) as.call(list(
    quote(ti), 
    quote(x), # `x`: augmented column
    xname, 
    by = quote(L), # `L`: augmented column
    bs = 'cr', # cubic regression spline
    mc = c( # see ?mgcv::ti; which marginals should have centering constraints applied
      FALSE, 
      TRUE
    ), 
    ...
  )) else as.call(list(
    quote(s),
    quote(x), # `x`: augmented column
    by = call(name = '*', quote(L), xname), # `L`: augmented column
    bs = 'cr', 
    ...
  ))
  
  if (
    length(call.$family) && 
    # identical(eval(call.$family[[1L]]), cox.ph, ignore.environment = TRUE) &&
    (eval(call.$family)$family == 'Cox PH') && # mgcv::cox.ph() return
    !length(call.$weight)
  ) {
    fom <- call.$formula
    call.$weight <- call(name = '[', fom[[2L]], alist(x = )[[1L]], 2L)
    call.$formula[[2L]] <- call(name = '[', fom[[2L]], alist(x = )[[1L]], 1L)
  }
  
  newfom <- call(name = '~', quote(.), call(name = '+', quote(.), trm_))
  call.$formula <- update.formula(old = call.$formula, new = newfom)
  
  control <- call.$control
  if (!length(control)) {
    control <- list(keepData = TRUE) # to keep augmented data
  } else control$keepData <- TRUE # overwrite existing, if present
  call.$control <- control
  
  call.$data <- quote(data_aug)
  
  # see inside ?stats::update.default
  ret <- eval(call.)
  
  # additional stuff
  attr(ret, which = 'xname') <- xname
  
  if (inherits(ret, what = 'gam')) {
    # instead of 'gam.prefit'
    class(ret) <- c('gam_matrix', class(ret))
  }
  
  return(ret)

}




