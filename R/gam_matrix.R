

#' @title \link[mgcv]{gam} with \link[base]{matrix} predictor
#' 
#' @param formula \link[stats]{formula}, e.g., `y~X`. 
#' Response \eqn{y} may be \link[base]{double}, \link[base]{logical} and \link[survival]{Surv}.
#' Functional predictor \eqn{X} is a \link[base]{double} \link[base]{matrix}.
#' The \link[base]{colnames} of \eqn{X} must be convertible to \link[base]{numeric} \link[base]{vector},
#' indicating the *common tabulating grid* shared by all subjects.
#' 
#' @param data \link[base]{data.frame}
#' 
#' @param family \link[stats]{family} object, 
#' see function \link[mgcv]{gam}.
#' Default values are
#' \itemize{
#' \item `mgcv::cox.ph()` for \link[survival]{Surv} response \eqn{y};
#' \item `binomial(link = 'logit')` for \link[base]{logical} response \eqn{y};
#' \item `gaussian(link = 'identity')` for \link[base]{double} response \eqn{y}
#' }
#' 
#' @param nonlinear \link[base]{logical} scalar, 
#' whether to use nonlinear or linear functional model.
#' Default `FALSE`
#' 
#' @param fit see function \link[mgcv]{gam}
#' 
#' @param ... additional parameters for functions \link[mgcv]{s} and \link[mgcv]{ti},
#' most importantly `k`
#' 
# @details 
# Function [gam_matrix] calculates the sign-adjusted quantile indices in the following steps.
# \enumerate{
# \item Fit a functional model (via \link[mgcv]{gam}) 
# of response \eqn{y} with functional predictor \eqn{X};
# \item Obtain the \link[base]{sign}-adjustment, see section **Details** of function [integrandSurface];
# }
# 
# *Sign-adjusted quantile indices*
# are the product of 
# `sign` (from Step 2) and `gam(.)$linear.predictors` (from Step 1).
# Multiplication by `sign` ensures
# that the sign-adjusted quantile indices
# are positively correlated with the user-selected \eqn{X_{\cdot,j}}.
#' 
#' 
#' @returns 
#' Function [gam_matrix] returns a [gam_matrix] object, which \link[base]{inherits} from class \link[mgcv]{gam}.
#' 
#' 
#' @examples 
#' # see ?oldPublication.R
#' @importFrom mgcv gam cox.ph s ti
#' @importFrom stats binomial gaussian
#' 
#' @export
gam_matrix <- function(
    formula, data,
    family,
    fit = TRUE,
    nonlinear = FALSE,
    ...
) {

  xname <- formula[[3L]] # right-hand-side
  data <- data_augment_gam_matrix(data = data, xname = xname)
  
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
    bs = 'cr', ...
  ))
  
  y <- eval(formula[[2L]], envir = data)
  
  gam_cl <- call(
    name = 'gam', 
    fit = fit,
    data = quote(data),
    control = list(keepData = TRUE)
  ) 
  
  if (inherits(y, what = 'Surv')) {
    gam_cl$formula <- call(name = '~', call(name = '[', formula[[2L]], alist(x = )[[1L]], 1L), trm_)
    gam_cl$weights <- call(name = '[', formula[[2L]], alist(x = )[[1L]], 2L)
    gam_cl$family <- if (missing(family)) quote(cox.ph()) else substitute(family)
  } else {
    gam_cl$formula <- call(name = '~', formula[[2L]], trm_)
    gam_cl$family <- if (!missing(family)) {
      substitute(family) 
    } else if (is.logical(y) || all(y %in% c(0, 1))) {
      quote(binomial(link = 'logit'))
    } else if (is.numeric(y)) {
      quote(gaussian(link = 'identity'))
    } else stop('not supported yet')
  }
  
  ret <- eval(gam_cl) # 'gam' if `fit = TRUE`; 'gam.prefit' if `fit = FALSE`
  
  attr(ret, which = 'xname') <- xname
  if (fit) class(ret) <- c('gam_matrix', class(ret))
  return(ret)
  
}







#' @importFrom cli col_blue col_magenta
data_augment_gam_matrix <- function(
    data, xname
) {
  
  if (
    all(c('L', 'x') %in% names(data)) &&
    is.matrix(data$L) && is.matrix(data$x)
  ) {
    # `data` already augmented
    return(data)
  }
  
  if (any(c('L', 'x') %in% names(data))) {
    stop('input data cannot contain names `x` and `L`')
  }
  
  if (!is.symbol(xname)) stop('Right-hand-side ', col_magenta(sQuote(deparse1(xname))), ' must be a symbol')
  
  X <- data[[xname]]
  if (!is.matrix(X)) stop(col_blue(sQuote(xname)), ' column in `data` must be evaluated to be a matrix')
  
  dm <- dim(X)
  x. <- as.double(colnames(X))
  if (!length(x.) || !is.numeric(x.) || anyNA(x.) || is.unsorted(x., strictly = TRUE)) {
    stop('matrix column ', col_blue(sQuote(xname)), ' must have colnames convertible to strictly-increasing numerics')
  }
  
  data$x <- tcrossprod(rep(1, times = dm[1L]), x.)
  
  nx <- length(x.)
  # for numeric integration of the functional term - Erjia's comment
  data$L <- array(1/nx, dim = dm)
  
  return(data)
  
}



