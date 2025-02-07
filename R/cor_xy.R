


#' @title \eqn{xy}-Correlation
#' 
#' @param object a [gam_matrix] object
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name cor_xy
#' @export
cor_xy <- function(object, ...) UseMethod(generic = 'cor_xy')



#' @rdname cor_xy
#' 
#' @param probs \link[base]{numeric} scalar or \link[base]{vector} \eqn{\tilde{p}}, 
#' taking values between 0 and 1, see function \link[stats]{quantile}.
#' 
#' @returns
#' Function [cor_xy.gam_matrix] returns a \link[base]{numeric} scalar or \link[base]{vector} of 
#' \link[stats]{cor}relation(s).
#' 
#' @importFrom stats cor quantile
#' @export cor_xy.gam_matrix
#' @export
cor_xy.gam_matrix <- function(object, probs = .5, ...) { # parameter `xfom` removed
  
  if (!inherits(object, what = 'gam_matrix')) stop('input must be `gam_matrix` object')
  
  model <- object$model
  if (!length(model)) stop('should not happen!')
  
  xname <- attr(object, which = 'xname', exact = TRUE)
  # stopifnot(is.symbol(xname))
  
  x <- eval(xname, envir = model)
  if (!is.matrix(x)) stop('`x` information should be matrix')
  id <- quantile(seq_len(dim(x)[2L]), probs = probs, type = 3L)
  
  c(cor(
    x = x[, id, drop = FALSE], # ?stats::cor is very beautifully vectorized!
    y = object$linear.predictors,
    use = 'complete.obs'
  ))
  
}
