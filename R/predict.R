

#' @title Predicted Sign-Adjusted Quantile Indices
#' 
#' @description
#' To predict sign-adjusted quantile indices of a test set.
#' 
#' @param object an [gam_matrix] object based on the training set.
#' 
#' @param newdata test \link[base]{data.frame}, with at least 
#' the response \eqn{y^{\text{new}}} and
#' the \link[base]{double} \link[base]{matrix} of 
#' functional predictor values \eqn{X^{\text{new}}}
#' of the test set, tabulated on the same \eqn{p}-grid as the training set \eqn{X}.
#' If missing, the training set `object@gam$data` will be used.
#' 
#' @param sign_adjusted \link[base]{logical} scalar
#' 
#' @param sgn ...
#' 
#' @param ... additional parameters, currently not in use.
#' 
#' @details 
#' 
#' Function [predict.gam_matrix] computes 
#' the predicted sign-adjusted quantile indices on the test set, 
#' which is 
#' the product of function \link[mgcv]{predict.gam} return
#' and the correlation sign based on training set
#' (see Step 3 of section **Details** of function [gam_matrix]).
#' Multiplication by this sign is required to ensure
#' that the predicted sign-adjusted quantile indices
#' are positively associated with the **training** functional predictor values
#' at the selected tabulating grid.
#' 
#' 
#' @returns 
#' Function [predict.gam_matrix] returns a 
#' \link[base]{double} \link[base]{vector}, 
#' which is the predicted sign-adjusted quantile indices on the test set.
#' 
#' @importFrom mgcv predict.gam
#' @importFrom stats predict
#' @export predict.gam_matrix
#' @export
predict.gam_matrix <- function(
    object, 
    newdata = object$data,
    sign_adjusted = TRUE,
    sgn = if (sign_adjusted) object |> cor_xy(probs = .5) |> sign() else 1,
    ...
) {
  
  xname <- attr(object, which = 'xname', exact = TRUE)
  newdata <- data_augment_gam_matrix(data = newdata, xname = xname)
  
  # do we really need to check the `$x` and `$L` of `newdata` and `olddata` being the same???
  # from tzh's previous code, we do need to check '$L' are the same
  newL <- newdata$L
  oldL <- object$data$L
  if (length(newl <- unique.default(newL)) != 1L) stop()
  oldl <- unique.default(oldL)
  if (!all.equal.numeric(newl, oldl)) stop()
  # what about `$x` ?

  fv <- predict.gam(object = object, newdata = newdata) |> # mgcv::predict.gam returns 'array'
    as.double() #?base::as.double much faster than ?base::c
  
  return(fv * sgn)
    
}






