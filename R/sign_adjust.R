

#' @title Sign Adjustment
#' 
#' @param object ..
#' 
#' @param ... parameters of function [cor_xy.gam]
#' 
#' @name sign_adjust
#' @export
sign_adjust <- function(object, ...) UseMethod(generic = 'sign_adjust')


#' @rdname sign_adjust
#' @export sign_adjust.gam_matrix
#' @export
sign_adjust.gam_matrix <- function(object, ...) {
  
  sgn <- object |>
    cor_xy.gam(...) |>
    sign()
  
  return(sgn * object$linear.predictors)
  
  # old practice of 
  #object$linear.predictors <- sgn * object$linear.predictors
  #return(object)
  # is wrong!
  
}



