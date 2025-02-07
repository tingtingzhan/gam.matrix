

#' @title Sign Adjustment
#' 
#' @param object ..
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name sign_adjust
#' @export
sign_adjust <- function(object, ...) UseMethod(generic = 'sign_adjust')


#' @rdname sign_adjust
#' @export sign_adjust.gam_matrix
#' @export
sign_adjust.gam_matrix <- function(object, ...) {
  
  sgn <- object |>
    cor_xy.gam_matrix(probs = .5) |>
    sign()
  
  return(sgn * object$linear.predictors)
  
}



