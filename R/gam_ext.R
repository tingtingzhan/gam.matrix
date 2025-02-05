
#' @importFrom mgcv summary.gam
pval.gam <- function(object, ...) {
  summary.gam(object)$s.table[, 'p-value']
}



