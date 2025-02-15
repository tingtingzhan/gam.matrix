

#' @title Predict via Cross Validation
#' 
#' @param object a [gam_matrix] object
#' 
#' @param k \link[base]{integer} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @importFrom caret createFolds
#' @importFrom stats update
#' @export
predict_k_fold.gam_matrix <- function(object, k, ...) { 
  
  if (!inherits(object, what = 'gam_matrix')) stop('input must be `gam_matrix`')
  if (!is.data.frame(data <- object$data)) stop('re-run input gam with `keepData = TRUE`')
  
  # ?caret::createFolds depends on ?base::set.seed
  nr <- .row_names_info(data, type = 2L)
  fld <- createFolds(y = seq_len(nr), k = k, list = TRUE, returnTrain = FALSE)
  
  ret <- list(
    fold = rep(NA_integer_, times = nr),
    est = rep(NA_real_, times = nr),
    est.k = rep(NA_real_, times = nr),
    est.global = rep(NA_real_, times = nr),
    signadj = rep(NA_integer_, times = k)
  )

  for (i in seq_along(fld)) {
    
    id <- fld[[i]]
    #dataout$folds.[id] <- i
    ret$fold[id] <- i
    
    d0 <- data[-id, , drop = FALSE] # training set
    d1 <- data[id, , drop = FALSE] # test set
    
    # training model per fold
    m <- update(object, data = d0) # 'gam', no longer 'gam_matrix'
    # invokes ?stats::update.default, as of 2023-12-20 packageDate('mgcv')
    attr(m, which = 'xname') <- attr(object, which = 'xname', exact = TRUE) # needed by [cor_xy.gam]
    ret$signadj[i] <- cor_xy.gam(m) |> sign()
    
    # predicted value on test set
    ret$est[id] <- predict.gam_matrix(m, newdata = d1, sign_adjusted = FALSE)
    ret$est.k[id] <- predict.gam_matrix(m, newdata = d1, sign_adjusted = TRUE)
    
  }
  
  ret$est.global <- ret$est * sign(cor_xy.gam(object))
  
  return(ret)
  
}



