

#' @title Predict via Cross Validation
#' 
#' @param object a [gam_matrix] object
#' 
#' @param k \link[base]{integer} scalar
#' 
#' @param mc.cores \link[base]{integer} scalar, see function \link[parallel]{mclapply}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [predict_k_fold.gam_matrix] returns a \link[base]{list}.
#' 
#' @examples
#' data(wrobel_lung, package = 'spatstat.grouped.data')
#' library(spatstat.grouped)
#' lungQp = grouped_ppp(hladr ~ OS | patient_id/image_id, f_sum_ = 'min', data = wrobel_lung) |> 
#'  aggregate_quantile(by = ~ patient_id, probs = seq.int(from = .05, to = .95, by = .01))
#' m = gam_matrix(OS ~ hladr.quantile, data = lungQp, nonlinear = FALSE)
#' 
#' set.seed(145); pred = predict_k_fold.gam_matrix(m, k = 10L)
#' 
#' with(pred, boxplot(est ~ fold))
#' with(pred, boxplot(est.k ~ fold)) 
#' with(pred, boxplot(est.global ~ fold)) 
#' table(pred$signadj)
#' 
#' lungQp. = data.frame(lungQp, pred[c('est', 'est.k', 'est.global')])
#' 
#' library(survival)
#' summary(coxph(OS ~ est, data = lungQp.))
#' summary(coxph(OS ~ est.k, data = lungQp.))
#' summary(coxph(OS ~ est.global, data = lungQp.))
#' 
#' @importFrom caret createFolds
#' @importFrom parallel mclapply detectCores
#' @importFrom stats update
#' @export
predict_k_fold.gam_matrix <- function(
    object, 
    k, 
    mc.cores = switch(.Platform$OS.type, windows = 1L, detectCores()), 
    ...
) { 
  
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
  
  tmp <- mclapply(X = fld, mc.cores = mc.cores, FUN = function(id) {
    d0 <- data[-id, , drop = FALSE] # training set
    d1 <- data[id, , drop = FALSE] # test set
    m <- update(object, data = d0) # training model; 'gam', no longer 'gam_matrix'
    # invokes ?stats::update.default, as of 2023-12-20 packageDate('mgcv')
    attr(m, which = 'xname') <- attr(object, which = 'xname', exact = TRUE) # needed by [cor_xy.gam]
    signadj <- cor_xy.gam(m) |> sign()
    # predicted value on test set
    est <- predict.gam_matrix(m, newdata = d1, sgn = 1)
    est.k <- predict.gam_matrix(m, newdata = d1, sgn = signadj)
    return(list(signadj = signadj, est = est, est.k = est.k))
  })
  
  for (i in seq_along(fld)) {
    id <- fld[[i]]
    ret$fold[id] <- i
    ret$signadj[i] <- tmp[[i]]$signadj
    ret$est[id] <- tmp[[i]]$est
    ret$est.k[id] <- tmp[[i]]$est.k
  }
  
  ret$est.global <- ret$est * sign(cor_xy.gam(object))
  
  return(ret)
  
}



