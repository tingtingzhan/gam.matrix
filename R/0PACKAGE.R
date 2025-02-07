

#' @title \link[mgcv]{gam} with one-and-only-one \link[base]{matrix} Predictor
#' 
#' @description
#' 
#' Function [gam_matrix] ..
#' 
#' Function [cor_xy.gam_matrix] ..
#' 
#' Function [sign_adjust.gam_matrix] ..
#' 
#' Function [predict.gam_matrix] ..
#' 
#' Functions [persp.gam_matrix] and [contour.gam_matrix] ..
#' 
#' Function [integrandSurface] ..
#' 
#' @examples
#' library(survival)
#' library(spatstat.grouped)
#' data(Ki67, package = 'spatstat.grouped.data')
#' Ki67 = within.data.frame(Ki67, expr = {
#'  Ki67 = log1p(Ki67)
#'  PFS = Surv(time = recfreesurv_mon, event = recurrence); recfreesurv_mon = recurrence = NULL
#' })
#' (npt = length(unique(Ki67$patientID))) # 622
#' Ki67q = grouped_ppp(Ki67 ~ PFS + node + Tstage | patientID/tissueID, data = Ki67) |>
#'   aggregate_quantile(by = ~ patientID, probs = seq.int(from = .01, to = .99, by = .01))
#'   
#' set.seed(234); id = sort.int(sample.int(n = npt, size = 500L))
#' Ki67q_0 = Ki67q[id, , drop = FALSE] # training set
#' Ki67q_1 = Ki67q[-id, , drop = FALSE] # test set
#' 
#' gam0 = gam_matrix(PFS ~ Ki67.quantile, data = Ki67q_0)
#' fr = sign_adjust(gam0)
#' predict(gam0, newdata = Ki67q_1)
#' persp(gam0)
#' \donttest{
#' integrandSurface(gam0)
#' integrandSurface(gam0, newdata = Ki67q_1)
#' } # to save time
#' 
#' nlgam0 = gam_matrix(PFS ~ Ki67.quantile, data = Ki67q_0, nonlinear = TRUE)
#' nlfr = sign_adjust(nlgam0)
#' \donttest{
#' integrandSurface(nlgam0)
#' integrandSurface(nlgam0, newdata = Ki67q_1)
#' } # to save time
#' 
#' \donttest{integrandSurface(gam0, nlgam0)}
#' 
'_PACKAGE'



