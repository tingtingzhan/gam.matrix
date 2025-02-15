

#' @title \link[mgcv]{gam} with one-and-only-one \link[base]{matrix} Predictor
#' 
#' @description
#' 
#' Function [gam_matrix] ..
#' 
#' Function [cor_xy.gam] ..
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
#' #library(mgcv)
#' library(survival)
#' library(spatstat.grouped)
#' data(Ki67, package = 'spatstat.grouped.data')
#' Ki67 = within.data.frame(Ki67, expr = {
#'  Ki67 = log1p(Ki67)
#'  PFS = Surv(time = recfreesurv_mon, event = recurrence)
#' })
#' #(npt = length(unique(Ki67$patientID))) # 622
#' Ki67q = grouped_ppp(Ki67 ~ PFS + node + Tstage | patientID/tissueID, data = Ki67) |>
#'   aggregate_quantile(by = ~ patientID, probs = seq.int(from = .01, to = .99, by = .01))
#'   
#' #set.seed(234); id = sort.int(sample.int(n = npt, size = 500L))
#' #Ki67q_0 = Ki67q[id, , drop = FALSE] # training set
#' #Ki67q_1 = Ki67q[-id, , drop = FALSE] # test set
#' 
#' m0 = gam_matrix(PFS ~ Ki67.quantile, data = Ki67q)
#' 
#' m0a = quote(gam(formula = PFS[,1L] ~ 1, data = Ki67q, family = cox.ph(), weight = PFS[,2L])) |>
#'  add_matrix(~ Ki67.quantile)
#' #ThomasJeffersonUniv::relaxed_identical(m0, m0a)
#' 
#' m0b = quote(gam(formula = PFS ~ 1, data = Ki67q, family = cox.ph())) |>
#'  add_matrix(~ Ki67.quantile)
#' #ThomasJeffersonUniv::relaxed_identical(m0, m0b)
#' 
#' #fr = sign_adjust(m0)
#' #predict(m0, newdata = Ki67q_1)
#' predict_k_fold.gam_matrix(m0, k = 10L)
#' persp(m0)
#' \donttest{
#' # integrandSurface(gam_matrix(PFS ~ Ki67.quantile, data = Ki67q_0)) # correct
#' integrandSurface(m0) # bug with proj_beta!!!!
#' #integrandSurface(m0, newdata = Ki67q_1)
#' } # to save time
#' 
#' m1 = gam_matrix(PFS ~ Ki67.quantile, data = Ki67q, nonlinear = TRUE)
#' m1b = quote(gam(formula = PFS ~ 1, data = Ki67q, family = cox.ph())) |>
#'  add_matrix(~ Ki67.quantile, nonlinear = TRUE)
#' #which(!mapply(ThomasJeffersonUniv::relaxed_identical, m1, m1b))
#' 
#' predict_k_fold.gam_matrix(m1, k = 10L)
#' \donttest{
#' integrandSurface(m1)
#' #integrandSurface(m1, newdata = Ki67q_1)
#' } # to save time
#' 
#' \donttest{integrandSurface(m0, m1)}
#' 
'_PACKAGE'



