#' Fitting Time-Varying Linear Models via FLS
#'
#' \eqn{fls} is used to fit Time-Varying Linear Regression via Flexible least squares (FLS) as discribed in R. Kalaba and L. Tesfatsion (1989).
#'
#' @docType package
#' @importFrom Rcpp evalCpp
#' @importFrom Rdpack reprompt
#' @useDynLib fls, .registration = TRUE
#' @name fls
#'
#' @param X design matrix of dimensuin \eqn{n * K}.
#' @param y vector of observations of length \eqn{n}.
#' @param mu parameter controling relative weight of sum of dynamic errors (\eqn{r_D^2}) vs sums of squared residual measurement errors (\eqn{r_M^2}).
#' @param smooth logical. If TRUE, a smoothed estimate is provided.
#' @return A \eqn{n * K} matrix coefficient estimates.
#'
#' @references{
#'   \insertRef{KALABA19891215}{fls}
#' }
#' @export
fls = function(X, y, mu = 1, smooth = TRUE) {

}

