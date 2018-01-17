#' Fitting Time-Varying Linear Models via FLS
#'
#' \eqn{fls} is used to fit Time-Varying Linear Regression via Flexible least squares (FLS) as discribed in Kalaba and Tesfatsion (1989).
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
#' @param smooth logical. If TRUE, a smoothed coefficients are provided.
#' @return Returns object of class "fls". An object of class "fls" is a list containing the following components:
#' \describe{
#'   \item{coefficients}{A \eqn{n * K} matrix coefficient estimates.}
#'   \item{fitted.values}{the fitted mean values.}
#'   \item{r_D}{sum of dynamic errors.}
#'   \item{r_M}{sum of squared residual measurement errors.}
#' }
#'
#' @references{
#'   \insertRef{KALABA19891215}{fls}
#' }
#' @export
fls = function(X, y, mu = 1, smooth = TRUE) {
  B = fls.fit(X, y, mu, smooth)
  rownames(B) = rownames(X)
  colnames(B) = colnames(X)
  y.hat = rowSums(X * B)
  r_D = sum(diff(B)^2)
  r_M = sum((y - y.hat)^2)

  structure(
    list(
      X = X,
      y = y,
      mu = mu,
      smooth = smooth,
      coefficients = B,
      fitted.values = y.hat,
      residualss = y - y.hat,
      r_D = r_D,
      r_M = r_M
    ), class = "fls"
  )
}
#' @export
print.fls = function(x, ...) {
  n = nrow(x$coefficients)
  cat("Coefficients:\n")

  if(n <=  10) {
    Coef = round(x$coefficients,3)
  } else {
    Coef = rbind(round(utils::head(x$coefficients, 5),3), rep("...", ncol(x$coefficients)), round(utils::tail(x$coefficients, 5), 3))
  }

  if(is.null(rownames(x$coefficients))) {
    row.names(Coef) = rep("", 11)
  }

  print(Coef, quote = FALSE)
  cat("\n")
  cat("Sum of squared errors:\n")
  print(c(`r_D` = x$r_D, `r_M` = x$r_M))
}

#' @export
coef.fls = function(object, ...) {
  object$coefficients
}
