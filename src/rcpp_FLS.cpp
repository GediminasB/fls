// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat rcpp_FLS(arma::mat X, arma::vec y, double mu, bool smooth = false) {
  int K = X.n_cols;
  int n = X.n_rows;
  arma::mat Q(K, K, arma::fill::zeros);
  arma::mat B(n, K, arma::fill::zeros);
  arma::mat I(K, K, arma::fill::eye);
  arma::mat E(n, K, arma::fill::zeros);
  arma::colvec p = arma::zeros<arma::colvec>(K);
  arma::cube M(K, K, n, arma::fill::zeros);
  //double r = 0;

  for(int i = 0; i < n; i++) {
    arma::vec p_ = p + X.row(i).t() * y(i);
    arma::mat Q_ = Q + X.row(i).t() * X.row(i);

    B.row(i) = p_.t() * arma::inv(Q_);

    M.slice(i) = mu * arma::inv(Q_ + mu*I);
    //Q = M.slice(i) * Q_;
    Q = mu * (I - M.slice(i));
    p = M.slice(i) * p_;
    E.row(i) = 1/mu * p.t();
    //r = r + y(i)*y(i) - (p + X.row(i) * y(i)).t() * arma::inv(Q + X.row(i).t() * X.row(i)) * (p + X.row(i) * y(i));
  }
  if(smooth) {
    for(int i = n-2; i >= 0; i--) {
      B.row(i) = E.row(i) + B.row(i+1) * M.slice(i);
    }
  }
  return(B);
}
