
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.betaBootNp)]]
arma::mat betaBoot(arma::colvec& y, const arma::mat& X, const int nboot) {
  int n = X.n_rows, k = X.n_cols;
  arma::mat betaHat(k,nboot);
  for(int i = 0; i < nboot; i++){
    arma::uvec colId = arma::randi<arma::uvec>(n, arma::distr_param(0,n-1));

    arma::mat X_boot = X.rows(colId);
    arma::colvec y_boot = y(colId);
    betaHat.col(i) = arma::solve(X_boot, y_boot);
  }
  return betaHat;
}


#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.betaBootWild)]]

arma::mat betaBootWild(arma::colvec& y, const arma::mat& X, const int nboot) {
  int n = X.n_rows, k = X.n_cols;
  arma::mat betaHat(k,nboot);
  arma::colvec beta_true = arma::solve(X, y);
  arma::colvec y_fit = X*beta_true;
  arma::colvec residuals = y - y_fit;
  Rcpp::IntegerVector v;
  v[0] = -1;
  v[1] = 1;
  for(int i = 0; i < nboot; i++){

    arma::colvec w = Rcpp::as<arma::vec>(Rcpp::sample(v, n, true)) ;
    arma::colvec z = w % residuals;
    arma::colvec y_boot = y_fit + z;
    betaHat.col(i) = arma::solve(X, y_boot);
  }
  return betaHat;
}

