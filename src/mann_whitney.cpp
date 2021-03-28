#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


double comp_fun(double x, double y) {
  double a;
  if( x > y ){
    a = 1;
  }
  if( y > x ){
    a = 0;
  }
  if( x == y ){
    a = 0.5;
  }
  return a;
}


#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.uExactBootNp)]]
arma::vec u_exact_boot_np(arma::colvec& x, arma::colvec& y, const int nboot) {
  int n_1 = x.n_elem;
  int n_2 = y.n_elem;
  arma::uvec seq_1 = arma::linspace<arma::uvec>(0, n_1 - 1, n_1);
  arma::uvec seq_2 = arma::linspace<arma::uvec>(n_1, n_2 + n_1 - 1, n_2);
  arma::colvec u(nboot);
  arma::vec tot = arma::join_cols(x, y);
  for(int i = 0; i < nboot; i++){
    arma::vec d_boot = RcppArmadillo::sample(tot, tot.n_elem, true);
    arma::vec x_boot = d_boot(seq_1);
    arma::vec y_boot = d_boot(seq_2);
    double u_temp = 0;
    for(int j = 0; j < n_1; j++){
      for(int k = 0; k < n_2; k++){
        u_temp += comp_fun(x_boot(j), y_boot(k));
      }
    }
    u(i) = u_temp;
  }
  return u;
}


#include <RcppArmadilloExtensions/sample.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export(.uExactBootPerm)]]
arma::vec u_exact_boot_perm(arma::colvec& x, arma::colvec& y, const int nboot) {
  int n_1 = x.n_elem;
  int n_2 = y.n_elem;
  arma::uvec seq_1 = arma::linspace<arma::uvec>(0, n_1 - 1, n_1);
  arma::uvec seq_2 = arma::linspace<arma::uvec>(n_1, n_2 + n_1 - 1, n_2);
  arma::colvec u(nboot);
  arma::vec tot = arma::join_cols(x, y);
  for(int i = 0; i < nboot; i++){
    arma::vec d_boot = RcppArmadillo::sample(tot, tot.n_elem, false);
    arma::vec x_boot = d_boot(seq_1);
    arma::vec y_boot = d_boot(seq_2);
    double u_temp = 0;
    for(int j = 0; j < n_1; j++){
      for(int k = 0; k < n_2; k++){
        u_temp += comp_fun(x_boot(j), y_boot(k));
      }
    }
    u(i) = u_temp;
  }
  return u;
}
