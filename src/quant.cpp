#include <Rcpp.h>
using namespace Rcpp;

//' Computes rowwise quantiles of a numeric matrix
//'
//' @param x Numeric Matrix
//' @param probs Vector of quantiles to compute
//' @return Vector/Matrix of rowwise quantiles
// [[Rcpp::export(.rowQuantCpp)]]
NumericMatrix rowQuant(NumericMatrix a, NumericVector probs){
    int qtL = probs.length();
    int nr = a.nrow();
    int nc = a.ncol();
    NumericMatrix rowQ(nr, qtL);

  for(int i = 0; i < nr; i++){
      NumericVector x = a( i , _ );
    x.sort();
      for(int j = 0; j < qtL; j++){
          double ind = 1 + (nc - 1) * probs[j];
          int lo = floor(ind);
          int hi = ceil(ind);
          double qs = x[lo-1];
          double h = ind - lo;
          rowQ( i, j) = ((1 - h) * qs) + (h * x[hi-1]);
      }


  }
    CharacterVector qNames(probs.begin(), probs.end());
    colnames(rowQ) = qNames;
  return rowQ;
}

//' Computes columnwise quantiles of a numeric matrix
//'
//' @param x Numeric Matrix
//' @param probs Vector of quantiles to compute
//' @return Vector/Matrix of columnwise quantiles
// [[Rcpp::export(.colQuantCpp)]]
NumericMatrix colQuant(NumericMatrix a, NumericVector probs){
    int qtL = probs.length();
    int nr = a.nrow();
    int nc = a.ncol();
    NumericMatrix colQ(nc, qtL);

  for(int i = 0; i < nc; i++){
    NumericVector x = a( _ , i);
    x.sort();
      for(int j = 0; j < qtL; j++){
          double ind = 1 + (nr - 1) * probs[j];
          int lo = floor(ind);
          int hi = ceil(ind);
          double qs = x[lo-1];
          double h = ind - lo;
          colQ( i, j) = ((1 - h) * qs) + (h * x[hi-1]);
      }


  }
    CharacterVector qNames(probs.begin(), probs.end());
    colnames(colQ) = qNames;
  return colQ;
}


