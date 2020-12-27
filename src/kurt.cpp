#include <Rcpp.h>
using namespace Rcpp;


//' Computes sample excess kurtosis for each column of a matrix
//'
//' @param x Numeric Matrix
//' @return Vector of sample excess kurtosis for all columns
// [[Rcpp::export(.colKurtCpp)]]
NumericVector colKurt(NumericMatrix x) {
  int nc = x.ncol();
  int nr = x.nrow();
  NumericVector colK(nc);
  for(int i = 0; i < nc; i++){
      double cMean = mean(x( _ , i));
      double cK = 0;
      double colVar = 0;
      for(int j = 0; j < nr; j++){
          cK += std::pow( (x(j, i) - cMean), 4.0) / nr;
          colVar += std::pow( (x(j, i) - cMean), 2.0) / nr;
      }
      colK[i] = (cK / (std::pow(colVar, 2.0))) - 3.0;
  }
  return(colK);
}


//' Computes sample excess kurtosis for each row of a matrix
//'
//' @param x Numeric Matrix
//' @return Vector of sample excess kurtosis for all rows
// [[Rcpp::export(.rowKurtCpp)]]
NumericVector rowKurt(NumericMatrix x) {
  int nc = x.ncol();
  int nr = x.nrow();
  NumericVector colK(nr);
  for(int i = 0; i < nr; i++){
    double cMean = mean(x( i , _ ));
    double cK = 0;
    double colVar = 0;
    for(int j = 0; j < nc; j++){
      cK += std::pow( (x(i, j) - cMean), 4.0) / nc;
      colVar += std::pow( (x(i, j) - cMean), 2.0) / nc;
    }
    colK[i] = (cK / (std::pow(colVar, 2.0))) - 3.0;
  }
  return(colK);
}
