#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(.colMinCpp)]]
NumericVector colMin(NumericMatrix x) {
    int nc = x.ncol();
    NumericVector colM(nc);
    for(int i = 0; i < nc; i++){
        colM[i] = min(x( _ , i));
    }
    return(colM);
}


// [[Rcpp::export(.colMaxCpp)]]
NumericVector colMax(NumericMatrix x) {
  int nc = x.ncol();
  NumericVector colM(nc);
  for(int i = 0; i < nc; i++){
    colM[i] = max(x( _ , i));
  }
  return(colM);
}


// [[Rcpp::export(.rowMinCpp)]]
NumericVector rowMin(NumericMatrix x) {
  int nr = x.nrow();
  NumericVector colM(nr);
  for(int i = 0; i < nr; i++){
    colM[i] = min(x( i , _ ));
  }
  return(colM);
}



// [[Rcpp::export(.rowMaxCpp)]]
NumericVector rowMax(NumericMatrix x) {
  int nr = x.nrow();
  NumericVector colM(nr);
  for(int i = 0; i < nr; i++){
    colM[i] = max(x( i , _ ));
  }
  return(colM);
}

