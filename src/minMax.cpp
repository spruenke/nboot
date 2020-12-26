#include <Rcpp.h>
using namespace Rcpp;


//' Computes columnwise minimum of a numeric matrix
//'
//' @param x Numeric Matrix
//' @return Vector of minimum value of each column
// [[Rcpp::export]]
NumericVector colMin(NumericMatrix x) {
    int nc = x.ncol();
    NumericVector colM(nc);
    for(int i = 0; i < nc; i++){
        colM[i] = min(x( _ , i));
    }
    return(colM);
}

//' Computes columnwise maximum of a numeric matrix
//'
//' @param x Numeric Matrix
//' @return Vector of maximum value of each column
// [[Rcpp::export]]
NumericVector colMax(NumericMatrix x) {
  int nc = x.ncol();
  NumericVector colM(nc);
  for(int i = 0; i < nc; i++){
    colM[i] = max(x( _ , i));
  }
  return(colM);
}

//' Computes rowwise minimum of a numeric matrix
//'
//' @param x Numeric Matrix
//' @return Vector of minimum value of each row
// [[Rcpp::export]]
NumericVector rowMin(NumericMatrix x) {
  int nr = x.nrow();
  NumericVector colM(nr);
  for(int i = 0; i < nr; i++){
    colM[i] = min(x( i , _ ));
  }
  return(colM);
}


//' Computes rowwise maximum of a numeric matrix
//'
//' @param x Numeric Matrix
//' @return Vector of maximum value of each row
// [[Rcpp::export]]
NumericVector rowMax(NumericMatrix x) {
  int nr = x.nrow();
  NumericVector colM(nr);
  for(int i = 0; i < nr; i++){
    colM[i] = max(x( i , _ ));
  }
  return(colM);
}

