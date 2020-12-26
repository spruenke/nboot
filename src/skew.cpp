#include <Rcpp.h>
using namespace Rcpp;

//' Computes sample skewness for each column of a matrix
//'
//' @param x Numeric Matrix
//' @return Vector of sample skewness for all columns
// [[Rcpp::export]]
NumericVector colSkew(NumericMatrix x) {
    int nc = x.ncol();
    int nr = x.nrow();
    NumericVector colS(nc);
    for(int i = 0; i < nc; i++){
        double cMean = mean(x( _ ,i));
        double xSq = 0;
        double cSt = 0;
          for(int j = 0; j < nr; j++){
              xSq += std::pow(x(j,i), 2.0);
              cSt += std::pow(x(j,i) - cMean, 3.0);
          }
          double colMsq = nr * std::pow(cMean, 2.0);
          double cTT = std::sqrt(((xSq - colMsq)) / (nr - 1.0));
          double colNew = nr * std::pow(cTT, 3);
          colS[i] = cSt / colNew;
      }
    return(colS);
}

//' Computes sample skewness for each row of a matrix
//'
//' @param x Numeric Matrix
//' @return Vector of sample skewness for all rows
// [[Rcpp::export]]
NumericVector rowSkew(NumericMatrix x) {
  int nc = x.ncol();
  int nr = x.nrow();
  NumericVector colS(nr);
  for(int i = 0; i < nr; i++){
    double cMean = mean(x( i , _ ));
    double xSq = 0;
    double cSt = 0;
    for(int j = 0; j < nc; j++){
      xSq += std::pow(x(i,j), 2.0);
      cSt += std::pow(x(i,j) - cMean, 3.0);
    }
    double colMsq = nc * std::pow(cMean, 2.0);
    double cTT = std::sqrt((xSq - colMsq) / (nc - 1.0));
    double colNew = nc * std::pow(cTT, 3);
    colS[i] = cSt / colNew;
  }
  return(colS);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//


