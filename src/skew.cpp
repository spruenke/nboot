#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export(.colSkewCpp)]]
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

// [[Rcpp::export(.rowSkewCpp)]]
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


