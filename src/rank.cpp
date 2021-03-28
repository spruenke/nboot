#include <Rcpp.h>
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
class Comparator {
private:
  const Rcpp::NumericVector& ref;

  bool is_na(double x) const
  {
    return Rcpp::traits::is_na<REALSXP>(x);
  }

public:
  Comparator(const Rcpp::NumericVector& ref_)
    : ref(ref_)
  {}

  bool operator()(const int ilhs, const int irhs) const
  {
    double lhs = ref[ilhs], rhs = ref[irhs];
    if (is_na(lhs)) return false;
    if (is_na(rhs)) return true;
    return lhs < rhs;
  }
};

Rcpp::NumericVector avg_rank(Rcpp::NumericVector x)
{
  R_xlen_t sz = x.size();
  Rcpp::IntegerVector w = Rcpp::seq(0, sz - 1);
  std::sort(w.begin(), w.end(), Comparator(x));

  Rcpp::NumericVector r = Rcpp::no_init_vector(sz);
  for (R_xlen_t n, i = 0; i < sz; i += n) {
    n = 1;
    while (i + n < sz && x[w[i]] == x[w[i + n]]) ++n;
    for (R_xlen_t k = 0; k < n; k++) {
      r[w[i + k]] = i + (n + 1) / 2.;
    }
  }

  return r;
}


#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(.ColRankAvg)]]
Rcpp::NumericMatrix rankMatCol(Rcpp::NumericMatrix x){
  Rcpp::NumericMatrix res( x.nrow() , x.ncol() );

  for(int i = 0; i < x.ncol(); i++){
      Rcpp::NumericVector v = x( _ , i);
      res( _ , i) = avg_rank(v);
    }
  return res;
}

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(.RowRankAvg)]]
Rcpp::NumericMatrix rankMatRow(Rcpp::NumericMatrix x){
  Rcpp::NumericMatrix res( x.nrow() , x.ncol() );

  for(int i = 0; i < x.nrow(); i++){
    Rcpp::NumericVector v = x( i , _ );
    res( i , _ ) = avg_rank(v);
  }
  return res;
}
