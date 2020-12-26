// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// colKurt
NumericVector colKurt(NumericMatrix x);
RcppExport SEXP _nboot_colKurt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colKurt(x));
    return rcpp_result_gen;
END_RCPP
}
// rowKurt
NumericVector rowKurt(NumericMatrix x);
RcppExport SEXP _nboot_rowKurt(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowKurt(x));
    return rcpp_result_gen;
END_RCPP
}
// colMin
NumericVector colMin(NumericMatrix x);
RcppExport SEXP _nboot_colMin(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colMin(x));
    return rcpp_result_gen;
END_RCPP
}
// colMax
NumericVector colMax(NumericMatrix x);
RcppExport SEXP _nboot_colMax(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colMax(x));
    return rcpp_result_gen;
END_RCPP
}
// rowMin
NumericVector rowMin(NumericMatrix x);
RcppExport SEXP _nboot_rowMin(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMin(x));
    return rcpp_result_gen;
END_RCPP
}
// rowMax
NumericVector rowMax(NumericMatrix x);
RcppExport SEXP _nboot_rowMax(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowMax(x));
    return rcpp_result_gen;
END_RCPP
}
// rowQuant
NumericMatrix rowQuant(NumericMatrix a, NumericVector probs);
RcppExport SEXP _nboot_rowQuant(SEXP aSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(rowQuant(a, probs));
    return rcpp_result_gen;
END_RCPP
}
// colQuant
NumericMatrix colQuant(NumericMatrix a, NumericVector probs);
RcppExport SEXP _nboot_colQuant(SEXP aSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type a(aSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    rcpp_result_gen = Rcpp::wrap(colQuant(a, probs));
    return rcpp_result_gen;
END_RCPP
}
// colSkew
NumericVector colSkew(NumericMatrix x);
RcppExport SEXP _nboot_colSkew(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(colSkew(x));
    return rcpp_result_gen;
END_RCPP
}
// rowSkew
NumericVector rowSkew(NumericMatrix x);
RcppExport SEXP _nboot_rowSkew(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rowSkew(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_nboot_colKurt", (DL_FUNC) &_nboot_colKurt, 1},
    {"_nboot_rowKurt", (DL_FUNC) &_nboot_rowKurt, 1},
    {"_nboot_colMin", (DL_FUNC) &_nboot_colMin, 1},
    {"_nboot_colMax", (DL_FUNC) &_nboot_colMax, 1},
    {"_nboot_rowMin", (DL_FUNC) &_nboot_rowMin, 1},
    {"_nboot_rowMax", (DL_FUNC) &_nboot_rowMax, 1},
    {"_nboot_rowQuant", (DL_FUNC) &_nboot_rowQuant, 2},
    {"_nboot_colQuant", (DL_FUNC) &_nboot_colQuant, 2},
    {"_nboot_colSkew", (DL_FUNC) &_nboot_colSkew, 1},
    {"_nboot_rowSkew", (DL_FUNC) &_nboot_rowSkew, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_nboot(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
