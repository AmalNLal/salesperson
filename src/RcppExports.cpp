// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// getAnglesToNearestNeighborsCPP
NumericVector getAnglesToNearestNeighborsCPP(NumericMatrix coords, NumericMatrix dist_mat);
RcppExport SEXP _salesperson_getAnglesToNearestNeighborsCPP(SEXP coordsSEXP, SEXP dist_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type dist_mat(dist_matSEXP);
    rcpp_result_gen = Rcpp::wrap(getAnglesToNearestNeighborsCPP(coords, dist_mat));
    return rcpp_result_gen;
END_RCPP
}
// getCentroidCoordinatesCPP
NumericVector getCentroidCoordinatesCPP(const NumericMatrix coords);
RcppExport SEXP _salesperson_getCentroidCoordinatesCPP(SEXP coordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type coords(coordsSEXP);
    rcpp_result_gen = Rcpp::wrap(getCentroidCoordinatesCPP(coords));
    return rcpp_result_gen;
END_RCPP
}
// getDistancesToCentroidCPP
NumericVector getDistancesToCentroidCPP(const NumericMatrix coords, NumericVector centroid_coords);
RcppExport SEXP _salesperson_getDistancesToCentroidCPP(SEXP coordsSEXP, SEXP centroid_coordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type centroid_coords(centroid_coordsSEXP);
    rcpp_result_gen = Rcpp::wrap(getDistancesToCentroidCPP(coords, centroid_coords));
    return rcpp_result_gen;
END_RCPP
}
// getDistanceFeatureSetCPP
List getDistanceFeatureSetCPP(const NumericMatrix d, const NumericVector dd);
RcppExport SEXP _salesperson_getDistanceFeatureSetCPP(SEXP dSEXP, SEXP ddSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const NumericMatrix >::type d(dSEXP);
    Rcpp::traits::input_parameter< const NumericVector >::type dd(ddSEXP);
    rcpp_result_gen = Rcpp::wrap(getDistanceFeatureSetCPP(d, dd));
    return rcpp_result_gen;
END_RCPP
}
// getFractionOfPointsNearBoundingBoxCPP
List getFractionOfPointsNearBoundingBoxCPP(NumericMatrix coords, double distanceFraction, bool normalize);
RcppExport SEXP _salesperson_getFractionOfPointsNearBoundingBoxCPP(SEXP coordsSEXP, SEXP distanceFractionSEXP, SEXP normalizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type coords(coordsSEXP);
    Rcpp::traits::input_parameter< double >::type distanceFraction(distanceFractionSEXP);
    Rcpp::traits::input_parameter< bool >::type normalize(normalizeSEXP);
    rcpp_result_gen = Rcpp::wrap(getFractionOfPointsNearBoundingBoxCPP(coords, distanceFraction, normalize));
    return rcpp_result_gen;
END_RCPP
}
// getNearestNeighbourDistancesCPP
NumericVector getNearestNeighbourDistancesCPP(NumericMatrix dist_mat);
RcppExport SEXP _salesperson_getNearestNeighbourDistancesCPP(SEXP dist_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type dist_mat(dist_matSEXP);
    rcpp_result_gen = Rcpp::wrap(getNearestNeighbourDistancesCPP(dist_mat));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP getMonitoringFeatureSetC(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_salesperson_getAnglesToNearestNeighborsCPP", (DL_FUNC) &_salesperson_getAnglesToNearestNeighborsCPP, 2},
    {"_salesperson_getCentroidCoordinatesCPP", (DL_FUNC) &_salesperson_getCentroidCoordinatesCPP, 1},
    {"_salesperson_getDistancesToCentroidCPP", (DL_FUNC) &_salesperson_getDistancesToCentroidCPP, 2},
    {"_salesperson_getDistanceFeatureSetCPP", (DL_FUNC) &_salesperson_getDistanceFeatureSetCPP, 2},
    {"_salesperson_getFractionOfPointsNearBoundingBoxCPP", (DL_FUNC) &_salesperson_getFractionOfPointsNearBoundingBoxCPP, 3},
    {"_salesperson_getNearestNeighbourDistancesCPP", (DL_FUNC) &_salesperson_getNearestNeighbourDistancesCPP, 1},
    {"getMonitoringFeatureSetC", (DL_FUNC) &getMonitoringFeatureSetC, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_salesperson(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
