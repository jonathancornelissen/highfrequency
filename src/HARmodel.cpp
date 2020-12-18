#include <RcppArmadillo.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
arma::mat har_agg(arma::vec RM, arma::vec periods, int iNperiods){
  int iT = RM.size();
  arma::mat mHARData(iT , iNperiods);
  mHARData.fill(NA_REAL);
  // make return with NA's
  int k = 0;

  for(int i = k; i<iNperiods; i++){
    for(int j = periods(i); j<=iT; j++){
      mHARData((j-1),i ) = sum(RM(arma::span((j - periods(i)) , j-1)))/periods(i);
    }
  }

  return(mHARData);
}
