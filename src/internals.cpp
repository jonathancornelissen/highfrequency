#include <RcppArmadillo.h>
using namespace arma;



//' @keywords internal
// [[Rcpp::export]]
arma::mat colCumsum(arma::mat& x) {
  
  // We loop through the columns of x and apply cumsum
  for(uword col = 0; col < x.n_cols; col++){
    x.col(col) = cumsum(x.col(col));
  }
  
  return(x);
  
}


