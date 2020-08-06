#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;



//' @keywords internal
// [[Rcpp::export]]
arma::mat colCumsum(arma::mat& x) {
  
  // We loop through the columns of x and apply cumsum
  for(uword col = 0; col < x.n_cols; col++){
    x.col(col) = cumsum(x.col(col));
  }
  
  return(x);
  
}


//' @keywords internal
// [[Rcpp::export]]
Rcpp::List refreshTimeMathing(const arma::mat& x, arma::vec& idx) {
  
  const uword N = x.n_rows;
  const uword D = x.n_cols;
  
  // Create N by D matrix filled with NA's
  arma::mat tmp(N, D);
  tmp.fill(NA_REAL);
  
  arma::rowvec lastValues = x.row(0);

  // Here, lastValues has no nans and we can put all the observations into the tmp matrix.
  // This a special case for the beginning of the algorithm.
  if(!lastValues.has_nan()){
    tmp.row(0) = lastValues;
    lastValues.fill(NA_REAL);
  }
  
  
  
  for(uword n = 1; n < N; n++){
    
    
    for(uword d = 0; d < D; d++){
    
      if(!std::isnan(x(n,d))){
        lastValues(d) = x(n,d);
      }
      
    }
    
    
    // Check if we have filled the last value vector with values.
    if(lastValues.is_finite()){
      tmp.row(n) = lastValues;
      lastValues.fill(NA_REAL);
    }
    
  }
  
  arma::uvec keep = arma::find_finite(tmp.col(0));
  
  //tmp.rows(keep), 
  tmp = tmp.rows(keep);
  idx = idx.elem(keep);
  
  return Rcpp::List::create(Rcpp::Named("data") = tmp,
                            Rcpp::Named("indices") = idx);
  
}


//' @keywords internal
inline double weightedSumPreAveragingInternal(const arma::vec& x, const arma::vec& series){
  
  return(arma::dot(x,series));
}

//' @keywords internal
// [[Rcpp::export]]
arma::mat preAveragingReturnsInternal(arma::mat& ret, const int kn){
  
  const int N = ret.n_rows + 1;
  const int D = ret.n_cols;
  arma::mat out(N-kn + 1, D);
  
  arma::vec weights = arma::linspace(1,kn-1, kn-1)/kn;
  arma::vec foo = 1-weights;
  
  weights(find(weights > (1-weights))) = foo(find(weights > (1-weights)));



  for(int i = 0; i < N - kn + 1; i++) {
    // Do column wise multiplication of our weights on the returns.
    out.row(i) = sum(ret(span(i,i+kn-2), span(0, D-1)).each_col() % weights , 0);

  }

  
  return(out);
  
  
}


// Armadillo version of np.clip needed for the lead-lag estimation, didn't find a version in base R
// [[Rcpp::export]]
arma::vec clamp(arma::vec& x, const double lower = 0.0, const double upper = 1.0){
  return(arma::clamp(x, lower, upper));
}


// Armadillo version of bisect_left needed for the lead-lag estimation, didn't find a version in base R
// [[Rcpp::export]]
arma::uword findFirst(arma::vec& x , const int thresh){
  arma::uword i;
  for(i = 0; i < x.n_elem; i++){ 
    
    if(x[i] >= thresh){
      return i; 
    }
    
  }
  return i; 
}


// [[Rcpp::export]]
bool overlap(double min1, double max1, double min2, double max2){
  return (std::max(0.0, ((double) std::min(max1, max2) - (double) std::max(min1, min2))) > 0.0);
}


// [[Rcpp::export]]
arma::vec quadraticKernel(arma::vec x){
  return(0.975 * pow(pow(1 - arma::clamp(x, 0.0, 1.0) , 2.0) , 2.0));
}