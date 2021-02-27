#include <RcppArmadillo.h>

// This median should be faster than the built-in median in R
// https://stackoverflow.com/questions/34771088/why-is-standard-r-median-function-so-much-slower-than-a-simple-c-alternative
inline double medianCpp(Rcpp::NumericVector &x) {
  std::size_t n = x.size() / 2;
  std::nth_element(x.begin(), x.begin() + n, x.end());
  
  if (x.size() % 2) return x[n]; 
  return (x[n] + *std::max_element(x.begin(), x.begin() + n)) / 2.0;
}


// Function to use instead of median when we have to assign the value, if possible use list(foo = median(foo)) or similar in data.table's [ as they 
// have an incredibly fast median function.
// [[Rcpp::export]]
double medianCase(Rcpp::NumericVector x) {
  switch(x.size()){
  case 1:
    return x[0];
    case 2:
      return sum(x)/2.0;
      default:
        return medianCpp(x);
  }
  
}