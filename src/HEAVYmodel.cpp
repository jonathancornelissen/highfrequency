#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calcRecVarEq(NumericVector par, NumericVector rm) {
  int n = rm.size();
  
  NumericVector g(n);
  g[0] = mean(rm);
  
  for (int i = 1; i < n; i++) {
    g[i] = par[0] + par[1] * rm[i-1] + par[2] * g[i-1];
  }
  
  return g;
}
