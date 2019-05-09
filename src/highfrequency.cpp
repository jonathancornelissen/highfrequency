//Includes/namespaces
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calculate_g(double omega, double alpha, double beta, double gamma, NumericVector returns, double g0) {
  int n = returns.size();
  
  NumericVector g(n);
  g[0] = g0;
  
  for (int i = 1; i < n; i++) {
    
    if (returns[i-1] >= 0) {
      g[i] = omega + alpha * returns[i-1] * returns[i-1] + beta * g[i-1];
    } else {
      g[i] = omega + alpha * returns[i-1] * returns[i-1] + gamma * returns[i-1] * returns[i-1] + beta * g[i-1];
    }
  }
  
  return g;
}


// [[Rcpp::export]]
int nsmaller(NumericVector times, NumericVector lengths, int start, int end, int max) {
  int i = 0;
  while ( (i < (lengths[end] - lengths[start]))) {// && (*(times+ (lengths[start]+i)) <= max)) {
    i++;
  }
  return i;
}

// // [[Rcpp::export]]
// void refreshpoints( int *times, int *lengths, int *ttau, int *dim, int *aa, int *indices, int *lindex){
//   //my first C program, so probably improvable..
//   //length start with all starting points: from zero and upto last endpoint +1
//   // getting variables declared
//   int  t_max=0, i, j,a,b;;
//   int condition = 1;
//   int Ntau[*dim];
//   int tnext[*dim];
//   //int xx tau[*lindex];
//   // getting starting values before looping
//   *ttau = 0;
//   for(i = 0; i<*dim ; i++){
//     if( *ttau < *(times + lengths[i]) )
//     { *ttau = *(times + lengths[i] ); }
//   }
// 
//   for(i = 0; i < *dim; i++){
//     Ntau[i] = nsmaller(times, lengths, i, (i+1), *ttau);
//     *(indices + i * (*lindex))  = Ntau[i];}
//   j=0;
//   // start the loop over all observations
//   while( condition >= 1 ){
// 
//     for(i = 0; i<*dim; i++){
//       tnext[i] = *(times + Ntau[i] + lengths[i]);
//       if( tnext[i] > t_max ){ t_max = tnext[i]; }
//     }
//     j++;
//     *(ttau+j) = t_max;
//     for(i = 0; i<*dim; i++){
//       Ntau[i] = nsmaller(times, lengths, i, (i+1), *(ttau+j));
//       *(indices + i* (*lindex) + j)  = Ntau[i];
//       a = lengths[i+1];
//       b =  (Ntau[i] + lengths[i]);
//       if( a <= b ){ condition = 0; }
//     }
//     t_max = 0;
// 
//   }
//   *aa = j+1;
// }

