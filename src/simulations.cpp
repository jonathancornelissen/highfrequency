#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]] // So we can use lambdafunctions
inline arma::rowvec vecpow(arma::rowvec x, const arma::rowvec power){
  assert (x.n_elem == power.n_elem);
  
  for(arma::uword i = 0; i < x.n_elem; i++){
    if(power[i] == 0.0){
      x[i] = 1.0;
    } else {
      x[i] = pow(x[i], power[i]);
    }
  }
  
  return(x);
  
}


// [[Rcpp::export]]
List vasicekModel(List model, int nObs, int nSeries, int nDays, arma::mat dt){
  
  arma::rowvec a = model["meanReversion"];
  arma::rowvec b = model["drift"];
  arma::rowvec sigma = model["driftVol"];
  
  arma::mat drift = arma::zeros<mat>((nObs * nDays), nSeries);
  
  drift.row(0) = b; //+ 1e-8; // So we don't have problems when drift = 0;
  
  for(int t = 1; t < nObs * nDays; t++){
    drift.row(t) = drift.row(t-1) + a % (b - drift.row(t-1)) % dt.row(t-1) + sigma % randn(1, nSeries) % sqrt(dt.row(t-1));
  }

  List out;
  out["drift"] = drift;
  return out;
  
  
}



//' Heston model: 
//' dXt = mu_t * dt + sigma_t * dW_t 
//' dSigma_t^2 = kappa * (theta - sigma_t^2) * dt + xi * sigma_t *dB_t
//' Where Wt and Bt are (often negatively) correlated brownian motions with correlation rho
//' We will not simulate mu_t here, that is done in R code elsewhere.
//' kappa is the mean reversion factor.
//' theta is the long term mean of volatility
//' xi is the vol of vol parameter.
//' @keywords internal
// [[Rcpp::export]]
List hestonModel(List model, int nObs, int nSeries, int nDays, arma::mat dt){
  arma::mat sigma = model["sigma"];
  const arma::rowvec kappa = model["meanReversion"];
  const arma::rowvec theta = sigma.diag().t();
  const arma::rowvec xi = model["volOfVol"];
  const arma::rowvec rho = model["rho"];
  
  //sigma /= prod(sqrt(sigma.diag()));
  sigma = sqrt(diagmat(sigma.diag())).i() * sigma * sqrt(diagmat(sigma.diag())).i();
  
  
  //sigma = sigma * (1/as_scalar(sigma.min()));
  // arma::mat wt = arma::randn((nObs * nDays), nSeries) % sqrt(dt);
  arma::mat wt = arma::mvnrnd(arma::zeros(nSeries), sigma, nObs * nDays).t() % sqrt(dt);
  arma::mat bt = arma::randn((nObs * nDays), nSeries) % sqrt(dt);
  bt = bt.each_row(([&rho](const arma::rowvec& B){return B % sqrt(1.0 - square(rho)); })) + wt.each_row(([&rho](const arma::rowvec& W){ return W % rho;}));
  
  // sigma^2 container
  arma::mat sigma2 = mat(bt);
  sigma2.row(0) = theta;

  for(int t = 1; t < nObs * nDays; t++){
    sigma2.row(t) = sigma2.row(t-1) + kappa % (theta - sigma2.row(t-1)) % dt.row(t-1) + xi % sqrt(sigma2.row(t-1)) % bt.row(t-1);
  }
  
  
  
  // Convert sigma2 to sigma (keeping same name for memory purposes!!!!!)
  sigma2 = sqrt(sigma2);
  arma::mat returns = sigma2 % wt;
  
  List out;
  out["underlyingVolatility"] = sigma2;
  out["returns"] = returns;
  return out;
  
}

//List 