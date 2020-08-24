#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;
// [[Rcpp::plugins(cpp11)]] // So we can use lambda functions



// This needs to be corrected so we actually do s-exp instead of just exp.
arma::rowvec s_exp(const arma::rowvec& x){
  const double u_0 = std::log(1.5);
  arma::rowvec y = arma::zeros<rowvec>(x.n_elem);
  for(arma::uword i = 0; i < y.n_elem; i++){
    if( (x[i] <= u_0) ){
      y[i] = exp(x[i]);
    } else {
      y[i] = exp(u_0)/sqrt(u_0) * sqrt(u_0 - pow(u_0, 2.0) + x[i]);
    }
  }
  
  
  return y;
}




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
List vasicekModel(List model, int nObs, int nSeries, int nDays, const arma::mat& dt){
  
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
List hestonModel(List model, int nObs, int nSeries, int nDays, const arma::mat& dt){
  arma::mat sigma = model["sigma"];
  const arma::rowvec kappa = model["meanReversion"];
  const arma::rowvec theta = sigma.diag().t();
  const arma::rowvec xi = model["volOfVol"];
  const arma::rowvec rho = model["rho"];
  
  // Map the variance covariance matrix into a correlation matrix to use for RNG
  sigma = sqrt(diagmat(sigma.diag())).i() * sigma * sqrt(diagmat(sigma.diag())).i();
  
  arma::mat wt = arma::mvnrnd(arma::zeros(nSeries), sigma, nObs * nDays).t() % sqrt(dt);
  arma::mat bt = arma::randn((nObs * nDays), nSeries) % sqrt(dt);
  // Create the bt matrix as correlated with wt using lambda functions.
  bt = bt.each_row(([&rho](const arma::rowvec& B){return B % sqrt(1.0 - square(rho)); })) + wt.each_row(([&rho](const arma::rowvec& W){ return W % rho;}));
  
  // sigma^2 container
  arma::mat sigma2 = mat(bt);
  sigma2.row(0) = theta;

  for(int t = 1; t < nObs * nDays; t++){
    sigma2.row(t) = sigma2.row(t-1) + kappa % (theta - sigma2.row(t-1)) % dt.row(t-1) + xi % sqrt(sigma2.row(t-1)) % bt.row(t-1);
  }
  
  
  
  arma::mat returns = sqrt(sigma2) % wt;
  
  List out;
  out["sigma2"] = sigma2;
  out["returns"] = returns;
  return out;
  
}



//' Huang and Tauchen 2005 model
//' (we don't include drift here) (sigma_ut is calculated elsewhere too!)
//' sigma_ut * nu_t (rho_1 * dWt_1 + rho_2 * dWt_2 + sqrt(1 - rho_1 ^2 - rho^2) * dWt_3)
//' nu_t^2 = s-exp(beta_0 + beta_1 * nu_t_1^2 + beta_2 * nu_t_2^2)
//' dnu_1_t^2 = alpha_1  * nu_1_t^2 * dt + dWt_1
//' dnu_2_t^2 = alpha_2 * nu_2_t^2 * dt + (1 + phi nu_2_t^2) * dW2_t
//' sigma_ut = C + A*exp(-a*t) + B*exp(-b*(1-t))
//' @keywords internal
// [[Rcpp::export]]
List huangTauchen(List model, int nObs, int nSeries, int nDays, const arma::mat& dt){
  
  // unpack values
  const arma::rowvec alpha1 = model["alpha1"];
  const arma::rowvec alpha2 = model["alpha2"];
  const arma::rowvec beta0 =  model["beta0"];
  const arma::rowvec beta1 = model["beta1"];
  const arma::rowvec beta2 = model["beta2"];
  const arma::rowvec phi = model["phi"];
  const arma::rowvec rho1 = model["rho1"];
  const arma::rowvec rho2 = model["rho2"];
  
  // Initialize containers
  const arma::mat dW1t = arma::randn(nObs * nDays, nSeries) % sqrt(dt);
  const arma::mat dW2t = arma::randn(nObs * nDays, nSeries) % sqrt(dt);
  const arma::mat dW3t = arma::randn(nObs * nDays, nSeries) % sqrt(dt);
  
  arma::mat volFactor1 = arma::zeros<mat>((nObs * nDays), nSeries);
  arma::mat volFactor2 = arma::zeros<mat>((nObs * nDays), nSeries);
  arma::mat nu = arma::zeros<mat>((nObs * nDays), nSeries);
  //volFactor1.row(0) = arma::randn(1, nSeries) % sqrt(-1.0/(2 * alpha1)) + dW1t.row(0);
  
  nu.row(0) = s_exp(beta0 + beta1 % volFactor1.row(0) + beta2 % volFactor2.row(0));
  for(int t = 1; t < nObs * nDays; t++){
    
    volFactor1.row(t) = volFactor1.row(t-1) + alpha1 % volFactor1.row(t-1) % dt.row(t) + dW1t.row(t);
    
    volFactor2.row(t) = volFactor2.row(t-1) + alpha2 % volFactor2.row(t-1) % dt.row(t) + (1.0 + phi % volFactor2.row(t-1)) % dW2t.row(t);
    
    nu.row(t) = s_exp(beta0 + beta1 % volFactor1.row(t) + beta2 % volFactor2.row(t));
    
  }
  
  
  arma::mat returns = sqrt(nu) % (dW1t.each_row([&rho1](const arma::rowvec& w1){return rho1 % w1;}) + dW2t.each_row([&rho2](const arma::rowvec& w2){return rho2 % w2;}) + 
    dW3t.each_row([&rho1, &rho2](const arma::rowvec& w3){ sqrt(1.0 - rho1 - rho2) % w3;}));
  
  List out;
  out["returns"] = returns;
  out["sigma2"] = nu;
  out["volatilityFactor"] = volFactor1;
  out["volatilityFactor2"] = volFactor2;
  return out;
}
