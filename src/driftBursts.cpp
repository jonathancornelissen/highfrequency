#include <RcppArmadillo.h>
#ifdef _OPENMP
#include <omp.h>
// [[Rcpp::plugins(openmp)]]
#endif

using namespace arma;
using namespace Rcpp;
const int iMaxLag = 20;
const double dC = 2.6614;



arma::colvec HACWeightC(int iLag){ 
  // Input will always be non-zero and positive, thus, we can ignore the 0 case and save some time
  arma::colvec vW = linspace(1, iLag,iLag) / iLag;
  int iIdx = floor(iLag * 0.5);
  vW(span(0,iIdx-1)) = 1.0 - 6.0 * square(vW(span(0,iIdx-1))) + 6.0 * pow(vW(span(0,iIdx-1)), 3.0);
  vW(span(iIdx, iLag-1)) = 2.0 * pow(1 - vW(span(iIdx, iLag-1)), 3.0);
  return vW;
}



//[[Rcpp::export]]
double AsymptoticVarianceC(const arma::colvec& vIn, int iLag){  
  int iT = vIn.size();
  if(iT<=iLag){ //We cannot calculate the variance in this case as there are not enough observations.
    return(datum::nan);
  }else{
    arma::colvec vW;
    vec vAc = zeros(iLag);
    if(iLag == 0 || iLag == 1){
      return(sum(vIn % vIn)); //Special cases where the weight will be 0.
    }else{
      vW = HACWeightC(iLag);
      for(int i=0; i<iLag; i++){
        vAc[i] = sum(vIn(span((i+1), iT-1)) % vIn(span(0, (iT-i-2))));
      }
    }
    double dOut = sum(vIn % vIn) + 2.0 * sum(vAc % vW);
    return(dOut);
  }
}




// [[Rcpp::export]]
int AutomaticLagSelectionC(const arma::colvec& vX, double dMu){
  int iN = round(4.0 * pow((dMu * 0.01), 0.16));
  int iT = vX.size();
  if(iT<=iN+1){
    return(iMaxLag);
  }
  
  arma::colvec vE = zeros(iN+1);
  arma::colvec vAc = zeros(iN);
  
  for(int i=0; i<=iN; i++){
    vE[i] = mean(vX(span((i+1), iT-1)) % vX(span(0, (iT-i-2))));
  }
  
  double d0 = - sum(linspace(1, (iN+1), (iN+1)) % vE);
  
  for(int i=1; i<=iN; i++){
    vAc[(i-1)] = sum(linspace(1, (iN-i+1), (iN-i+1)) % vE(span(i, iN)));
  }
  
  double s0 = d0 + 2.0 * -sum(vAc);
  
  
  double sQ = 2.0 * sum(square(linspace(1, iN, iN)) % vAc);
  double dGamma = dC * pow(pow((sQ / s0), 2.0 ), 0.2);
  
  int iOut = round(dGamma * pow(dMu, 0.2));
  
  return(iOut);
  
}



//[[Rcpp::export]]
Rcpp::List DriftBurstLoopC(const arma::colvec& vPreAveraged, const arma::colvec& diffedlogprices, 
                           const arma::colvec& vTime, const arma::colvec& vTesttime, 
                           double iMeanBandwidth, double iVarBandwidth, int iPreAverage, int iAcLag){
  
  // Initialization
  int iT = vTesttime.size();
  int iQ;
  int iIdx;
  int iAutoAcLag;
  arma::colvec vMu = zeros<vec>(iT);
  arma::colvec vSigma = zeros<vec>(iT);
  arma::colvec vX = zeros<vec>(vTime.size());
  arma::colvec vWm;
  arma::colvec vWvar;
  
  const double invMB = 1.0/iMeanBandwidth; // So we can multiply instead of divide which is much faster.
  const double invVB = 1.0/iVarBandwidth; // So we can multiply instead of divide which is much faster.
  
  for(int i = 0; i<iT; i++){
    vX = vTime - vTesttime[i];
    iIdx = sum((vX<=0))-2;
    // Construct the weights for the mu estimator using a left-sided exponential kernel
    //  and construct the mu estimate
    vWm = exp(vX(span(0,iIdx)) * invMB); 
    vMu[i] = sum(vWm % vPreAveraged(span(0,iIdx))); 
    // Construct the weights for the sigma estimator using a left-sided exponential kernel
    vWvar = exp(vX(span(0,iIdx)) * invVB);
    
    if(iAcLag == -1){
      // Perform automatic lag selection if mandated.
      iQ = AutomaticLagSelectionC(diffedlogprices(span(0,iIdx)), sum(vWm));
      iAutoAcLag = std::min(iMaxLag, iQ) + 2 * (iPreAverage-1);
      // Estimate the asymptotic variance
      vSigma[i] = AsymptoticVarianceC((vWvar % vPreAveraged(span(0,iIdx))), iAutoAcLag); 
      
    }else{
      // Estimate the asymptotic variance, not using automatic lag selection.
      vSigma[i] = AsymptoticVarianceC((vWvar(span(0,iIdx)) % vPreAveraged(span(0,iIdx))), iAcLag);
      
    }
    
    
  }
  // Calculate the test statistic and create a nice named list containing the estimates, which we will return.
  arma::colvec vDB = sqrt(iMeanBandwidth) * (vMu * invMB) / sqrt(vSigma * invVB) ; 
  Rcpp::List lOut = Rcpp::List::create(Rcpp::Named("tStat") = vDB,
                                       Rcpp::Named("sigma") = vSigma * invVB,
                                       Rcpp::Named("mu") = vMu * invMB);
  return(lOut);
}




//[[Rcpp::export]]
Rcpp::List DriftBurstLoopCPAR(const arma::colvec& vPreAveraged, const arma::colvec& diffedlogprices, 
                              const arma::colvec& vTime, const arma::colvec& vTesttime, 
                              double iMeanBandwidth, double iVarBandwidth, int iPreAverage, int iAcLag, int iCores){
#ifdef _OPENMP
  omp_set_num_threads(iCores);
  int iT = vTesttime.size();
  int iIdx, iAutoAcLag;
  int iQ;
  arma::colvec vMu= zeros<vec>(iT);
  arma::colvec vSigma= zeros<vec>(iT);
  arma::colvec vX = zeros<vec>(vTime.size());
  arma::colvec vWm;
  arma::colvec vWvar;
  //Parallelization setup
  const double invMB = 1.0/iMeanBandwidth; // So we can multiply instead of divide which is much faster.
  const double invVB = 1.0/iVarBandwidth; // So we can multiply instead of divide which is much faster.
#pragma omp parallel for                                                               \
  shared(vPreAveraged, diffedlogprices, vTime, vTesttime, iMeanBandwidth,              \
         iVarBandwidth, iPreAverage, iAcLag, vMu, vSigma, iT, invMB, invVB)            \
    private(vX, vWm, vWvar, iIdx, iQ, iAutoAcLag)
    for(int i = 0; i<iT; i++){
      
      vX = vTime - vTesttime[i];
      
      iIdx = sum((vX <= 0)) - 2;
      
      vWm = exp(vX(span(0, iIdx)) * invMB);  // exponential kernel
      
      
      vMu[i] = sum(vWm(span(0,iIdx)) % vPreAveraged(span(0, iIdx)));
      
      vWvar = exp(vX(span(0,iIdx)) * invVB);  // exponential kernel
      
      if(iAcLag == -1){
        
        iQ = AutomaticLagSelectionC(diffedlogprices(span(0,iIdx)), sum(vWm));
        iAutoAcLag = std::min(iMaxLag, iQ) + 2 * (iPreAverage-1);
        vSigma[i]= AsymptoticVarianceC((vWvar(span(0,iIdx)) % vPreAveraged(span(0,iIdx))), iAutoAcLag); 
      }else{
        vSigma[i] = AsymptoticVarianceC((vWvar(span(0,iIdx)) % vPreAveraged(span(0,iIdx))), iAcLag);
      }
      
    }
    
    arma::colvec vDB = sqrt(iMeanBandwidth) * (vMu * invMB) / sqrt(vSigma * invVB) ; 
  Rcpp::List lOut =  Rcpp::List::create(Rcpp::Named("tStat") = vDB,
                                        Rcpp::Named("sigma") = vSigma * invVB,
                                        Rcpp::Named("mu") = vMu * invMB);
#else
  // If openMP is not available, we use single core execution.
  Rf_warning("OpenMP is not available. Sequential processing is used.");
  Rcpp::List lOut = DriftBurstLoopC(vPreAveraged, diffedlogprices, vTime ,\
                                    vTesttime, iMeanBandwidth, iVarBandwidth, iPreAverage, iAcLag);
#endif
  
  return(lOut);
  
  
}

