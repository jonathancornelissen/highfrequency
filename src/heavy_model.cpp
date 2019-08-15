#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//[[Rcpp::export]]
List heavy_parameter_transformR_(NumericVector parameters, int K, IntegerVector p, IntegerVector q, NumericVector O, NumericVector A, NumericVector B, IntegerVector pMax1, IntegerVector qMax1) {

  int i,j,k;

  for (k = 0; k < K; k++) {
    O[k] = parameters[k];
  }

  int pc,qc;
  int pMax,qMax;
  int offset = K;
  pMax = pMax1[0];
  qMax = qMax1[0];

  for (i=0; i<pMax; i++) {
    for (j=0; j<K*K; j++) {
      pc = p[j];
      if (pc - i > 0) {
        A[j+K*K*(i)] = parameters[offset];
        offset = offset+1;
      }
    }
  }

  for (i=0; i<qMax; i++) {
    for (j=0; j<K*K; j++) {
      qc = q[j];
      if(qc - i > 0) {
        B[j+K*K*(i)] = parameters[offset];
        offset = offset+1;
      }
    }
  }
  
  return List::create(Named("O") = O,
                      Named("A") = A,
                      Named("B") = B);
}

//[[Rcpp::export]]
List heavy_parameter_transform_RetrackR_(NumericVector parameters, int K,  IntegerVector p,  IntegerVector q,  NumericVector means,
                                         NumericVector O, NumericVector A, NumericVector B, IntegerVector pMax1, IntegerVector qMax1) {

  int i,j,k;

  int pc,qc;
  int offset = K;
  int pMax,qMax;

  pMax = pMax1[0];
  qMax = qMax1[0];

  NumericVector suma(K);
  NumericVector sumb(K);

  for (i = 0; i < pMax; i++) {
    for (j = 0; j < K * K; j++) {
      pc = p[j];
      if (pc - i > 0) {
        A[j + K * K * (i)] = parameters[offset];
        offset = offset + 1;
      }
    }
  }

  for (i=0; i < qMax; i++) {
    for (j=0; j < K * K; j++) {
      qc = q[j];
      if (qc - i > 0) {
        B[j + K * K * (i)] = parameters[offset];
        offset = offset + 1;
      }
    }
  }

  for (k = 0; k < K; k++) {
    suma[k] = 0;
    sumb[k] = 0;
    for (i = 0; i < K; i++) {
      for (j = 0; j < pMax; j++) {
        suma[k] = suma[k] + A[K * K * j + K * i + k];
      }
    }

    for (i = 0; i < K; i++) {
      for (j = 0; j < qMax; j++) {
        sumb[k] = sumb[k] + B[K*K*j + K*i + k];
      }
    }
    O[k] = means[k] * (1- suma[k] - sumb[k]);
  }
  return List::create(Named("O") = O,
                      Named("A") = A,
                      Named("B") = B);
}

//[[Rcpp::export]]
List heavy_likelihoodR_(NumericVector h, NumericVector O, NumericVector A, NumericVector B, int TT, int K, int pMax, int qMax, NumericVector data, NumericVector backcast,
                        NumericVector LB, NumericVector UB, NumericVector llRM, NumericVector lls) {
  int t, i, j, k, l;
  double sum, htemp;
  double likConst = K * log(2 * M_PI);
  double ll = 0;
  double lll = 0.0;
  double sum1 = 0;
  double sum2 = 0;
  
  for (t = 0; t < TT; t++) {//for t=1:T

    for (i=0;i<K;i++) {
      h[t*K + i] = O[i];
    }

    for (j = 0; j < pMax; j++) {
      if (t-j > 0) {
        //h(:,t) = h(:,t) + A(:,:,j)*data(:,t-j);
        for (l = 0; l < K; l++) {
          sum = 0.0;
          for(k = 0; k < K; k++) {
            sum = sum + A[K*K*j + K*k + l] * data[(t-j-1) * K + k];
          }
          htemp = h[t*K+l]; // htemp = GETM(model->h,t,l);
          h[t*K+l] = htemp+sum; //SETM(model->h,t,l,htemp + sum);
        }
      } else {
        //h(:,t) = h(:,t) + A(:,:,j)*backCast';
        for (l = 0; l < K; l++) {
          sum = 0.0;
          for (k = 0; k < K; k++) {
            sum = sum + A[K * K * j + K * k + l] * backcast[k];
          }
          h[t*K + l] = h[t * K + l] + sum;
        }
      }
    }

    for (j = 0; j < qMax; j++) {
      if (t-j > 0) {
        for (l = 0; l < K; l++) {
          sum = 0.0;
          for (k = 0; k < K; k++) {
            sum = sum + B[K*K*j + K*k + l]* h[(t-j-1)*K+k];
          }
          h[t*K + l] = h[t*K+l]+sum;
        }
      } else {
        //h(:,t) = h(:,t) + B(:,:,j)*backCast';
        for (l = 0;l < K; l++) {
          sum = 0.0;
          for (k=0;k<K;k++) {
            sum = sum + B[K*K*j + K*k + l] * backcast[k];
          }
          h[t*K + l] = h[t*K+l]+sum;
        }
      }
    }

    for (j = 0; j < K; j++) {
      htemp = h[t * K + j];
      if (htemp < LB[j]) { //h(j,t)<lb(j)
        htemp = LB[j]/(1.0 - (htemp - LB[j]));
        h[t * K + j] = htemp;
      } else {
        if (htemp > UB[j]) {
          h[t * K + j] = UB[j] + log(htemp - UB[j]);
        }
      }
    }
    //lls(t)  = 0.5*(likConst + sum(log(h(:,t))) + sum(data(:,t)./h(:,t)));
    sum1 = 0.0; sum2 = 0.0;
    for (l = 0; l < K; l++) {
      sum1 = sum1 + log(h[t * K + l]);
      sum2 = sum2 + data[t * K + l]/h[t * K + l];

      llRM[l] = llRM[l] - .5 * (log(h[t * K + l]) + data[t * K + l]/h[t * K + l]);
    }

    //SET(model->lls,t, 0.5*(likConst + sum1 + sum2));
    lls[t] = 0.5 * (likConst + sum1 + sum2);
    //printf("%lf\n", GET(model->lls,t));

    lll = lll + lls[t];
  }

  ll = lll;

  if (isnan(ll) || isinf(ll)) {
    ll = 1000000000000.0;
  }
  return List::create(Named("ll") = ll,
                      Named("lls") = lls,
                      Named("h") = h,
                      Named("llRM") = llRM);
}
// 
