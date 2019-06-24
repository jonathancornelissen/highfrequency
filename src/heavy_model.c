#include <stdio.h> 
#include <stdlib.h> 
#include <R.h> 
#include "heavy_model.h"
#include "realized_measures.h"
#include <math.h> 
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const
  R_CMethodDef cMethods[] = {
    {"KK",  (DL_FUNC) &KK, 3},
    //{"justKernel", (DL_FUNC) &justKernel, 3},
    //{"kernel", (DL_FUNC) &kernel, 8},
    //{"kernelEstimator", (DL_FUNC) &kernelEstimator, 9},
    //{"kernel2", (DL_FUNC) &kernel2, 10},
    //{"subsample", (DL_FUNC) &subsample, 10},
    //{"rv", (DL_FUNC) &rv, 9},
    //{"rvperiod", (DL_FUNC) &rvperiod, 9},
    //{"rfourth", (DL_FUNC) &rfourth, 9},
    //{"rfourthlead", (DL_FUNC) &rfourthlead, 9},
    //{"covkernel", (DL_FUNC) &covkernel, 7},
    //{"covcc", (DL_FUNC) &covcc, 8},
    //{"pcovcc", (DL_FUNC) &pcovcc, 12},
    //{"sametime", (DL_FUNC) &sametime, 5},
    //{"tocts", (DL_FUNC) &tocts, 7},
    //{"portfolio", (DL_FUNC) &portfolio, 10},
    //{"refreshpoints", (DL_FUNC) &refreshpoints, 7},
    {NULL}
  };

void R_init_highfrequency(DllInfo *info) {
  R_registerRoutines(info,
                     cMethods, NULL,
                     NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

void heavy_likelihoodR_(double *parameters, double *data, int *T1, int *K1, double *means, int *p, int *q, int *pMax1, int *qMax1, 
                       double *backcast, double *LB, double *UB, int *compconst1, double *h, double *lls, double *llRM, double *ll) {
  
  int i,j,t,l,k;
  double sum=0.0;
  double sum1,sum2;
  double lll = 0.0;
  double htemp;
  
  int K,T,pMax,qMax,compconst;
  K = K1[0];
  T = T1[0];
  
  pMax = pMax1[0]; qMax = qMax1[0];
  compconst = compconst1[0];
  
  double likConst = K*log(2*M_PI);
  
  double O[K];
  double A[K*K*pMax];
  double B[K*K*qMax];
  // double temp[K]; 
  
  //---- initialize -----------------
  for(i=0;i<K;i++)
  {
    O[i] = 0.0;
    for(k=0;k<K;k++)
    {
      for(j=0;j<pMax;j++)
      {A[K*K*j + K*i+k] = 0.0;}
      for(j=0;j<qMax;j++)
      {B[K*K*j + K*i+k] = 0.0;}              
    } 
    llRM[i] = 0.0;
  }   
  
  if(compconst == 1) {
    heavy_parameter_transformR_(parameters, K, p, q, &O[0], &A[0], &B[0],pMax1,qMax1);
  } else {
    heavy_parameter_transform_RetrackR_(parameters, K, p, q, means, &O[0], &A[0], &B[0],pMax1,qMax1);
  }
  
  
  for (i = 0; i < K; i++) {
    if (O[i] < 0) {
      O[i] = .000001;
    }
  } //?? O(O<0) = realmin();
  
  for(t = 0; t < T; t++) {//for t=1:T
    
    //for(i=0;i<K;i++) {SETM(model->h,t,i,O[i]);} //h(:,t) = O
    
    for(i=0;i<K;i++) {h[t*K + i] = O[i];}
    
    //------------------------------------------------
    
    for(j=0;j<pMax;j++) {
      if(t-j > 0) {        
        //h(:,t) = h(:,t) + A(:,:,j)*data(:,t-j);
        for(l=0;l<K;l++) {
          sum = 0.0;
          for(k=0;k<K;k++) {
            sum = sum + A[K*K*j + K*k + l]*data[(t-j-1)*K+k];
          } //{sum = sum + A[K*K*j + K*l + k]*GETM(model->data,t-j-1,k);}
          
          htemp = h[t*K+l]; // htemp = GETM(model->h,t,l); 
          h[t*K+l] = htemp+sum; //SETM(model->h,t,l,htemp + sum);
        }
      } else {
        //h(:,t) = h(:,t) + A(:,:,j)*backCast';
        for (l = 0; l < K; l++) {
          sum = 0.0;
          for(k=0;k<K;k++) {
            sum = sum + A[K*K*j + K*k + l]*backcast[k];
          } //{sum = sum + A[K*K*j + K*l + k]*GET(model->backcast,k);}
          //htemp = h[t*K+l]; //htemp = GETM(model->h,t,l); 
          h[t*K + l] = h[t*K+l]+sum; //SETM(model->h,t,l,htemp + sum);
        } 
      }
    }
    
    for(j=0;j<qMax;j++) {
      if(t-j > 0) {        
        //h(:,t) = h(:,t) + B(:,:,j)*h(:,t-j);
        for(l=0;l<K;l++) {
          sum = 0.0;
          for(k=0;k<K;k++)
          {sum = sum + B[K*K*j + K*k + l]* h[(t-j-1)*K+k];} //{sum = sum + B[K*K*j + K*l + k]*GETM(model->h,t-j-1,k);}
          //htemp = GETM(h,l,t); 
          //temp[l] = sum;
          //printf("%lf\n", temp[l]);
          
          //for(l=0;l<K;l++) 
          h[t*K + l] = h[t*K+l]+sum;    
        }
      }
      else
      {
        //h(:,t) = h(:,t) + B(:,:,j)*backCast';
        for(l=0;l<K;l++) 
        {
          sum = 0.0;
          for(k=0;k<K;k++)
          {sum = sum + B[K*K*j + K*k + l]*backcast[k];}
          
          h[t*K + l] = h[t*K+l]+sum;
        } 
      }
    }    
    
    
    // printf("%lf\n", GETM(model->h,t,1));
    
    for(j=0;j<K;j++) //j=1:K
    {
      
      htemp = h[t*K + j];
      
      if(htemp < LB[j])       //h(j,t)<lb(j)
      { 
        //printf("%lf\n", GETM(model->h,t,j)); 
        //h(j,t) = lb(j) * 1./(1-(h(j,t)-lb(j)));
        //htemp = GETM(h,j,t);
        htemp = LB[j]/(1.0 - (htemp - LB[j]));
        h[t*K+j] = htemp;
      }
      else if(htemp > UB[j])
      {
        h[t*K+j] = UB[j] + log(htemp - UB[j]);
      }
    }
    
    //lls(t)  = 0.5*(likConst + sum(log(h(:,t))) + sum(data(:,t)./h(:,t)));
    sum1 = 0.0; sum2 = 0.0;
    for(l=0;l<K;l++)
    {
      sum1 = sum1 + log(h[t*K+l]);
      sum2 = sum2 + data[t*K+l]/h[t*K+l]; 
      
      llRM[l] = llRM[l] - .5*(log(h[t*K+l]) + data[t*K+l]/h[t*K+l]);     
    }
    
    
    
    //SET(model->lls,t, 0.5*(likConst + sum1 + sum2));
    lls[t] = 0.5*(likConst + sum1 + sum2);
    //printf("%lf\n", GET(model->lls,t));
    
    lll = lll + lls[t];
  }
  
  //plotData(model->lls->data, model->n_obs);
  
  //ll = gsl_blas_dasum(model->lls);
  
  *ll = lll;
  
  //printf("likelihood = %lf\n", ll);
  
  if(isnan(*ll) || isinf(*ll)) 
  {*ll = 1000000000000.0;}
  
}

void heavy_parameter_transformR_(double *parameters, int K, int *p, int *q, double *O, double *A, double *B, int *pMax1, int *qMax1) {
  
  int i,j,k;
  
  for(k=0;k<K;k++) {O[k] = parameters[k];} //O(:) = parameters(1:K);
  
  int pc,qc;
  int pMax,qMax;
  int offset = K;
  pMax = pMax1[0];
  qMax = qMax1[0];
  
  ////////
  
  for(i=0; i<pMax; i++)
  {
    for(j=0; j<K*K; j++)
    {
      pc = p[j];
      if(pc - i > 0)
      {A[j+K*K*(i)] = parameters[offset];
        offset = offset+1;}
    }
  }
  /////////
  for(i=0; i<qMax; i++)
  {
    for(j=0; j<K*K; j++)
    {
      qc = q[j];
      if(qc - i > 0)
      {B[j+K*K*(i)] = parameters[offset];
        offset = offset+1;}
    }
  }
}


void heavy_parameter_transform_RetrackR_(double *parameters, int K,  int *p,  int *q,  double* means, double *O, double *A, double *B, int *pMax1, int *qMax1) {
  
  int i,j,k;
  
  int pc,qc;
  int offset = K;
  int pMax,qMax;
  
  pMax = pMax1[0];
  qMax = qMax1[0];
  
  double suma[K];
  double sumb[K];
  
  for(i = 0; i < pMax; i++) {
    for(j=0; j<K*K; j++) {
      pc = p[j];
      if(pc - i > 0)
      {A[j+K*K*(i)] = parameters[offset];
        offset = offset+1;}
    }
  }
  /////////
  for(i=0; i<qMax; i++)
  {
    for(j=0; j<K*K; j++)
    {
      qc = q[j];
      if(qc - i > 0)
      {B[j+K*K*(i)] = parameters[offset];
        offset = offset+1;}
    }
  }
  
  
  for (k=0;k<K;k++) {
    suma[k] = 0;
    sumb[k] = 0;
    for (i=0; i<K; i++)
    {
      for (j=0;j<pMax;j++)
      {
        suma[k] = suma[k] + A[K*K*j + K*i + k];
      }
    }
    
    for (i=0; i<K; i++)
    {
      for (j=0;j<qMax;j++)
      {
        sumb[k] = sumb[k] + B[K*K*j + K*i + k];
      }
    }
    O[k] = means[k] * (1- suma[k] - sumb[k]);
  }
}
