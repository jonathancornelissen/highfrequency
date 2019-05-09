#include <stdio.h> 
#include <stdlib.h> 
#include <R.h> 
#include "highfrequency.h" 
#include <math.h> 
#include <R_ext/Rdynload.h>

static const
  R_CMethodDef cMethods[] = {
    {"KK",  (DL_FUNC) &KK, 3},
    {"justKernel", (DL_FUNC) &justKernel, 3},
    {"kernel", (DL_FUNC) &kernel, 8},
    {"kernelEstimator", (DL_FUNC) &kernelEstimator, 9},
    {"kernel2", (DL_FUNC) &kernel2, 10},
    {"subsample", (DL_FUNC) &subsample, 10},
    {"rv", (DL_FUNC) &rv, 9},
    {"rvperiod", (DL_FUNC) &rvperiod, 9},
    {"rfourth", (DL_FUNC) &rfourth, 9},
    {"rfourthlead", (DL_FUNC) &rfourthlead, 9},
    {"covkernel", (DL_FUNC) &covkernel, 7},
    {"covcc", (DL_FUNC) &covcc, 8},
    {"pcovcc", (DL_FUNC) &pcovcc, 12},
    {"sametime", (DL_FUNC) &sametime, 5},
    {"tocts", (DL_FUNC) &tocts, 7},
    {"portfolio", (DL_FUNC) &portfolio, 10},
    {"refreshpoints", (DL_FUNC) &refreshpoints, 7},
    {NULL}
  };

void R_init_highfrequency(DllInfo *info)
{
  R_registerRoutines(info,
                     cMethods, NULL,
                     NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}



double KK(double x, int type)
{
//    double thex = x[0];
    double thex = x;
  switch(type)
  {
      case 0:  // Rectangular
         return(1);
         break;

	  case 1: // Bartlett
	    return(1-thex);
	    break;

	  case 2: //2nd Order
	    return(1-2*thex+thex*thex);
	    break;

	  case 3: //Epanechnikov
	    return(1-thex*thex);
	    break;

	  case 4: //Cubic
	    return(1-3*thex*thex+2*thex*thex*thex);
	    break;

	  case 5: //5th Order
	    return(1-10*thex*thex*thex + 15*thex*thex*thex*thex - 6*thex*thex*thex*thex*thex);
	    break;

	  case 6: //6th Order
	    return(1 - 15*thex*thex*thex*thex + 24*thex*thex*thex*thex*thex - 10*thex*thex*thex*thex*thex*thex);
	    break;

	  case 7: //7th Order
	    return(1 - 21*thex*thex*thex*thex*thex + 35*thex*thex*thex*thex*thex*thex - 15*thex*thex*thex*thex*thex*thex*thex);
	    break;

	  case 8: //8th Order
	    return(1 - 28*thex*thex*thex*thex*thex*thex + 48*thex*thex*thex*thex*thex*thex*thex - 21*thex*thex*thex*thex*thex*thex*thex*thex);
	    break;

	  case 9: //Parzen
	    if(thex > .5)
	    {
	        return( 2*(1-thex)*(1-thex)*(1-thex));
	    }
	    else
	    {
	    	return(1- 6*thex*thex +6*thex*thex*thex);
	    }
	    break;

	  case 10: //Tukey-Hanning
	    return((1 + sin(M_PI_2 - M_PI * thex))/2);
	    break;

	  case 11: //Modified Tukey-Hanning
	    return((1 - sin(M_PI_2 - M_PI *(1 - thex) *(1 - thex)) )/2);
	    break;
	}
	return(-999);
}

void justKernel(double *x, int *type, double *ans)
{
    ans[0] = KK(x[0], type[0]);

}

void kernel(double *a, int *na, int *q, int *adj, int *nw, double *ab, double *ans)
{
     	int i, j,lags = *q, nab = *na - 1;
        double theadj, nwadj;

        if(*nw == 1)
                lags = 2*lags;

        for(j = 0; j <= lags; j++)
		{

        	for(i = 0; i <= nab-j; i++)
			     ab[j] += a[i] * a[i+j];
		}
        for(i = 0; i <= lags; i ++)
        {

              nwadj = 1;
              if(i > *q && *nw == 1)
              {

                  nwadj = ((double)*q - ((double)i - (double)*q))/((double)*q);
              }

              if(*adj == 0)
	     	 {
                  theadj = 1;
              }
              else
              {
                  theadj =  ((double)(nab+1)/((double)(nab+1)-(double)i));
	          }
              if(i == 0)
	      	  {
              		ans[0] += theadj * ab[i];
              }
              else
              {
	                ans[0] += 2 * nwadj *  theadj * ab[i];
              }
        }
}

void kernelEstimator(double *a, double *b, int *na, int *q, int *adj, int *type, double *ab,  double *ab2, double *ans)
{
     	int i, j,lags = *q, nab = *na - 1;
        double theadj, w;


        for(j = 0; j <= lags; j++)
		{

        	for(i = 0; i <= nab-j; i++)
			     ab[j] += a[i] * b[i+j];
        	for(i = j; i <= nab; i++)
			     ab2[j] += a[i] * b[i-j];

		}
        for(i = 0; i <= lags; i ++)
        {
             if(i == 0)
             {
                 w = 1;
             }
             else
             {
             	w = KK( ((double)i-1.0)/(double)lags, type[0]);
             }


             if(*adj == 0)
	     	 {
                  theadj = 1;
             }
             else
             {
                  theadj =  ((double)(nab+1)/((double)(nab+1)-(double)i));
	         }

              if(i == 0)
	      	  {
              		ans[0] += w * theadj * ab[i];
              }
              else
              {
	                ans[0] += w* ( theadj * ab[i] + theadj * ab2[i]);
              }
        }
}

void kernel2(double *a, double *b, int *na, int *q, int *adj, int *nw, double *ab,  double *ab2, double *ans)
{
     	int i, j,lags = *q, nab = *na - 1;
        double theadj, nwadj;

        if(*nw == 1)
                lags = 2*lags;

        for(j = 0; j <= lags; j++)
		{

        	for(i = 0; i <= nab-j; i++)
			     ab[j] += a[i] * b[i+j];
        	for(i = j; i <= nab; i++)
			     ab2[j] += a[i] * b[i-j];

		}
        for(i = 0; i <= lags; i ++)
        {

              nwadj = 1;
              if(i > *q && *nw == 1)
              {

                  nwadj = ((double)*q - ((double)i - (double)*q))/((double)*q);
              }

              if(*adj == 0)
	     	 {
                  theadj = 1;
              }
              else
              {
                  theadj =  ((double)(nab+1)/((double)(nab+1)-(double)i));
	          }
              if(i == 0)
	      	  {
              		ans[0] += theadj * ab[i];
              }
              else
              {
	                ans[0] += nwadj *  theadj * ab[i];
	                ans[0] += nwadj *  theadj * ab2[i];
              }
        }
}



void subsample(double *a, double *b, int *na, int *m, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans)
{
    int i,j,k;
    double starta, startb;

    for(i = 0; i <= *period -1 ; i++)
    {
        starta = 0.0;
        startb = 0.0;
        for(j = 0; j < *tmpna; j++)
        {
             tmpa[j] = 0;
             tmpb[j] = 0;
        }
        for(j = 0; j <= *na-i-1; j++)
        {
        	tmpa[j/ *period] += a[j + i];
        	tmpb[j/ *period] += b[j + i];
        }
        for(j = 0; j < i; j++)
        {
            starta += a[j];
            startb += b[j];
        }
        for(k = 0; k < *tmpna; k++)
        {
            ans[i] += tmpa[k]*tmpb[k];
        }
        ans[i] += starta*startb;
	}
}

void rv(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans)
{
    int j,k;

        for(j = 0; j <= *na-1; j++)
        {
        	tmpa[j/ *period] += a[j];
        	tmpb[j/ *period] += b[j];
        }
        for(k = 0; k < *tmpna; k++)
        {
            ans[0] += tmpa[k]*tmpb[k];
        }
}

void rvperiod(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans)
{
    int j,k;

        for(j = 0; j <= *na-1; j++)
        {
        	tmpa[j/ *period] = a[j];
        	tmpb[j/ *period] = b[j];
        }
        for(k = 0; k < *tmpna; k++)
        {
            ans[0] += tmpa[k]*tmpb[k];
        }
}


void rfourth(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans)
{
    int j,k;

        for(j = 0; j <= *na-1; j++)
        {
        	tmpa[j/ *period] += a[j];
        	tmpb[j/ *period] += b[j];
        }
        for(k = 0; k < *tmpna; k++)
        {
            ans[0] += tmpa[k]*tmpb[k]*tmpa[k]*tmpb[k];
        }
}

void rfourthlead(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans)
{
    int j,k;

        for(j = 0; j <= *na-1; j++)
        {
        	tmpa[j/ *period] += a[j];
        	tmpb[j/ *period] += b[j];
        }
        for(k = 0; k < *tmpna-1; k++)
        {
            ans[0] += tmpa[k]*tmpb[k]*tmpa[k+1]*tmpb[k+1];
        }
}

void covkernel(double *a, double *b, int *na, int *q, int *adj, double *ans)
{
     	int i, j,lags = *q, nab = *na - 1;
        double theadj;

		for(i = lags; i <= nab-lags; i++)
        {
		    for(j =0; j<=lags; j++)
		       ans[0] += a[i] * b[i-j];
        }
        if(*adj == 0)
	    {
            theadj = 1;
        }
        else
        {
             theadj =  ((double)(nab+1)/((double)(nab+1)-(double)(2*lags)));
	    }
        ans[0] = ans[0] * theadj;

}

void covcc(double *a, double *b, double *at, double *bt, int *na, int *nb, double *ans)
{
        int i, prevj = 0, j, nbi=*nb, nai = *na;
        double tmpRet=0;

        for(i = 0; i < nai; i++)
        {
            tmpRet=0;
            for(j=prevj; j < nbi; j++)
            {
                tmpRet += b[j];
                if(bt[j] > at[i])
                {
                    prevj = j;
                    j = nbi;
                }
                else if(bt[j] == at[i])
                {
                    prevj = j+1;
                    j = nbi;
                }
            }
            ans[i] = a[i]* tmpRet;
        }
}

void pcovcc(double *a, double *ap, double *b, double *at,double *atp, double *bt, int *na, int *nap, int *nb, int *period, double *ans)
{
        int i, prevj = 0, j, nbi=*nb, nai = *na, napi = *nap;
        double tmpRet=0;

        for(i = 0; i < nai; i++)
        {
            ap[i/ *period] += a[i];
            atp[i/ *period] = at[i];
        }
        for(i = 0; i < napi; i++)
        {
            tmpRet=0;
            for(j=prevj; j < nbi; j++)
            {
                tmpRet += b[j];
                if(bt[j] > atp[i])
                {
                    prevj = j;
                    j = nbi;
                }
                else if(bt[j] == atp[i])
                {
                    prevj = j+1;
                    j = nbi;
                }
            }
            ans[i] = ap[i]* tmpRet;
        }
}


void sametime(double *a, int *na, int *millis, double *tts)
{
    int prev = -1, i, j=0;
    for(i = 0; i < *na; i ++)
    {
    	if(millis[i] != prev)
    	{
    	    prev = millis[i];
    	    tts[j] = a[i];
    	    j = j +1;
    	}
    }
}

void tocts(double *a, int *na, int *millis, int *millisstart, int *millisend, double *cts)
{
//	array cts is passed from R having length (millisend-millisstart)/1000
//	loop tries to write (*millisend - *millisstart)/1000 +1 values into the
//	array -> C runs out of memory and throws segfault error
//    int n = (*millisend - *millisstart)/1000 +1,i, j=0;
//	Still need to carefully check if the correction below works correctly, maybe
//  you rather need to increase the size of the array on the R side
    int n = (*millisend - *millisstart)/1000,i, j=0;
   for(i = 0; i < n; i ++)
    {
    	if((j < *na) && (millis[j] == ((i*1000) + *millisstart)))
    	{
    	    cts[i] = a[j];
    	    j = j+1;
    	}
    	else
    	{
    	   cts[i] = 0;
    	}
    }
 }

void portfolio(double *a, double *b, int *na, int *nb, int *millisa, int *millisb, int *millis,int *milliesn, double *port)
{
    int i, j=0, k=0;
    for(i = 0; i < *milliesn; i ++)
    {
    	if((j < *na) && (millisa[j] == millis[i]))
    	{
    	    port[i] = a[j];
    	    j = j+1;
    	}
    	else
    	{
    	   port[i] = 0;
    	}
        if((k < *nb) && (millisb[k] == millis[i]))
    	{
    	    port[i] += b[k];
    	    k = k+1;
    	}
    }
 }

int nsmaller(int *times, int *lengths, int start, int end, int max)
{
int i=0;
while ( (i < (lengths[end] - lengths[start])) && (*(times+ (lengths[start]+i)) <= max)){
i++;
}
return i;
}

void refreshpoints( int *times, int *lengths, int *ttau, int *dim, int *aa, int *indices, int *lindex){
  //my first C program, so probably improvable..
  //length start with all starting points: from zero and upto last endpoint +1
  // getting variables declared
int  t_max=0, i, j,a,b;;
int condition = 1;
int Ntau[*dim];
int tnext[*dim];
  //int xx tau[*lindex];
  // getting starting values before looping
 *ttau = 0;
 for(i = 0; i<*dim ; i++){
     if( *ttau < *(times + lengths[i]) )
     { *ttau = *(times + lengths[i] ); }
     }

 for(i = 0; i < *dim; i++){
    Ntau[i] = nsmaller(times, lengths, i, (i+1), *ttau);
    *(indices + i * (*lindex))  = Ntau[i];}
 j=0;
 // start the loop over all observations
while( condition >= 1 ){

 for(i = 0; i<*dim; i++){
    tnext[i] = *(times + Ntau[i] + lengths[i]);
    if( tnext[i] > t_max ){ t_max = tnext[i]; }
    }
    j++;
    *(ttau+j) = t_max;
 for(i = 0; i<*dim; i++){
    Ntau[i] = nsmaller(times, lengths, i, (i+1), *(ttau+j));
    *(indices + i* (*lindex) + j)  = Ntau[i];
    a = lengths[i+1];
    b =  (Ntau[i] + lengths[i]);
    if( a <= b ){ condition = 0; }
    }
    t_max = 0;

 }
 *aa = j+1;
}


  void heavy_likelihoodR(double *parameters, double *data, int *T1, int *K1, double *means, int *p, int *q, int *pMax1, int *qMax1, 
                         double *backcast, double *LB, double *UB, int *compconst1, double *h, double *lls, double *llRM, double *ll) 
{
    
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
    
    if(compconst == 1)
    {heavy_parameter_transformR(parameters, K, p, q, &O[0], &A[0], &B[0],pMax1,qMax1);}
    else
    {heavy_parameter_transform_RetrackR(parameters, K, p, q, means, &O[0], &A[0], &B[0],pMax1,qMax1);}
    
    
    for(i=0;i<K;i++) if(O[i] < 0) {O[i] = .000001;} //?? O(O<0) = realmin();
    
    for(t = 0; t < T; t++) //for t=1:T
{
  
  //for(i=0;i<K;i++) {SETM(model->h,t,i,O[i]);} //h(:,t) = O
  
  for(i=0;i<K;i++) {h[t*K + i] = O[i];}
  
  //------------------------------------------------
    
    for(j=0;j<pMax;j++) // j=1:pMax
{
  if(t-j > 0)
  {        
    //h(:,t) = h(:,t) + A(:,:,j)*data(:,t-j);
    for(l=0;l<K;l++) 
    {
      sum = 0.0;
      for(k=0;k<K;k++)
      {sum = sum + A[K*K*j + K*k + l]*data[(t-j-1)*K+k];} //{sum = sum + A[K*K*j + K*l + k]*GETM(model->data,t-j-1,k);}
      
      htemp = h[t*K+l]; // htemp = GETM(model->h,t,l); 
      h[t*K+l] = htemp+sum; //SETM(model->h,t,l,htemp + sum);
    }
  }    
  else
  {
    //h(:,t) = h(:,t) + A(:,:,j)*backCast';
    for(l=0;l<K;l++) 
{
    sum = 0.0;
    for(k=0;k<K;k++)
{sum = sum + A[K*K*j + K*k + l]*backcast[k];} //{sum = sum + A[K*K*j + K*l + k]*GET(model->backcast,k);}
    //htemp = h[t*K+l]; //htemp = GETM(model->h,t,l); 
    h[t*K + l] = h[t*K+l]+sum; //SETM(model->h,t,l,htemp + sum);
} 
  }
    
}
    
    for(j=0;j<qMax;j++) // j=1:pMax
{
    if(t-j > 0)
{        
    //h(:,t) = h(:,t) + B(:,:,j)*h(:,t-j);
    for(l=0;l<K;l++) 
{
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
  
  void heavy_parameter_transformR(double *parameters, int K, int *p, int *q, double *O, double *A, double *B, int *pMax1, int *qMax1)
{
    
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
  
  
  void heavy_parameter_transform_RetrackR(double *parameters, int K,  int *p,  int *q,  double* means, double *O, double *A, double *B, int *pMax1, int *qMax1)
{
    
    int i,j,k;
    
    int pc,qc;
    int offset = K;
    int pMax,qMax;
    
    pMax = pMax1[0];
    qMax = qMax1[0];
    
    double suma[K];
    double sumb[K];
    
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
    
    
    for(k=0;k<K;k++)
    {
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



  
