void heavy_likelihoodR(double *parameters, double *data, int *T, int *K, double *means, int *p, int *q, int *pMax, int *qMax, 
    	double *backcast, double *LB, double *UB, int *compconst, double *h, double *lls, double *llRM, double *ll);
      
void heavy_parameter_transformR(double *parameters, int K, int *p, int *q, double *O, double *A, double *B, int *pMax,int *qMax);

void heavy_parameter_transform_RetrackR(double *parameters, int K,  int *p,  int *q,  double* means, double *O, double *A, double *B, int *pMax,int *qMax);

double KK(double x, int type);

void justKernel(double *x, int *type, double *ans);

void kernel(double *a, int *na, int *q, int *adj, int *nw, double *ab, double *ans);

void kernelEstimator(double *a, double *b, int *na, int *q, int *adj, int *type, double *ab,  double *ab2, double *ans);

void kernel2(double *a, double *b, int *na, int *q, int *adj, int *nw, double *ab,  double *ab2, double *ans);

void subsample(double *a, double *b, int *na, int *m, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans);

void rv(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans);

void rvperiod(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans);

void rfourth(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans);

void rfourthlead(double *a, double *b, int *na, int *period, double *tmpa, double *tmpb, int *tmpna, double *ans);

void covkernel(double *a, double *b, int *na, int *q, int *adj, double *ans);

void covcc(double *a, double *b, double *at, double *bt, int *na, int *nb, double *ans);

void pcovcc(double *a, double *ap, double *b, double *at,double *atp, double *bt, int *na, int *nap, int *nb, int *period, double *ans);

void sametime(double *a, int *na, int *millis, double *tts);

void tocts(double *a, int *na, int *millis, int *millisstart, int *millisend, double *cts);

void portfolio(double *a, double *b, int *na, int *nb, int *millisa, int *millisb, int *millis,int *milliesn, double *port);

void refreshpoints( int *times, int *lengths, int *ttau, int *dim, int *aa, int *indices, int *lindex);


