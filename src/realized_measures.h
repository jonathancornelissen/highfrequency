double KK(double x, int type);

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

int nsmaller(int *times, int *lengths, int start, int end, int max);

