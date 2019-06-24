void heavy_likelihoodR_(double *parameters, double *data, int *T, int *K, double *means, int *p, int *q, int *pMax, int *qMax, 
    	double *backcast, double *LB, double *UB, int *compconst, double *h, double *lls, double *llRM, double *ll);
      
void heavy_parameter_transformR_(double *parameters, int K, int *p, int *q, double *O, double *A, double *B, int *pMax,int *qMax);

void heavy_parameter_transform_RetrackR_(double *parameters, int K,  int *p,  int *q,  double* means, double *O, double *A, double *B, int *pMax,int *qMax);

