
# ############### STANDARD ERROR OF HEAVY MODEL IMPLEMENTATION ##########
# ###########
# # Likelihood for HEAVY volatility model of Shephard and Sheppard
# # Code is R-translation by Jonathan Cornelissen of matlab code of http://www.kevinsheppard.com/wiki/MFE_Toolbox
# # by: kevin.sheppard@economics.ox.ac.uk
# 
# # INPUTS:
# #   PARAMETERS  - A vector with K+sum(sum(P))+sum(sum(Q)) elements.
# #    DATA       - A T by K vector of non-negative data.  Returns should be squared before using
# #    P          - A K by K matrix containing the lag length of model innovations.  Position (i,j)
# #                   indicates the number of lags of series j in the model for series i
# #    Q          - A K by K matrix containing the lag length of conditional variances.  Position (i,j)
# #                   indicates the number of lags of series j in the model for series i
# #    BACKCAST   - A 1 by K matrix of values to use fo rback casting
# #    LB         - A 1 by K matrix of volatility lower bounds to use in estimation
# #    UB         - A 1 by K matrix of volatility upper bounds to use in estimation
# #
# # OUTPUTS:
# #    LL          - The log likelihood evaluated at the PARAMETERS
# #    LLS         - A T by 1 vector of log-likelihoods
# #    HT          - A T by K matrix of conditional variances
# 
# 
# .get_param_names = function( estparams, p, q){
#   K = dim(p)[2];
#   nAlpha =  sum(p);
#   nBeta  =  sum(q);
#   omegas = paste("omega",1:K,sep="");
#   alphas = paste("alpha",1:nAlpha,sep="");
#   betas  = paste("beta", 1:nBeta,sep="");
#   names  = c(omegas,alphas,betas);
# 
# }
# 
# 
# ############### INTERNAL HELP FUNCTIONS ###############
# ### thetaROWVar help functions:
# .IF_MCD = function( x, alpha )
# {
#   N = 1
#   q = qchisq( 1-alpha , df = N )
#   calpha = (1-alpha)/pchisq( q , df = N+2 )
#   out = ( (x^2-q)/(1-alpha) )*( abs(x) <= sqrt(q) )
# 
#   return( -1+q*calpha + calpha*out )
# }
# 
# # .int = function(x){
# #   return( .IF_MCD(x)^2*dnorm(x) )
# # }
# 
# .avar_MCD = function(alpha){
#   N = 1
#   q_alpha = qchisq( 1-alpha , df = N )
#   c_alpha = (1-alpha)/pchisq( q_alpha , df = N+2 )
#   a_alpha = -2*sqrt(q_alpha)*dnorm(sqrt(q_alpha))+1-alpha
#   b_alpha = -2*q_alpha^(3/2)*dnorm(sqrt(q_alpha))+3*a_alpha
# 
#   avar = c_alpha^2*q_alpha^2+1-2*c_alpha*q_alpha
#   avar = avar + c_alpha^2/(1-alpha)^2*(b_alpha+q_alpha^2*(1-alpha)-2*q_alpha*a_alpha)
#   avar = avar + 2*( c_alpha*q_alpha - 1)*c_alpha*(1/(1-alpha))*(-q_alpha*(1-alpha)+a_alpha)
# 
#   return(avar)
# }
# 
# 

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## BNSJumptest help functions:
# 
# ## zgamma function:
# .zgamma <- function (x, y, gamma_power) {##gamma_power need to be in number 
#   if (x^2 < y) {
#     out <- abs(x)^gamma_power
#   } else {
#     if (gamma_power == 1) {
#       out <- 1.094 * sqrt(y)
#     }
#     if (gamma_power == 2) {
#       out <- 1.207 * y
#     }
#     if (gamma_power == 4/3) {
#       out <- 1.129 * y^(2/3)
#     } 
#   }
#   
#   return(out)
# }
# ##corrected threshold bipower variation:
# .ctBV = function(rdata, startV = NULL) {
#   #q = as.numeric(rdata)
#   N = length(rdata);
# 
#   if (is.null(startV)) {
#     hatV = medRV(rdata)
#   } else {
#     hatV = startV
#   }
#   v = 3^2 * hatV
#   z1 = rep(0, N - 1);
#   for (i in 2:N) {
#     z1[i-1] = .zgamma(rdata[i], v, gamma_power = 1)
#   }
# 
#   z2 = rep(0, N - 1);
#   for (j in 1:(N - 1)) {
#     z2[j] = .zgamma(rdata[j], v, gamma_power = 1)
#   }
#   ctbv = (pi/2) * sum(z1 * z2)
#   return(ctbv)
# }
# 
# 
# ###C-TTriPV
# .ctTPV = function (rdata, startV = NULL){
#   q = as.numeric(rdata)
#   N = length(rdata);
# 
#   if (is.null(startV)) {
#     hatV = medRV(rdata)
#   }else{
#     hatV = startV
#   }
#   v = 3^2 * hatV
#   z1 = rep(0, N - 2);
#   for (i in 3:N) {
#     z1[i-2] = .zgamma(rdata[i], v, gamma_power = 4/3)
#   }
# 
#   z2 = rep(0, N - 2);
#   for (j in 2:(N - 1)) {
#     z2[j-1] = .zgamma(rdata[j], v, gamma_power = 4/3)
#   }
#   z3 = rep(0, N - 2);
#   for (l in 1:(N-2)) {
#     z3[l] = .zgamma(rdata[l], v, gamma_power = 4/3)
#   }
#   cttpv = 0.8309^(-3) * sum(z1^(4/3) * z2^(4/3) * z3^(4/3))
#   return(cttpv)
# }
# 
# 


# 
# .thetaROWVar <- function(alpha = 0.001 , alphaMCD = 0.5) {
#   N = 1
#   q_alpha = qchisq(1-alpha , df = N)
#   c_alpha = (1-alpha) / pchisq(q_alpha , df = N+2)
#   a_alpha = -2 * sqrt(q_alpha) * dnorm(sqrt(q_alpha)) + 1 - alpha
#   b_alpha = -2 * q_alpha^(3/2) * dnorm(sqrt(q_alpha)) + 3 * a_alpha
# 
#   k = qchisq(1 - alpha, df = 1); #TODO GIANG ## suggestion in the article.
#   halfk = sqrt(k) 
#   halfq = sqrt(q_alpha)
# 
#   Ewu2   = 2 * pnorm(halfk)-1
#   Ewu2u2 = -2 * halfk * dnorm(halfk) + Ewu2
#   Ewu2u4 = -2 * (k^(3/2)) * dnorm(halfk)+3 * Ewu2u2
# 
#   Ewu2u2IF = (-1+c_alpha*q_alpha-(c_alpha*q_alpha)/(1-alpha))*a_alpha+c_alpha*b_alpha/(1-alpha)
#   Ewu2u2IF = Ewu2u2IF + 2*(1-c_alpha*q_alpha)*(
#     halfk*dnorm(halfk)-halfq*dnorm(halfq) + 1 - alpha/2 - pnorm(halfk)   )
#   Ewu2IF = (alpha-1-c_alpha*q_alpha*alpha) + c_alpha*a_alpha/(1-alpha) + 2*(c_alpha*q_alpha-1)*( pnorm(halfk)-(1-alpha/2))
#   Ederwu2u4 = -k^(3/2)*dnorm(halfk)
#   Ederwu2u2 = -halfk*dnorm(halfk)
#   c1 = 1/Ewu2u2;   c2 = 1/Ewu2;   c3 = c2*Ederwu2u2-c1*Ederwu2u4
#   Avar0 = .avar_MCD(alpha)
#   theta = c3^2*Avar0 + c1^2*Ewu2u4 + c2^2*Ewu2 - 2*c1*c2*Ewu2u2
#   theta = theta + 2*c3*( c1*Ewu2u2IF-c2*Ewu2IF )
# 
#   return(theta)
# }
# 
# 
# ### Standard error of HEAVY model:
# ## INPUT:
# # - rpara, rmpara: vectors of parameters: omega, alpha and beta of demeaned returns and realized measures from heavyModel.
# # - lR, lRM: function of log-likelihood from heavyModel;
# # Note: numDeriv package is required.
# ## OUTPUT: standard error of parameters calculated from log-likelihood heavyModel.
# 
# 
# .transformparams <- function( p, q, par ) {
#   K = dim(p)[1];
#   pmax = max(p); qmax = max(q); # Max number of lags for innovations and cond vars
# 
#   O = matrix( par[1:K], ncol=1);
#   A = B = list();
#   start = (K+1);
# 
#   for(i in 1:pmax){    # A will contain a list-item per innovation lag
#     end =          start + sum(p>=i) - 1; # How many non-zero params in this loop?
#     A[[i]] =       matrix(rep(0,K^2),ncol=K);
#     A[[i]][p>=i] = par[start:end];
#     start  = end + 1;
#   }#end loop over number of lags for innovations
# 
#   for(i in 1:qmax){   # B will contain a list-item per cond var lag
#     end   = start + sum(q>=i) -1; # How many non-zero params in this loop?
#     B[[i]] = matrix(rep(0,K^2),ncol=K);
#     B[[i]][q >= i] = par[start:end];
#     start  = end + 1;
#   }#End loop over number of lags for cond variances
# 
#   return( list(O,A,B) )
# }
# 
# 
# 
# .heavy_likelihood <- function( par, data, p, q, backcast, LB, UB, foroptim=TRUE, compconst=FALSE ){
#   # Get the required variables
#   # p is Max number of lags for innovations
#   # q is Max number of lags for conditional variances
# 
#   K    = dim(data)[2];  #Number of series to model
#   T    = dim(data)[1];  #Number of time periods
#   lls  = rep(NA,T);     #Vector containing the likelihoods
#   h    = matrix(nrow=K,ncol=T); #Matrix to containing conditional variances
#   maxp = max(p); maxq=max(q);
# 
#   if(!(class(data) == "matrix")){data = matrix(data,ncol=K)}
# 
#   # Get the parameters:
#   x = .transformparams( par, p=p, q=q );
#   if( compconst ){ O = x[[1]]; }
#   A = x[[2]]; B = x[[3]];
#   # Compute constant in case it needn't to be optimized:
#   if( !compconst ){ # Don't compute the omega's but do (1-alpha-beta)*unconditional
#     totalA = totalB = matrix( rep(0,K) ,ncol=1,nrow=K);
#     for(j in 1:length(A) ){ totalA = totalA + t(t(rowSums(A[[j]]))); } # Sum over alphas for all models
#     for(j in 1:length(B) ){ totalB = totalB + t(t(rowSums(B[[j]]))); } # Sum over betas for all models
#     O = 1 - totalA - totalB; # The remaing weight after substracting A & B
#     # Calculate the unconditionals
#     uncond = t(t(colMeans(data)));
#     O = O*uncond;
#   } #End if close for not optimizing over omega
# 
#   if( sum(O) < 0 ){ O[O<0] = 10^(-10)} #params here are shouldn't be smaller than zero
# 
#   likConst = K*log(2*pi); #constant for loglikelihood
# 
#   for(t in 1:T){ # Start loop over time periods
#     h[,t] = O;    #Add constant to h
# 
#     for(j in 1:maxp){# Loop over innovation lags
#       if( (t-j) > 0 ){
#         h[,t] = h[,t] + t( A[[j]] %*% t(t(data[(t-j),])) ); #Adding innovations to h
#       }else{
#         h[,t] = h[,t] + t( A[[j]] %*% backcast ); #Adding innovations to h
#       }
#     } #end loop over innovation lags # CHECK: error caution????
# 
#     for(j in 1:maxq){# Loop over cond variances lags
#       if( (t-j) > 0 ){
#         h[,t] = h[,t] + t( B[[j]] %*% t(t(h[,(t-j)])) ); #Adding cond vars to h
#       }else{
#         h[,t] = h[,t] + t( B[[j]] %*% backcast ); #Adding cond vars to h
#       }
#     }#End loop over innovation lags
# 
#     if( any( h[,t]>1e3 )){ break ;}
#     # Check whether h's are between LB and UB:
#     for(j in 1:K){ #Loop over
#       if( h[j,t] < LB[j] ){  h[j,t] = LB[j]/(1- (h[j,t] - LB[j]) );}
#       if( h[j,t] > UB[j] ){  h[j,t] = UB[j] + log( h[j,t] - UB[j]);}
#     }#end loop over series
# 
#     lls[t] = 0.5*( likConst + sum( log(h[,t]) ) + sum( data[t,] / h[,t] ) );
#   } #End loop over days
#   ll = sum(lls);
# 
#   if(is.na(ll) || is.infinite(ll) ){  ll = 10^7 }
# 
#   if(foroptim){   output = ll; return(output); }
#   if(!foroptim){
#     output = list( loglikelihood=ll, likelihoods=lls, condvar = h );
#     return(output)
#     # Output list:
#     # (i) total loglikelihood
#     # (ii) likelihood parts per time period
#     # (iii) matrix with conditional variances
#   } #end output in case you want params
# }
# 
# 
# 
# .heavy_likelihood_ll  <- function( splittedparams, data, p, q, backcast, LB, UB, compconst=FALSE, ... ){
#   K = ncol(data);
#   TT = nrow(data);
#   means = c(colMeans(data));
#   maxp  = max(p);
#   maxq = max(q);
# 
#   par = .transtopar( splittedparams,  p, q );
# 
# 
#   out = .C("heavy_likelihoodR",
#            parameters = as.double(par),
#            data = as.double(t(data)),
#            TT = as.integer(TT),
#            K = as.integer(K),
#            means = as.double(means),
#            p = as.integer(p),
#            q = as.integer(q),
#            pMax = as.integer(maxp),
#            qMax = as.integer(maxq),
#            backcast = as.double(t(backcast)),
#            LB = as.double(LB),
#            UB = as.double(UB),
#            compconst = as.integer(compconst),
#            h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
#            lls = as.double( rep(0, TT) ),
#            llRM = as.double( rep(0,K ) ),
#            ll = as.double(0),
#            PACKAGE="highfrequency");
# 
#   return((-1)*out$ll)
# }
# 
# 
# 
# .heavy_likelihood_lls  = function( splittedparams , data, p, q, backcast, LB, UB, compconst=FALSE,... ){
#   K = ncol(data);
#   TT = nrow(data);
#   means = c(colMeans(data));
#   maxp  = max(p);
#   maxq = max(q);
# 
#   par = .transtopar( splittedparams,  p, q );
# 
# 
#   out = .C("heavy_likelihoodR",
#            parameters = as.double(par),
#            data = as.double(t(data)),
#            TT = as.integer(TT),
#            K = as.integer(K),
#            means = as.double(means),
#            p = as.integer(p),
#            q = as.integer(q),
#            pMax = as.integer(maxp),
#            qMax = as.integer(maxq),
#            backcast = as.double(t(backcast)),
#            LB = as.double(LB),
#            UB = as.double(UB),
#            compconst = as.integer(compconst),
#            h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
#            lls = as.double( rep(0, TT) ),
#            llRM = as.double( rep(0,K ) ),
#            ll = as.double(0),
#            PACKAGE="highfrequency");
# 
#   return((-1)*out$lls)
# }
# 
# .get_param_names = function( estparams, p, q){
#   K = dim(p)[2];
#   nAlpha =  sum(p);
#   nBeta  =  sum(q);
#   omegas = paste("omega",1:K,sep="");
#   alphas = paste("alpha",1:nAlpha,sep="");
#   betas  = paste("beta", 1:nBeta,sep="");
#   names  = c(omegas,alphas,betas);
# 
# }
# 
# 
# .SEheavyModel <- function( paramsvector, data, p, q, backcast, LB, UB, compconst=FALSE, ...) {
# 
#   K <- ncol(data);  #Number of series to model
# 
#   # Set lower and upper-bound if not specified:
#   if (is.null(LB) == TRUE){ 
#     LB = rep(0,K)   
#   }
#   if (is.null(UB) == TRUE){ 
#     UB = rep(Inf, K) 
#   }
# 
#   if (compconst == FALSE) {
#     # based on p and q, map heavy paramsvector into splitted params vector called theta
# 
#     out = .transtosplit ( paramsvector=paramsvector,  p=p, q=q)
# 
#     # vk[i] is the number of parameters of equation i in the heavy model
#     vk = out[[2]];
# 
#     splittedparams = out[[1]];
# 
#     # compute the asymptotic covariance matrix of splittedparamsvector
# 
#     mH = numDeriv::hessian(.heavy_likelihood_ll, x= splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst)
# 
#     T        = nrow(data)
#     nm       = length(paramsvector)
#     Jmatrix  = matrix (rep(0, nm^2), ncol = nm)
#     end = 0
# 
# 
#     for( k in 1:K){
#       start = end + 1
#       end   = start + vk[k] - 1
#       Jmatrix[start:end, start:end] =  mH[start:end, start:end]
#     }
# 
#     ## Define It
#     # jacobian will be T x length of theta
#     m  = numDeriv::jacobian(.heavy_likelihood_lls, x = splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst) # returns a vector?
#     It = cov(m)
# 
#   } else {
# 
#     # Change value of mu:
#     paramsvector[1:K]  = colMeans(data)
# 
# 
#     # based on p and q, map heavy paramsvector into splitted params vector called theta
# 
#     out = .transtosplit ( paramsvector=paramsvector,  p=p, q=q)
# 
#     # vk[i] is the number of parameters of equation i in the heavy model
#     vk = out[[2]];
# 
#     splittedparams = out[[1]];
# 
#     # compute the asymptotic covariance matrix of splittedparamsvector
# 
#     mH = numDeriv::hessian(.heavy_likelihood_ll, x= splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst)
# 
#     T        = nrow(data)
#     nm       = length(paramsvector)
#     Jmatrix  = matrix (rep(0, nm^2), ncol = nm)
#     end = 0
# 
# 
#     for( k in 1:K){
#       start = end + 1
#       end   = start + vk[k] - 1
#       Jmatrix[start:end, start:end] =  mH[start:end, start:end]
#     }
# 
#     # Change value of matrix Jt:
# 
#     # Change value of 0:
#     end = 0;
# 
#     for (j in 1:K )
#     {
#       if(j>1){col= col+ vk[j-1]}
#       else{col=1};
# 
#       start = end + 1;
#       end = start + vk[j] - 1;
#       Jmatrix[start:end,col] = 0;
# 
#     }
# 
#     # Change value of T:
#     start = 1;
# 
#     for (i in 1:K)
#     {
# 
#       Jmatrix[start,start] = T;
# 
#       start = start + vk[i]
#     }
# 
#     ## Define It
#     # jacobian will be T x length of theta
#     m  = numDeriv::jacobian(.heavy_likelihood_lls, x = splittedparams, data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst) # returns a matrix of T by L (length of theta)
# 
#     start = 1;
#     for (i in 1:K)
#     {
#       m[,start] = 1/T * (data[,i] - paramsvector[i]);
#       start = start + vk[i-1]
# 
#       if(i<K){start = start + vk[i]}
# 
#     }
# 
# 
#     fm = lm(m ~ 0);
#     It = try(sandwich::vcovHAC(fm))
# 
#     if(class(It) == "try-error") {
#       print("HAC estimator reports an error. It is replaced by non-HAC estimator.")
#       It = cov(m)
#     }
#   }
# 
#   ## Jt matrix
#   Jt = - 1/T * Jmatrix
#   ## J-1 (inverse matrix of J):
# 
#   invJ <- try(solve(Jt))
#   if (class(invJ) == "try-error") {
#     print("-1 * Hessian is not invertible - generalized inverse is used")
#     invJ = MASS::ginv(Jt)
#   }
# 
# 
#   ## Standard error:
# 
#   ACOVheavyModel =  (invJ %*% It %*% t(invJ)) #return a matrix
# 
#   # compute the standard errors of splittedparamsvector
# 
#   # Standard error:
#   SEheavyModel = sqrt(diag(ACOVheavyModel))/sqrt(T)
# 
#   # reorder such that they correspond to paramsvector
#   reSEheavyModel = .transtopar(SEheavyModel, p, q)
# 
#   # map it back to paramsvector
#   out           = as.matrix(t(rbind(paramsvector, reSEheavyModel)))
#   colnames(out) <- c("Parameter", "Standard error")
#   rownames(out) = .get_param_names(estparams = paramsvector, p=p, q=q)
#   return(out)
# 
# }
# 
# ## Function: heavyModel by applying C code:
# heavyModelC <- function (data, p = matrix(c(0, 0, 1, 1), ncol = 2), q = matrix(c(1, 0, 0, 1), ncol = 2),
#                         startingvalues = NULL, LB = NULL, UB = NULL, backcast = NULL, compconst = FALSE) {
#   K = ncol(data);
#   TT = nrow(data);
#   means = c(colMeans(data));
#   maxp  = max(p);
#   maxq = max(q);
# 
#   if (is.null(LB)) {
#     LB = rep(0, K)
#   }
# 
#   if (is.null(UB)) {
#     UB = rep(10^6, K)
#   }
# 
#   if (is.null(startingvalues)) {
#     startingvalues = rep(NA, K + sum(p) + sum(q))
#     startingvalues[1:K] = 0.1
#     start = K + 1
#     end = K + sum(p)
#     startingvalues[start:end] = 0.3
#     start = end + 1
#     end = start + sum(q) - 1
#     startingvalues[start:end] = 0.6
#   }
# 
#   if (is.null(backcast)) {
#     backcast = t(t(colMeans(data)))
#   }
# 
#   KKK = length(startingvalues)
#   ui = diag(rep(1, KKK))
#   ci = rep(0, dim(ui)[2])
# 
#   splittedparams = .transtosplit(startingvalues, p, q)[[1]];
# 
#   x = try(optim( par = splittedparams, fn = .heavy_likelihood_llC,  data=data, p=p, q=q, backcast=backcast, LB=LB, UB=UB, compconst=compconst,
#                  method = "L-BFGS-B"));
# 
# 
#   if (class(x) == "try-error") {
#     print("Error in likelihood optimization")
#     print(x)
#   }
#   else {
#     if (x$convergence != 0) {
#       print("Possible problem in likelihood optimization. Check convergence")
#     }
#   }
# 
#   estparams = x$par
# 
#   loglikelihood = x$value
# 
#   xx = .C("heavy_likelihoodR",
#           parameters = as.double(estparams),
#           data = as.double(t(data)),
#           TT = as.integer(TT),
#           K = as.integer(K),
#           means = as.double(means),
#           p = as.integer(p),
#           q = as.integer(q),
#           pMax = as.integer(maxp),
#           qMax = as.integer(maxq),
#           backcast = as.double(t(backcast)),
#           LB = as.double(LB),
#           UB = as.double(UB),
#           compconst = as.integer(compconst),
#           h = as.double(matrix(rep(0,K*TT),nrow=K,ncol=TT)),
#           lls = as.double( rep(0, TT) ),
#           llRM = as.double( rep(0,K ) ),
#           ll = as.double(0),
#           PACKAGE="highfrequency")
# 
#   if (is.null(rownames(data)) == FALSE) {
#     xx$condvar = xts(t(matrix(xx$h,K)), order.by = as.POSIXct(rownames(data)))
#     xx$likelihoods = xts(t(matrix(xx$lls,1)), order.by = as.POSIXct(rownames(data)))
#   }
#   xx$estparams = matrix(estparams, ncol = 1)
#   rownames(xx$estparams) = .get_param_names(estparams, p, q)
#   xx$convergence = x$convergence
#   return(xx)
# }



