####JOjumptest:Jiang and Oomen tests for the presence of jumps in the price series.####

JOjumptest= function(pdata, power=4,...)
{
  R  = as.zoo(.simre(pdata));
  r  = as.zoo(makeReturns(pdata));
  N  = length(pdata)-1
  bv = RBPVar(r)
  rv = RV(r)
  
  SwV = 2*sum(R-r,na.rm = TRUE)
  mu1 = 2^(6/2)*gamma(1/2*(6+1))/gamma(1/2)
  
  ##mupower:
  if(power==4)
  {
    q      = abs(rollapply(r, width = 4, FUN = prod, align = "left",na.rm = TRUE))
    mu2    = 2^((6/4)/2)*gamma(1/2*(6/4+1))/gamma(1/2)
    av     = mu1/9 * N^3*(mu2)^(-4)/(N-4-1)*sum(q^(6/4),na.rm= TRUE)   ##check formula
    JOtest = N*bv/sqrt(av)*(1- rv/SwV)
    
    out                = {}
    out$ztest          = JOtest
    out$critical.value = qnorm(c(0.025,0.975))
    out$pvalue         = 2*pnorm(-abs(JOtest))
    return(out)
  }     
  
  if(power==6)
  {
    q=abs(rollapply(r, width = 6, FUN = prod, align = "left",na.rm = TRUE))
    mu2= 2^((6/6)/2)*gamma(1/2*(6/6+1))/gamma(1/2)
    av=mu1/9 * N^3*(mu2)^(-6)/(N-6-1)*sum(q^(6/6),na.rm= TRUE)   ##check formula
    JOtest= N*bv/sqrt(av)*(1- rv/SwV)
    
    out                = {}
    out$ztest          = JOtest
    out$critical.value = qnorm(c(0.025,0.975))
    out$pvalue         = 2*pnorm(-abs(JOtest))
    return(out)
  }     
}  