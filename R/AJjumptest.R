####AJjumptest:Ait- Sahalia and Jacod tests for the presence of jumps in the price series.####

AJjumptest = function(pdata, p=4 , k=2, align.by= NULL, align.period = NULL,alpha_multiplier=4, makeReturns= FALSE, ...)
{
  if (hasArg(data)) {  pdata = data  }
  
  multixts = .multixts(pdata)
  
  if (multixts) 
  {
    result = apply.daily(pdata, AJjumptest, align.by, align.period, makeReturns)
    return(result)
  }else{
    pdata = .aggregatets(pdata, on = "seconds", k = 1)
  }
  
  N = length(pdata)-1;
  p = as.numeric(p);
  k = as.numeric(k);
  
  alpha = alpha_multiplier * sqrt(rCov(pdata, align.by = align.by, align.period = align.period, makeReturns = makeReturns))
  w = 0.47;
  cvalue = alpha*(1/N)^w;
  
  h = align.period * .scale(align.by)
  hk= h*k
  
  seq1 = seq(1, N, h);
  seq2 = seq(1, N, hk);
  
  # return data
  pdata1 = pdata[seq1];
  pdata2 = pdata[seq2];
  
  r  = abs(makeReturns(pdata));
  r1 = abs(makeReturns(pdata1));
  r2 = abs(makeReturns(pdata2));
  
  pv1 = sum(r1^p);
  pv2 = sum(r2^p);
  
  S = pv1/pv2;
  
  ## selection return:
  selection <- abs(r) < cvalue
  rse <- abs(makeReturns(pdata[selection]))
  
  ## AJ test: 
  AJtest = (S-k^(p/2-1))/sqrt(.V(rse,p,k,N))
  
  out = {};
  out$ztest = AJtest;
  out$critical.value = qnorm(c(0.025,0.975));
  out$pvalue = 2*pnorm(-abs(AJtest));
  return(out)  
}    

