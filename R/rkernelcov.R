#' 
#' #' Realized Covariance: Kernel
#' #' 
#' #' @description Realized covariance calculation using a kernel estimator. 
#' #' 
#' #' @param rdata a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
#' #' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' #' @param cor boolean, in case it is TRUE, the correlation is returned. FALSE by default.
#' #' @param align.by Align the tick data to seconds|minutes|hours
#' #' @param align.period Align the tick data to this many [seconds|minutes|hours]
#' #' @param makeReturns Convert to Returns
#' #' @param kernel.type Kernel name (or number)
#' #' @param kernel.param Kernel parameter (usually lags)
#' #' @param kernel.dofadj Kernel Degree of freedom adjustment
#' #' 
#' #' @details The different types of kernels can be found using \code{\link{listAvailableKernels}}.
#' #' 
#' #' @return Kernel estimate of realized covariance.
#' #' 
#' #' Ole E. Barndorff-Nielsen, Peter Reinhard Hansen, Asger Lunde, and Neil Shephard (2008). Designing Realized Kernels to Measure the ex post Variation of Equity Prices in the Presence of Noise. \emph{Econometrica}, 76, pp. 1481-1536.
#' #' 
#' #' B. Zhou. High-frequency data and volatility in foreign-exchange rates. \emph{Journal of Buiness & Economic Statistics}, 14:45-52, 1996.
#' #' 
#' #' P. Hansen and A. Lunde. Realized variance and market microstructure noise. \emph{Journal of Business and Economic Statistics}, 24:127-218, 2006.
#' #' 
#' #' @author Scott Payseur
#' #' 
#' #' @examples
#' #' # Average Realized Kernel Variance/Covariance for aligned at one minute returns at 
#' #' # 5 subgrids (5 minutes).
#' #' # Univariate: 
#' #' rvKernel <- rKernelCov(rdata = sample_tdata$PRICE, align.by = "minutes", 
#' #'                        align.period = 5, makeReturns = TRUE)
#' #' rvKernel 
#' #' 
#' #' Multivariate: 
#' #' rcKernel <- rKernelCov(rdata = cbind(lltc, sbux), period = 5, align.by = "minutes", 
#' #'                        align.period = 5, makeReturns = FALSE)
#' #' rcKernel 
#' #' @keywords volatility
#' #' @export
#' rKernelCov <- function(rdata, cor = FALSE,  align.by = "seconds", align.period = 1, 
#'                        makeReturns = FALSE, kernel.type = "rectangular", kernel.param = 1, 
#'                        kernel.dofadj = TRUE) {
#'   
#'   multixts <- multixts(rdata)
#'   if (multixts == TRUE) { 
#'     stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
#'   }
#'   
#'   # Aggregate:
#'   if ((!is.null(align.by)) && (!is.null(align.period))) {
#'     rdata <- fastTickAgregation(rdata, on = align.by, k = align.period)
#'   }     
#'   if (makeReturns == TRUE) { 
#'     rdata <- makeReturns(rdata)
#'   }
#'   
#'   if (is.null(dim(rdata)) == TRUE) { 
#'     n <- 1 
#'   } else { 
#'     n <- dim(rdata)[2]
#'   }        
#'   type <- kernelCharToInt(kernel.type)
#'   if (n == 1) { 
#'     return(.C("kernelEstimator", as.double(rdata), as.double(rdata), as.integer(length(rdata)),
#'               as.integer(kernel.param), as.integer(ifelse(kernel.dofadj, 1, 0)),
#'               as.integer(type), ab = double(kernel.param + 1),
#'               ab2 = double(kernel.param + 1),
#'               ans = double(1), PACKAGE = "highfrequency")$ans)
#'   }
#'   
#'   if (n > 1) {
#'     
#'   }
#'   
#'   
#'   
#'   diagonal <- c()
#'   for (i in 1:n) {
#'     diagonal[i] = TSRV(pdata[[i]], K = K_var[i], J = J_var[i])
#'   }
#'   diag(cov) <- diagonal
#'   
#'   for (i in 2:n) {
#'     for (j in 1:(i - 1)) {
#'       cov[i, j] = cov[j, i] = TSCov_bi(pdata[[i]], 
#'                                        pdata[[j]], K = K_cov, J = J_cov)
#'     }
#'   }
#'   if (cor == FALSE) {
#'     if (makePsd == TRUE) {
#'       cov <- makePsd(cov)
#'     }
#'     return(cov)
#'   }
#'   if (cor == TRUE) {
#'     invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
#'     if (!inherits(invsdmatrix, "try-error")) {
#'       rcor <- invsdmatrix %*% cov %*% invsdmatrix
#'       if (makePsd == TRUE) {
#'         rcor <- makePsd(rcor)
#'       }
#'       return(rcor)
#'     }
#'   }
#' }
#'   # rdata
#'   # 
#'   # if (is.list(rdata) == FALSE) { # In case of only one stock this makes sense
#'   #   if(is.null(dim(rdata))){  
#'   #     n <- 1
#'   #   } else { 
#'   #     n <- dim(rdata)[2] 
#'   #   }
#'   #   if (n == 1) {
#'   #     if (makeReturns) {
#'   #       rdata <- makeReturns(rdata)
#'   #     }
#'   #     result <- rvKernel(rdata, kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
#'   #                        align.by = align.by, align.period = align.period)
#'   #   }
#'   #   if (n >  1){ 
#'   #     stop("Please provide a list with one list-item per stock as input.")  
#'   #   }    
#'   #   return(result)    
#'   #   #stop('The rdata input is not a list. Please provide a list as input for this function. Each list-item should contain the series for one asset.')
#'   # } else {
#'   #   n <- length(rdata);
#'   #   if (n == 1) {
#'   #     if (makeReturns) {
#'   #       rdata <- makeReturns(rdata)
#'   #     }
#'   #     result <- rvKernel(rdata[[1]], cor=cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
#'   #                        align.by = align.by, align.period = align.period, cts = cts)
#'   #     return(result)
#'   #   }
#'   #   
#'   #   if (n > 1) {
#'   #     multixts <- multixts(rdata[[1]])
#'   #     if (multixts == TRUE) { 
#'   #       stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
#'   #     }
#'   #     
#'   #     cov <- matrix(rep(0, n * n), ncol = n)
#'   #     diagonal <- c()  
#'   #     for (i in 1:n) { 
#'   #       if (makeReturns) {
#'   #         rdata[[i]] <- makeReturns(rdata[[i]])
#'   #       }
#'   #       diagonal[i] <- rvKernel(rdata[[i]], cor = cor,kernel.type = kernel.type, kernel.param = kernel.param, kernel.dofadj = kernel.dofadj, 
#'   #                               align.by = align.by, align.period = align.period, cts = cts)   
#'   #     } 
#'   #     diag(cov) <- diagonal
#'   #     
#'   #     for (i in 2:n){
#'   #       for (j in 1:(i - 1)){
#'   #         cov[i, j] <- rcKernel(x = rdata[[i]], y = rdata[[j]], kernel.type = kernel.type, kernel.param = kernel.param, 
#'   #                               kernel.dofadj = kernel.dofadj, align.by = align.by, align.period = align.period,
#'   #                               cts = cts)
#'   #         cov[j, i] <- cov[i, j]
#'   #       }
#'   #     }
#'   #     
#'   #     if(cor == FALSE){
#'   #       cov <- makePsd(cov)
#'   #       return(cov)
#'   #     }
#'   #     if(cor == TRUE){
#'   #       invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
#'   #       if (inherits(invsdmatrix, "try-error") == FALSE) {
#'   #         rcor <- invsdmatrix %*% cov %*% invsdmatrix
#'   #         return(rcor)
#'   #       }
#'   #     }
#'   #   }
#'   # }
#' # }