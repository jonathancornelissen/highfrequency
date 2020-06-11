expect_equal(
  formatC(sum(sum(HARmodel(makeReturns(sample_5minprices_jumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                           RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")$coefficients)), 
          digits = 5),
  "7.6406"
)

expect_identical(
  {blub <- HARmodel(makeReturns(sample_5minprices_jumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                    RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")
  blub2 <- plot(blub)
  blub2$get_xlim()},
  c(1263916800, 1264780800)
)

expect_identical(
  {blub <- HARmodel(makeReturns(sample_5minprices_jumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                    RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")
  blub2 <- plot(blub)
  blub2$get_xlim()},
  c(1263916800, 1264780800)
)

# expect_identical(
#   {data("realized_library")
#     returns <-  realized_library$open_to_close
#     bv      <-  realized_library$bv
#     returns <- returns[!is.na(bv)]  
#     bv <- bv[!is.na(bv)] # Remove NA's 
#     data <- cbind( returns^2, bv) # Make data matrix with returns and realized measures
#     backcast <- matrix(c(var(returns), mean(bv)), ncol = 1)
#     
#     #For traditional (default) version:
#     startvalues <- c(0.004,0.02,0.44,0.41,0.74,0.56) # Initial values
#     output <- HEAVYmodel(data = as.matrix(data,ncol=2), compconst=FALSE, startingvalues = startvalues, backcast=backcast) 
#     formatC(sum(output$parameters), digits = 5)},
#   "2.2092"
# )

