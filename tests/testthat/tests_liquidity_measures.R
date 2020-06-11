
expect_equal(
  {tqdata <- matchTradesQuotes(sample_tdata, sampleQData)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -833
)

expect_equal(
  {tqdata <- matchTradesQuotes(sample_tdata_microseconds, sampleQData_microseconds)
  directions <- getTradeDirection(tqdata)
  sum(directions)},
  -1403
)

# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sample_tdata, sampleQData)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )