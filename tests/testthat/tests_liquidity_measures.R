
expect_equal(
  {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -833
)

expect_equal(
  {tqdata <- matchTradesQuotes(sampleTData_microseconds, sampleQDataMicroseconds)
  directions <- getTradeDirection(tqdata)
  sum(directions)},
  -1403
)

# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sampleTData, sampleQData)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )