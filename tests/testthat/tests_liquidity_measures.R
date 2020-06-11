
expect_equal(
  {tqdata <- matchTradesQuotes(sampletData, sampleQData)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -833
)

expect_equal(
  {tqdata <- matchTradesQuotes(sampletDataMicroseconds, sampleQDataMicroseconds)
  directions <- getTradeDirection(tqdata)
  sum(directions)},
  -1403
)

# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sampletData, sampleQData)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )