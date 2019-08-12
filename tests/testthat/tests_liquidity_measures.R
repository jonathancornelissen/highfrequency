
expect_equal(
  {tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
    directions <- getTradeDirection(tqdata)
    sum(directions)
  },
  -833
)

expect_equal(
  {tqdata <- matchTradesQuotes(sample_tdata_microseconds, sample_qdata_microseconds)
  directions <- getTradeDirection(tqdata)
  sum(directions)},
  -1403
)

# 
# expect_equal(
#   {tqdata <- matchTradesQuotes(sample_tdata, sample_qdata)
#   getLiquidityMeasures(tqdata, type = "prs")
#   }
#   
#   sum(prs)
# )