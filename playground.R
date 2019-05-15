library(magrittr)
library(pryr)
library(data.table)
library(lubridate)

test_df <- sample_5minprices %>% as.data.table()

# test_df[, `:=`(date = floor_date(index, "days"))]

cols_for_returns <- setdiff(names(test_df), c("index", "date"))


test_df[, paste0(cols_for_returns, "_return") := lapply(.SD, function(x) (log(x) - log(shift(x, n = 1, fill = NA, type = "lag")))), .SDcols = cols_for_returns]

# test_df[, `:=`(out_cols_not = (log(stock1) - log(shift(stock1, n = 1, fill = NA, type = "lag"))))]

test_xts <- test_df %>% xts::as.xts()

test_xts

object_size(test_df)
object_size(test_xts)

object_size(tibble::as_tibble(test_df))

test_df[, low_freq := ceiling_date(index, "15 mins")]

test_df[, lapply(.SD, mean), by = "low_freq"]
