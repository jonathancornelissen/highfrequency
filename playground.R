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

?highfrequency::tradesCleanup

test_taq <- fread("unzip -p taqdata/CATjan2018.zip")

test_taq_nyse <- test_taq[PRICE != 0 & EX == "N"]

test_taq_nyse[, DT := as_date(DA)] %>% print()

blub2 <- test_taq_nyse[, DT := paste(as_datetime(as.character(DATE), format = "%Y%m%d"), TIME_M, sep = "T")]



blub2 <- test_taq_nyse[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")]

blub2[, lapply(.SD, median), by = .(DT, SYM_ROOT), .SDcols = c("PRICE")]

# blub2[, .(median(PRICE)), .keyby = .(DT, SYM_ROOT)]


# Check speed!

data("sample_tdataraw")
data("sample_qdata")

dt_sample_tdataraw <- as.data.table(sample_tdataraw)[, PRICE := as.numeric(as.character(PRICE))]

microbenchmark::microbenchmark(xts_cleaned <- {tdata_afterfirstcleaning <- highfrequency::tradesCleanup(tdataraw = sample_tdataraw, exchanges = list("N"))}$tdata, times = 10, unit = "s")

microbenchmark::microbenchmark({dt_cleaned <- dt_sample_tdataraw[PRICE != 0 & EX == "N" & COND %in% c("E", "F")][,  lapply(.SD, median), by = .(index, SYMBOL), .SDcols = c("PRICE")]}, times = 10, unit = "s")

xts_cleaned
dt_cleaned  

highfrequency::rmTradeOutliers(xts_cleaned, sample_qdata)

data("sample_tdataraw")
data("sample_tdata")
data("sample_qdata")

highfrequency::rmOutliers(sample_qdataraw)


# 
# microbenchmark::microbenchmark(xts_old <- rmOutliers(qdata = sample_qdataraw), times = 10, unit = "s")
# microbenchmark::microbenchmark(xts_new <- rmOutliersDataTable(qdata = setnames(as.data.table(sample_qdataraw)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")), times = 10, unit = "s")
# 
# xts_old <- rmOutliers(qdata = sample_qdataraw)
# xts_new <- rmOutliersDataTable(qdata = sample_qdataraw)


