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

test_taq <- fread("unzip -p taqdata/CAT/CATjan2018.zip")
test_taq_quotes <- fread("unzip -p taqdata/CAT/CATjan2018quotes.zip")

blub2 <- test_taq[DATE %in% c(20180102, 20180103)][, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")]
blub <- test_taq_quotes[DATE %in% c(20180102, 20180103)][, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")]

test_taq_nyse <- test_taq[PRICE != 0 & EX == "N"]
test_taq_nyse_quotes <- test_taq_quotes[BID != 0 & ASK != 0 & EX == "N"][ ,
  DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")]

test_taq_nyse[, DT := as_date(DATE)] %>% print()

save(blub, file = "data/sample_qdataraw_microseconds.rda")

# blub2 <- test_taq_nyse[, DT := paste(as_datetime(as.character(DATE), format = "%Y%m%d"), TIME_M, sep = "T")]

quotesCleanup(blub)

setcolorder(blub2, c("DT", "EX", "SYM_ROOT", "SYM_SUFFIX", "TR_SCOND", "SIZE", "PRICE", "TR_CORR", "TR_SEQNUM", "TR_SOURCE", "TR_RF"))

setcolorder(blub, c("DT", "EX", "BID", "BIDSIZ", "ASK", "ASKSIZ", "QU_COND", "QU_SEQNUM", "NATBBO_IND","QU_CANCEL", "QU_SOURCE", "SYM_ROOT", "SYM_SUFFIX"))

save

quotesCleanup(qdataraw = blub, exchanges = "N")


blub2 <- test_taq_nyse[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")]

blub2[, lapply(.SD, median), by = .(DT, SYM_ROOT), .SDcols = c("PRICE")]

# blub2[, .(median(PRICE)), .keyby = .(DT, SYM_ROOT)]

dt_sample_qdataraw <- setnames(as.data.table(sample_qdataraw), old = "index", new = "DT")

microbenchmark::microbenchmark(noZeroQuotes(dt_sample_qdataraw), times = 10, unit = "s")

microbenchmark::microbenchmark(xts_cleaned <- {tdata_afterfirstcleaning <- highfrequency::tradesCleanup(tdataraw = sample_tdataraw, exchanges = list("N"))}$tdata, times = 10, unit = "s")

microbenchmark::microbenchmark({dt_cleaned <- dt_sample_tdataraw[PRICE != 0 & EX == "N" & COND %in% c("E", "F")][,  lapply(.SD, median), by = .(index, SYMBOL), .SDcols = c("PRICE")]}, times = 10, unit = "s")

microbenchmark::microbenchmark(rmOutliersTrades(sample_tdata, sample_qdata), times = 10, unit = "s")


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

list.files("taqdata")
# 
# microbenchmark::microbenchmark(xts_old <- rmOutliers(qdata = sample_qdataraw), times = 10, unit = "s")
# microbenchmark::microbenchmark(xts_new <- rmOutliersDataTable(qdata = setnames(as.data.table(sample_qdataraw)[, BID := as.numeric(as.character(BID))][, OFR := as.numeric(as.character(OFR))], old = "index", new = "DT")), times = 10, unit = "s")
# 
# xts_old <- rmOutliers(qdata = sample_qdataraw)
# xts_new <- rmOutliersDataTable(qdata = sample_qdataraw)


unique(sapply(list.files("taqdata"), FUN = function(x) substring(x, nchar(x) - 2, nchar(x)))) == "zip"

list.files("taqdata")

for (ii in list.files("taqdata", recursive = TRUE)) {
  print(ii)
  df_test <- fread(cmd = paste0("unzip -p ", "taqdata/", ii))
  pryr::object_size(df_test)
  df_test <- df_test[PRICE != 0 & EX == "N" & TR_SCOND %in% c("E", "F")]
  
  df_test <- df_test[, DT := as.POSIXct(substring(paste(as.character(DATE), TIME_M, sep = " "), 1, 20), tz = "EST", format = "%Y%m%d %H:%M:%OS")][
    , lapply(.SD, median), by = .(DT, SYMBOL), .SDcols = c("PRICE")]
  
  df_test[PRICE != 0 & EX == "N" & TR_SCOND %in% c("E", "F")]
}

