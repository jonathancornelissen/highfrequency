# Mixed-frequency financial data frame -------------------------
rm(list= ls()[])
library(dplyr, warn.conflicts = FALSE)

# Download realized measures of volatility -----------------------
download.file("https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip",
              destfile = "data-raw/OxfordManRealizedVolatilityIndices.zip")
system("unzip -o data-raw/OxfordManRealizedVolatilityIndices.zip -d data-raw/")

realized_library <-
  read_csv("data-raw/oxfordmanrealizedvolatilityindices.csv") %>%
  filter(Symbol == ".SPX") %>%
  mutate(X = as.Date(substring(X, 1, 10))) %>%
  rename(date = X,
         symbol = Symbol) %>%
  mutate(symbol = as.character(symbol))

save(realized_library, file = "data/realized_library.rda")
