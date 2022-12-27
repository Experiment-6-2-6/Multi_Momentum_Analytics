# Packages Installation --------------------------------------------------------
install.packages("quantmod")
install.packages("tidyverse")
install.packages("RPostgres")
install.packages("knitr")

# Libraries -------------------------------------------------------------
library(quantmod)
library(tidyverse)
library(dplyr)
library(RPostgres)
library(ggplot2)
library(knitr)

# Establishing DB connection ---------------------------------------------------
my_postgr <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("sql_host"),
  dbname = Sys.getenv("sql_db_name"),
  user = Sys.getenv("sql_user"),
  password = Sys.getenv("sql_password"),
  port = 5432,
  sslmode = "require"
)

# Getting Raw Data -------------------------------------------------------------
start_date <- Sys.Date() - (3 * 365)
end_date <- Sys.Date()
etf_qts_env <- new.env()

etf_query <- dbGetQuery(my_postgr, "SELECT * FROM etf_info;")
# Getting the proper set of ETFs
assets_names <- as.vector(etf_query[["ticker_usd"]]) # ticker_gbp is available
yahoo_symbols <- as.vector(etf_query[["symbol_usd"]])# in my DB but there is no 
# real difference in performance they are only quoted in different currencies
getSymbols(yahoo_symbols,  from = start_date, to = Sys.Date(), env = etfs_quotes)
daily_closes <- do.call(merge, eapply(etfs_quotes, Ad))
from_web <- paste0("https://stooq.com/q/d/l/?s=xauusd&d1=", 
                  format(start_date, "%Y%m%d"),
                  "&d2=", 
                  format(end_date, "%Y%m%d"),
                  "&i=d")
gold_qts <- read_csv(from_web)
gold_qts <- xts(gold_tmp$Close, order.by = gold_tmp$Date)
daily_closes <- merge(daily_closes, gold_qts)
daily_closes <- na.omit(daily_closes)
assets_names <- c(assets_names,"IGLN")
names(daily_closes) <- assets_names
temp_df$FTSE <- NULL # FTSE index no longer needed (dates synch done)

# Writing the DB (quotes) ------------------------------------------------------
# Transforming quotes into PostgreSQL format before sending to DB
df_for_sql <- as.data.frame(temp_df)
df_for_sql <- cbind(date = as.Date(rownames(df_for_sql)), df_for_sql)
if (dbExistsTable(my_postgr, "quotes_usd")) {
  dbGetQuery(my_postgr, "DROP TABLE quotes_usd;")
} else {
  dbWriteTable(my_postgr, "quotes_usd", df_for_sql, row.names = FALSE)
}

# Absolute Momentum Check ------------------------------------------------------
# I will need it to validate the leaderboard candidates
price_above_ema <- rep(TRUE, ncol(temp_df))
names(price_above_ema) <- assets_names
if (nrow(temp_df[1]) < 30 ){
  data_is_too_short <- TRUE
} else {
  data_is_too_short <- FALSE
  for (ticker in assets_names){
    weekly_data <- Cl(to.weekly(temp_df[[ticker]]))
    ema_30w <- EMA(weekly_data, n = 30)
    price_above_ema[[ticker]] <- if (ema_30w < last(weekly_data)) TRUE else FALSE
  }
}

# Relative Momentum Calculation ------------------------------------------------
# I have chosen the simple Rate Of Change calculation
# because I want it to be between -100% and +100% and not -∞ to +∞
# ROC "descrete" = simple (aritmethic) return,
# ROC "continous" = logarithmic (geometric) method
daily_1MM <- map(temp_df, ROC, n = 21, type = "discrete") # <<TO DO>>


daily_1YM_merged <- ROC(temp_df[[1]], n = 252, type = "discrete")
weekly_1YM_merged <- to.weekly(temp_df[[1]], indexAt = "endof") %>% 
  Cl() %>% 
  ROC(., n = 50, type = "discrete") %>% 
  "*"(100) %>% 
  round(., 2)
for (i in seq_along(assets_names)){
  daily_1YM <- ROC(temp_df[[i]], 252)
  
  weekly_1YM <- to.weekly(temp_df[,i], indexAt = "endof") %>% 
    Cl() %>% 
    ROC(., n = 50, type = "discrete") %>% 
    "*"(100) %>% 
    round(., 2)
  
  daily_1YM_merged <- merge(daily_1YM_merged, daily_1YM)
  weekly_1MM_merged <- merge(weekly_1MM_merged, weekly_1MM)
  weekly_1YM_merged <- merge(weekly_1YM_merged, weekly_1YM)
}

daily_1YM_merged <- na.omit(daily_1YM_merged)
weekly_1YM_merged <- na.omit(weekly_1YM_merged)

names(daily_1YM_merged) <- assets_names
names(weekly_1YM_merged) <- assets_names

daily_1YM_merged <- as.data.frame(daily_1YM_merged)
weekly_1YM_merged <- as.data.frame(weekly_1YM_merged)

# Writting the Calculations to DB ----------------------------------------------
df_ym_d_for_sql <- data.frame(Date = as.Date(rownames(daily_1YM_merged)), daily_1YM_merged)
df_ym_w_for_sql <- data.frame(Date = as.Date(rownames(weekly_1YM_merged)), weekly_1YM_merged)

df_ym_w_for_sql <- tail(df_ym_w_for_sql, 50)

if (dbExistsTable(my_postgr, "ym_daily")) {
  dbGetQuery(my_postgr, "DROP TABLE ym_daily;")
} else dbWriteTable(my_postgr, "ym_daily", df_ym_d_for_sql, row.names = FALSE)

if (dbExistsTable(my_postgr, "ym_weekly")) {
  dbGetQuery(my_postgr, "DROP TABLE ym_weekly;")
} else dbWriteTable(my_postgr, "ym_weekly", df_ym_w_for_sql, row.names = FALSE)

# Terminatin the DB my_postgr
dbDisconnect(my_postgr)