# Packages ---------------------------------------------------------------------
install.packages("quantmod")
install.packages("tidyverse")
install.packages("RPostgres")
install.packages("knitr")

# Libraries --------------------------------------------------------------------
library(quantmod)
library(tidyverse)
library(dplyr)
library(RPostgres)
library(knitr)
library(ggplot2)

# Postgres DB connection -------------------------------------------------------
my_postgr <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("sql_host"),
  dbname = Sys.getenv("sql_db_name"),
  user = Sys.getenv("sql_user"),
  password = Sys.getenv("sql_password"),
  port = 5432,
  sslmode = "require"
)

# Getting the Data -------------------------------------------------------------
start_date <- Sys.Date() - (3 * 365)
end_date <- Sys.Date()
etf_qts_env <- new.env()

etf_query <- dbGetQuery(my_postgr, "SELECT * FROM etf_info;")
# etfs_tickers <- as.vector(etf_query[["ticker_usd"]])
yahoo_symbols <- as.vector(etf_query[["symbol_usd"]])
# etfs_names <- as.vector(etf_query[["full_name"]])

# daily quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "daily",
           env = etf_qts_env)
daily_closes <- do.call(merge, eapply(etf_qts_env, Ad))
daily_closes <- na.omit(daily_closes)
names(daily_closes) <- gsub(".L.Adjusted", "", names(daily_closes))

# weekly quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "weekly",
           env = etf_qts_env)
weekly_closes <- do.call(merge, eapply(etf_qts_env, Ad))
weekly_closes <- na.omit(weekly_closes)
names(weekly_closes) <- gsub(".L.Adjusted", "", names(weekly_closes))

# Writing into DB (quotes) -----------------------------------------------------
# Have to transform into PostgreSQL format before sending to DB
df_for_sql <- as.data.frame(daily_closes)
df_for_sql <- cbind(date = as.Date(rownames(df_for_sql)), df_for_sql)
if (dbExistsTable(my_postgr, "quotes_daily")) {
  dbRemoveTable(my_postgr, "quotes_daily")
}
dbWriteTable(my_postgr, "quotes_daily", df_for_sql, row.names = FALSE)

df_for_sql <- as.data.frame(weekly_closes)
df_for_sql <- cbind(date = as.Date(rownames(df_for_sql)), df_for_sql)
if (dbExistsTable(my_postgr, "quotes_weekly")){
  dbRemoveTable(my_postgr, "quotes_weekly")
}
dbWriteTable(my_postgr, "quotes_weekly", df_for_sql, row.names = FALSE)

# Absolute Momentum Check ------------------------------------------------------
positive_momentum <- rep(TRUE, ncol(weekly_closes))
names(positive_momentum) <- names(weekly_closes)
for (ticker in names(weekly_closes)){
  ema30w <- EMA(weekly_closes[,ticker], n = 30)
  if (last(ema30w) < last(weekly_closes[,ticker])) {
    positive_momentum[[ticker]] <- TRUE
  } else {
      positive_momentum[[ticker]] <- FALSE
    }
}

# Relative Momentum Calculation ------------------------------------------------
daily_1ym <- ROC(daily_closes, n = 252, type = "discrete") %>%  
              na.omit() %>% "*"(100) %>% round(2)
weekly_1ym <- ROC(weekly_closes, n = 50, type = "discrete") %>%  
                na.omit() %>% "*"(100) %>% round(2)

# Writing into DB (momentum) ---------------------------------------------------
df_for_sql <- as.data.frame(daily_1ym)
df_for_sql <- cbind(date = as.Date(rownames(df_for_sql)), df_for_sql)
if (dbExistsTable(my_postgr, "momentum_d")) {
  dbRemoveTable(my_postgr, "momentum_d")
}
dbWriteTable(my_postgr, "momentum_d", df_for_sql, row.names = FALSE)

df_for_sql <- as.data.frame(weekly_1ym)
df_for_sql <- cbind(date = as.Date(rownames(df_for_sql)), df_for_sql)
if (dbExistsTable(my_postgr, "momentum_w")) {
  dbRemoveTable(my_postgr, "momentum_w")
}
dbWriteTable(my_postgr, "momentum_w", df_for_sql, row.names = FALSE)

# Connection Off ---------------------------------------------------------------
dbDisconnect(my_postgr)

# Leaderboard Table ------------------------------------------------------------
temp_df <- as_tibble(weekly_1ym)
temp_df <- temp_df[,order(temp_df[nrow(temp_df),],decreasing = TRUE)]
temp_df <- tail(temp_df, 1)

descriptions <- NULL
for (ticker in names(temp_df)) {
  description <- etf_query$full_name[etf_query$ticker_usd == ticker]
  descriptions <- append(descriptions, description)
}

gbx_tickers <- NULL
for (ticker in names(temp_df)) {
  gbx_ticker <- etf_query$ticker_gbp[etf_query$ticker_usd == ticker]
  gbx_tickers <- append(gbx_tickers, gbx_ticker)
}


leaderboard_data <- tibble(descriptions,
                           names(temp_df),
                           gbx_tickers,
                           positive_momentum,
                           t(temp_df))

names(leaderboard_data) <- c("ETF Full Name / Description",
                             "USD", "GBP", "Positive Mm.", "1Y Mm. %")

knitr::kable(leaderboard_data, 
             "pipe", 
             align = c("l", "c", "c", "c", "r"),
             caption = "ETFs Leaderboard (In Descending Order)")

# Plot -------------------------------------------------------------------------
plot_data <- tibble(date = df_for_sql$date, 
                    select(df_for_sql, 
                           leaderboard_data[[1, "USD"]], 
                           leaderboard_data[[2, "USD"]], 
                           leaderboard_data[[3, "USD"]], 
                           leaderboard_data[[4, "USD"]], 
                           leaderboard_data[[5, "USD"]]))

