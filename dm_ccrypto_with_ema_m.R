################################################################################
################################### PART ONE ################################### 
######################### DUAL MOMENTUM CRYPTO VESSION #########################
############### ABSOLUTE MOMENTUM  MEASURED BY EMA(30) REFERENCE ###############
################################ MONTHLY  CHECK ################################
################################################################################

# Libraries --------------------------------------------------------------------
library(quantmod)
library(tidyverse)
library(dplyr)
library(RPostgres)
library(gt)
library(reshape2)
library(ggplot2)
library(googlesheets4)

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
etf_query <- dbGetQuery(my_postgr, "SELECT * FROM crypto_info;")
yahoo_symbols <- as.vector(etf_query[["ticker"]])

# daily quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "daily",
           env = etf_qts_env)
daily_closes <- do.call(merge, eapply(etf_qts_env, Ad))
daily_closes <- na.omit(daily_closes)
names(daily_closes) <- gsub("-USD.Adjusted", "", names(daily_closes))

# weekly quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "weekly",
           env = etf_qts_env)
weekly_closes <- do.call(merge, eapply(etf_qts_env, Ad))
weekly_closes <- na.omit(weekly_closes)
names(weekly_closes) <- gsub("-USD.Adjusted", "", names(weekly_closes))

# monthly quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "monthly",
           env = etf_qts_env)
monthly_closes <- do.call(merge, eapply(etf_qts_env, Ad))
monthly_closes <- na.omit(monthly_closes)
names(monthly_closes) <- gsub("-USD.Adjusted", "", names(monthly_closes))

# Writing into DB (quotes) -----------------------------------------------------
# Have to transform into PostgreSQL format before sending to DB
dc_for_sql <- as.data.frame(daily_closes)
dc_for_sql <- cbind(date = as.Date(rownames(dc_for_sql)), dc_for_sql)
if (dbExistsTable(my_postgr, "quotes_cr_d")) {
  dbRemoveTable(my_postgr, "quotes_cr_d")
}
dbWriteTable(my_postgr, "quotes_cr_d", dc_for_sql, row.names = FALSE)

wc_for_sql <- as.data.frame(weekly_closes)
wc_for_sql <- cbind(date = as.Date(rownames(wc_for_sql)), wc_for_sql)
if (dbExistsTable(my_postgr, "quotes_cr_w")){
  dbRemoveTable(my_postgr, "quotes_cr_w")
}
dbWriteTable(my_postgr, "quotes_cr_w", wc_for_sql, row.names = FALSE)

mc_for_sql <- as.data.frame(monthly_closes)
mc_for_sql <- cbind(date = as.Date(rownames(mc_for_sql)), mc_for_sql)
if (dbExistsTable(my_postgr, "quotes_cr_m")){
  dbRemoveTable(my_postgr, "quotes_cr_m")
}
dbWriteTable(my_postgr, "quotes_cr_m", mc_for_sql, row.names = FALSE)

# Absolute Momentum Check ------------------------------------------------------
mm_direction <- rep(TRUE, ncol(weekly_closes))
names(mm_direction) <- names(weekly_closes)
for (ticker in names(weekly_closes)){
  ema30w <- EMA(weekly_closes[,ticker], n = 30)
  if (last(ema30w) < last(weekly_closes[,ticker])) {
    mm_direction[[ticker]] <- TRUE
  } else {
      mm_direction[[ticker]] <- FALSE
    }
}

# Relative Momentum Calculation ------------------------------------------------
daily_1ym <- ROC(daily_closes, n = 252, type = "discrete") %>%  na.omit()
weekly_1ym <- ROC(weekly_closes, n = 50, type = "discrete") %>% na.omit()
monthly_1ym <- ROC(monthly_closes, n = 12, type = "discrete") %>% na.omit()

# Writing into DB (momentum) ---------------------------------------------------
dm_for_sql <- as.data.frame(daily_1ym)
dm_for_sql <- cbind(date = as.Date(rownames(dm_for_sql)), dm_for_sql)
if (dbExistsTable(my_postgr, "momentum_cr_d")) {
  dbRemoveTable(my_postgr, "momentum_cr_d")
}
dbWriteTable(my_postgr, "momentum_cr_d", dm_for_sql, row.names = FALSE)

wm_for_sql <- as.data.frame(weekly_1ym)
wm_for_sql <- cbind(date = as.Date(rownames(wm_for_sql)), wm_for_sql)
if (dbExistsTable(my_postgr, "momentum_cr_w")) {
  dbRemoveTable(my_postgr, "momentum_cr_w")
}
dbWriteTable(my_postgr, "momentum_cr_w", wm_for_sql, row.names = FALSE)

mm_for_sql <- as.data.frame(monthly_1ym)
mm_for_sql <- cbind(date = as.Date(rownames(mm_for_sql)), mm_for_sql)
if (dbExistsTable(my_postgr, "momentum_cr_m")) {
  dbRemoveTable(my_postgr, "momentum_cr_m")
}
dbWriteTable(my_postgr, "momentum_cr_m", mm_for_sql, row.names = FALSE)

# Connection Off ---------------------------------------------------------------
dbDisconnect(my_postgr)

# Leaderboard Table ------------------------------------------------------------
temp_df <- as_tibble(monthly_1ym)
temp_df <- temp_df[,order(temp_df[nrow(temp_df),],decreasing = TRUE)]
temp_df <- tail(temp_df, 1)

descriptions <- NULL
for (ticker in names(temp_df)) {
  description <- etf_query$name[etf_query$ticker == paste0(ticker,  "-USD")]
  descriptions <- append(descriptions, description)
}

leaderboard_data <- tibble(descriptions,
                           names(temp_df),
                           mm_direction,
                           t(temp_df))

names(leaderboard_data) <- c("Crypto", "Ticker", "Above_EMA", "Y_Mm")

leaderboard_table <-  
  gt(leaderboard_data) %>% 
  tab_header(title = md("**Cryptos Sorted By 1 Year Momentum**"), 
             subtitle = "last 2 years of data") %>% 
  tab_source_note(md("*datasource: CoinMarketCap via Yahoo.com*")) %>% 
  tab_style(style = list(cell_text(color = "#196F3D")),
    locations = cells_body(columns = Above_EMA, rows = Above_EMA == TRUE)) %>% 
  tab_style(style = list(cell_text(color = "#7B241C")),
    locations = cells_body(columns = Above_EMA, rows = Above_EMA == FALSE)) %>% 
  tab_style(style = list(cell_fill(color = "#E8F8F5"),
                         cell_text(weight =  "bold")),
    locations = cells_body(rows = 1)) %>% 
  tab_style(style = cell_text(align = "right"),
    locations = cells_source_notes()) %>%
  cols_label(etf_name = "Tocken Full Name", 
             USD = "Ticker",
             Above_EMA = "Above EMA", 
             Y_Mm = "1Y Mm %")
  
leaderboard_table

# Plot -------------------------------------------------------------------------
temp_df <- select(wm_for_sql, -date)
temp_df <- temp_df[,order(temp_df[nrow(temp_df),],decreasing = TRUE)]
temp_df <- select(temp_df, 1:3)
temp_df <- tibble(date = wm_for_sql$date, temp_df)
temp_df <- tail(temp_df, 24)
plot_data <- melt(temp_df, id = "date")

best_3_plot <- ggplot(plot_data, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "Top 3 Tockens With Best 1 Year Momentum",
       subtitle = "last 2 years of data",
       caption = "datasource: CoinMarketCap via Yahoo.com",
       x = NULL,
       y = "Momentum %",
       colour = "Coin") + 
  theme_minimal() +
  theme(plot.title = element_text(colour = "#3498DB", face = "bold"), 
        plot.subtitle = element_text(colour = "#555555", face = "bold"), 
        plot.caption = element_text(face = "italic"), 
        aspect.ratio = 9/16)

best_3_plot

# CSV Export -------------------------------------------------------------------
write.csv(leaderboard_data, "cr_classic_leaderboard.csv")
write.csv(temp_df, "cr_classic_all_2_years_data.csv")

# Google Sheet Export ----------------------------------------------------------
gs4_auth()
my_gsheets <- gs4_get(Sys.getenv("file_id"))

df_to_write <- leaderboard_data
range_write(
  my_gsheets,
  df_to_write,
  sheet = 1,
  range = "one_year_momentum!A2:Z11",
  col_names = FALSE,
  reformat = TRUE
)

df_to_write <- wc_for_sql
range_write(
  my_gsheets,
  df_to_write,
  sheet = "chart_data",
  range = NULL,
  col_names = TRUE,
  reformat = TRUE
)

################################################################################
############################### END OF  PART ONE ###############################
################################################################################
