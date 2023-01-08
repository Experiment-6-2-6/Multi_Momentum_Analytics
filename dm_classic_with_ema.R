################################################################################
######################## DUAL MOMENTUM  CLASSIC VESSION ########################
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
etf_query <- dbGetQuery(my_postgr, "SELECT * FROM dm_info;")
yahoo_symbols <- as.vector(etf_query[["ticker"]])

# daily quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "daily",
           env = etf_qts_env)
daily_closes <- do.call(merge, eapply(etf_qts_env, Ad))
daily_closes <- na.omit(daily_closes)
names(daily_closes) <- gsub(".Adjusted", "", names(daily_closes))

# weekly quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "weekly",
           env = etf_qts_env)
weekly_closes <- do.call(merge, eapply(etf_qts_env, Ad))
weekly_closes <- na.omit(weekly_closes)
names(weekly_closes) <- gsub(".Adjusted", "", names(weekly_closes))

# monthly quotes
getSymbols(yahoo_symbols, 
           from = start_date, 
           to = end_date, 
           periodicity = "monthly",
           env = etf_qts_env)
monthly_closes <- do.call(merge, eapply(etf_qts_env, Ad))
monthly_closes <- na.omit(monthly_closes)
names(monthly_closes) <- gsub(".Adjusted", "", names(monthly_closes))

# Writing into DB (quotes) -----------------------------------------------------
# Have to transform into PostgreSQL format before sending to DB
dc_for_sql <- as.data.frame(daily_closes)
dc_for_sql <- cbind(date = as.Date(rownames(dc_for_sql)), dc_for_sql)
if (dbExistsTable(my_postgr, "quotes_dm_d")) {
  dbRemoveTable(my_postgr, "quotes_dm_d")
}
dbWriteTable(my_postgr, "quotes_dm_d", dc_for_sql, row.names = FALSE)

wc_for_sql <- as.data.frame(weekly_closes)
wc_for_sql <- cbind(date = as.Date(rownames(wc_for_sql)), wc_for_sql)
if (dbExistsTable(my_postgr, "quotes_dm_w")){
  dbRemoveTable(my_postgr, "quotes_dm_w")
}
dbWriteTable(my_postgr, "quotes_dm_w", wc_for_sql, row.names = FALSE)

mc_for_sql <- as.data.frame(monthly_closes)
mc_for_sql <- cbind(date = as.Date(rownames(mc_for_sql)), mc_for_sql)
if (dbExistsTable(my_postgr, "quotes_dm_m")){
  dbRemoveTable(my_postgr, "quotes_dm_m")
}
dbWriteTable(my_postgr, "quotes_dm_m", mc_for_sql, row.names = FALSE)

# Absolute Momentum Check ------------------------------------------------------
abs_mmt_state_m <- rep(TRUE, ncol(monthly_closes))
names(abs_mmt_state_m) <- names(monthly_closes)
for (ticker in names(monthly_closes)){
  ema7m <- EMA(monthly_closes[,ticker], n = 7)
  if (last(ema7m) < last(monthly_closes[,ticker])) {
    abs_mmt_state_m[[ticker]] <- TRUE
  } else {
      abs_mmt_state_m[[ticker]] <- FALSE
    }
}

abs_mmt_state_w <- rep(TRUE, ncol(weekly_closes))
names(abs_mmt_state_w) <- names(weekly_closes)
for (ticker in names(weekly_closes)){
  ema30w <- EMA(weekly_closes[,ticker], n = 30)
  if (last(ema30w) < last(weekly_closes[,ticker])) {
    abs_mmt_state_w[[ticker]] <- TRUE
  } else {
    abs_mmt_state_w[[ticker]] <- FALSE
  }
}

# Relative Momentum Calculation ------------------------------------------------
daily_1ym <- ROC(daily_closes, n = 252, type = "discrete") %>%  na.omit()
weekly_1ym <- ROC(weekly_closes, n = 50, type = "discrete") %>% na.omit()
monthly_1ym <- ROC(monthly_closes, n = 12, type = "discrete") %>% na.omit()

# Writing into DB (momentum) ---------------------------------------------------
dm_for_sql <- as.data.frame(daily_1ym)
dm_for_sql <- cbind(date = as.Date(rownames(dm_for_sql)), dm_for_sql)
if (dbExistsTable(my_postgr, "momentum_dm_d")) {
  dbRemoveTable(my_postgr, "momentum_dm_d")
}
dbWriteTable(my_postgr, "momentum_dm_d", dm_for_sql, row.names = FALSE)

wm_for_sql <- as.data.frame(weekly_1ym)
wm_for_sql <- cbind(date = as.Date(rownames(wm_for_sql)), wm_for_sql)
if (dbExistsTable(my_postgr, "momentum_dm_w")) {
  dbRemoveTable(my_postgr, "momentum_dm_w")
}
dbWriteTable(my_postgr, "momentum_dm_w", wm_for_sql, row.names = FALSE)

mm_for_sql <- as.data.frame(monthly_1ym)
mm_for_sql <- cbind(date = as.Date(rownames(mm_for_sql)), mm_for_sql)
if (dbExistsTable(my_postgr, "momentum_dm_m")) {
  dbRemoveTable(my_postgr, "momentum_dm_m")
}
dbWriteTable(my_postgr, "momentum_dm_m", mm_for_sql, row.names = FALSE)

# Connection Off ---------------------------------------------------------------
dbDisconnect(my_postgr)

# Leaderboard Table ------------------------------------------------------------
# preparing the monthly table
temp_df_m <- as_tibble(monthly_1ym)
temp_df_m <- temp_df_m[,order(temp_df_m[nrow(temp_df_m),],decreasing = TRUE)]
temp_df_m <- tail(temp_df_m, 1) %>% "*"(100) %>% round(2)

descriptions <- NULL
for (ticker in names(temp_df_m)) {
  description <- etf_query$name[etf_query$ticker == ticker]
  descriptions <- append(descriptions, description)
}

leaderboard_data_m <- tibble(descriptions,
                           names(temp_df_m),
                           abs_mmt_state_m,
                           t(temp_df_m))

names(leaderboard_data_m) <- c("etf_name",
                             "USD", "Above_EMA", "Y_Mm")

leaderboard_table_m <-  
  gt(leaderboard_data_m) %>% 
  tab_header(title = md("**ETFs Sorted By 1 Year Momentum**"), 
             subtitle = "last 2 years of data (monthly)") %>% 
  tab_source_note(md("_datasource: www.yahoo.com_")) %>% 
  tab_style(style = list(cell_text(color = "#196F3D")),
    locations = cells_body(columns = Above_EMA, rows = Above_EMA == TRUE)) %>% 
  tab_style(style = list(cell_text(color = "#7B241C")),
    locations = cells_body(columns = Above_EMA, rows = Above_EMA == FALSE)) %>% 
  tab_style(style = list(cell_fill(color = "#E8F8F5"),
                         cell_text(weight =  "bold")),
    locations = cells_body(rows = 1)) %>% 
  tab_style(style = cell_text(align = "right"),
    locations = cells_source_notes()) %>%
  cols_label(etf_name = "ETF Full Name / Description", 
             USD = "Ticker",
             Above_EMA = "Above EMA", 
             Y_Mm = "1Y Mm %")

# preparing the weekly table
temp_df_w <- as_tibble(weekly_1ym)
temp_df_w <- temp_df_w[,order(temp_df_w[nrow(temp_df_w),],decreasing = TRUE)]
temp_df_w <- tail(temp_df_w, 1) %>% "*"(100) %>% round(2)

descriptions <- NULL
for (ticker in names(temp_df_w)) {
  description <- etf_query$name[etf_query$ticker == ticker]
  descriptions <- append(descriptions, description)
}

leaderboard_data_w <- tibble(descriptions,
                             names(temp_df_w),
                             abs_mmt_state_w,
                             t(temp_df_w))

names(leaderboard_data_m) <- c("etf_name",
                               "USD", "Above_EMA", "Y_Mm")


leaderboard_data_w <- tibble(descriptions,
                           names(temp_df_w),
                           abs_mmt_state_w,
                           t(temp_df_w))

names(leaderboard_data_w) <- c("etf_name",
                             "USD", "Above_EMA", "Y_Mm")

leaderboard_table_w <-  
  gt(leaderboard_data_w) %>% 
  tab_header(title = md("**ETFs Sorted By 1 Year Momentum**"), 
             subtitle = "last 2 years of data (weekly)") %>% 
  tab_source_note(md("_datasource: www.yahoo.com_")) %>% 
  tab_style(style = list(cell_text(color = "#196F3D")),
            locations = cells_body(columns = Above_EMA, 
                                   rows = Above_EMA == TRUE)) %>% 
  tab_style(style = list(cell_text(color = "#7B241C")),
            locations = cells_body(columns = Above_EMA, 
                                   rows = Above_EMA == FALSE)) %>% 
  tab_style(style = list(cell_fill(color = "#E8F8F5"),
                         cell_text(weight =  "bold")),
            locations = cells_body(rows = 1)) %>% 
  tab_style(style = cell_text(align = "right"),
            locations = cells_source_notes()) %>%
  cols_label(etf_name = "ETF Full Name / Description", 
             USD = "Ticker",
             Above_EMA = "Above EMA", 
             Y_Mm = "1Y Mm %")

leaderboard_table_w
leaderboard_table_m

# Plot -------------------------------------------------------------------------
# momentum plot weekly
temp_df_w <- select(wm_for_sql, -date)
temp_df_w <- temp_df_w[,order(temp_df_w[nrow(temp_df_w),],decreasing = TRUE)]
temp_df_w <- select(temp_df_w, 1:3) %>% "*"(100) %>% round(2)
temp_df_w <- tibble(date = wm_for_sql$date, temp_df_w)
temp_df_w <- tail(temp_df_w, 100)
plot_data_w <- melt(temp_df_w, id = "date")

best_3_plot_w <- ggplot(plot_data_w, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "ETFs by the best 1 Year Momentum",
       subtitle = "last 2 years of data (weekly)",
       caption = "datasource: www.yahoo.com",
       x = NULL,
       y = "Momentum %",
       colour = "ETF") + 
  theme_minimal() +
  theme(plot.title = element_text(colour = "#3498DB", face = "bold"), 
        plot.subtitle = element_text(colour = "#555555", face = "bold"), 
        plot.caption = element_text(face = "italic"), 
        aspect.ratio = 9/16)

# momentum plot monthly
temp_df_m <- select(mm_for_sql, -date)
temp_df_m <- temp_df_m[,order(temp_df_m[nrow(temp_df_m),],decreasing = TRUE)]
temp_df_m <- select(temp_df_m, 1:3) %>% "*"(100) %>% round(2)
temp_df_m <- tibble(date = mm_for_sql$date, temp_df_m)
temp_df_m <- tail(temp_df_m, 24)
plot_data_m <- melt(temp_df_m, id = "date")

best_3_plot_m <- ggplot(plot_data_m, aes(x = date, y = value, colour = variable)) +
  geom_line() +
  labs(title = "ETFs by the best 1 Year Momentum",
       subtitle = "last 2 years of data (monthly)",
       caption = "datasource: www.yahoo.com",
       x = NULL,
       y = "Momentum %",
       colour = "ETF") + 
  theme_minimal() +
  theme(plot.title = element_text(colour = "#3498DB", face = "bold"), 
        plot.subtitle = element_text(colour = "#555555", face = "bold"), 
        plot.caption = element_text(face = "italic"), 
        aspect.ratio = 9/16)

best_3_plot_w
best_3_plot_m

# CSV Export -------------------------------------------------------------------
write.csv(leaderboard_data_w, "dm_classic_weekly.csv")
write.csv(leaderboard_data_m, "dm_classic_monthly.csv")
write.csv(temp_df_w, "dm_classic_2_years_of_mmt_data_weekly.csv")
write.csv(temp_df_m, "dm_classic_2_years_of_mmt_data_monthly.csv")

################################################################################
################################### THE  END ###################################
################################################################################
