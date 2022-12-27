etfs_quotes <- new.env()
start_date <- Sys.Date() - (3 * 365)
end_date <- Sys.Date()
options(timeout = 120)

connection <- dbConnect(
  RPostgres::Postgres(),
  host = "db.bit.io",
  dbname = "Kristoff/ETFs",
  user = "Kristoff",
  password = "v2_3wCAp_AX6Lg5QT2mjXebQ2gywPKWD",
  port = 5432,
  sslmode = "require"
)

etf_query <- dbGetQuery(connection, "SELECT * FROM etf_info;")
assets_names <- as.vector(etf_query[["ticker_usd"]])
yahoo_symbols <- as.vector(etf_query[["symbol_usd"]])

yahoo_symbols <- c("^FTSE","AGES.L","BCHS.L","BCOG.L","CARP.L","CHRG.L","CNX1.L","CYBP.L","DAGB.L","DGIT.L","DRDR.L","EDOG.L","FOGB.L","GBDV.L","GILG.L","HDGB.L","HERG.L","IEUX.L","IH2O.L","IIND.L","INFR.L","INRG.L","INTL.L","ISFR.L","IWDP.L","IWFM.L","IWFQ.L","IWFV.L","KLWD.L","LTAM.L","MCHS.L","MINV.L","PBUY.L","PHSP.L","RBTX.L","REGB.L","RTWP.L","SHLG.L","SMGB.L","SPOG.L","SSHY.L","UFOP.L","VAGP.L","VFEM.L","VJPN.L","VUKE.L","VUSA.L","VWRL.L","WLDS.L","WOOD.L")
getSymbols(yahoo_symbols,  from = start_date, to = Sys.Date(), env = etfs_quotes)

daily_closes <- do.call(merge, eapply(etfs_quotes, Ad))
head(daily_closes)
tail(daily_closes)

options(sql_host = "db.bit.io")
options(sql_db_name = "Kristoff/ETFs")
options(sql_user = "Kristoff")
options(sql_password = "v2_3wCAp_AX6Lg5QT2mjXebQ2gywPKWD")
options(sql_port = 5432)

