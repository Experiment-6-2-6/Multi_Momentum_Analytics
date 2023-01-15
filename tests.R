# Preparing DF for performance comparison module
# Let it be in a long format
# ticker, date, price, mmt value, mmt T/F

library(quantmod)
library(tidyverse)
library(dplyr)
library(RPostgres)
library(gt)

