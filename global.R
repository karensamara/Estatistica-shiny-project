library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)
library(hrbrthemes)
library(viridis)


##master_df <- read.csv('df_stocks.csv')
##stock_list <- c('AAPL', 'FB', 'GOOG')
master_df <- read.csv('dataset_tk.csv')
states_list <- c(names(master_df))
states_list <- states_list[-1]
print(states_list)
##print(stock_list)

##datelist <- c(master_df$X)
##print(datelist)



##master_df$X <- NULL

##master_df <- master_df %>% drop_na()
master_df$X <- strptime(master_df$X, format="%d/%m/%Y %H:%M:%S")
##master_df$X <- format(as.POSIXct(master_df$X,format='%d/%m/%Y %H:%M:%S'),format='%Y/%m/%d')

datelist <- c(master_df$X)
print(datelist)