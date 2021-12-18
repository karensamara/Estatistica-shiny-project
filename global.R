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


master_df <- read.csv('dataset_tk.csv')
states_list <- c(names(master_df))
states_list <- states_list[-1]

master_df$X <- strptime(master_df$X, format="%d/%m/%Y %H:%M:%S")

datelist <- c(master_df$X)
