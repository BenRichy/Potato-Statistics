#global.R


#load in packages
library(shiny)
library(dplyr)
library(readr)


#read in data... haha nice pun ;)
raw_potato_data <- read_csv("app/data/Potato Ranking Form.csv") |> 
  pivot_longer(names_to = )
