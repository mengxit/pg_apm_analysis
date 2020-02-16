
#load libraries

library(readxl)
library(ggplot2)
library(tidyverse)

#data loading

user_sessions <- read_xlsx("dataset.xlsx", na = c("", "NA"))

ingame_purchase <- read_xlsx("dataset.xlsx", sheet = 2, na = c("", "NA"))
