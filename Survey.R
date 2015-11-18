## Survey Project in R

#install and load library
install.packages("dplyr")
install.packages ("ggplot2")
library(dplyr)
library(ggplot2)

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")  # data for project is in the portal_data_joined.csv

# import that file
surveys <- read.csv('data/portal_data_joined.csv')

## explore distribution of weight for each species

#data parsing

#build figure

## look at composition of data in sampling

## explore comparisons between variables
