#######################################################################################################################
#This file demonstrates how to perform basic data preparation and merging
#######################################################################################################################

## 0. Setup ----

setwd('C:/DAWG/R training')

#Load Road Fatality data
deaths <- read.csv("bitre_ardd_fatalities_dec_2018.csv")
str(deaths)
head(deaths)


## 1 Some basic data manipulation ----

# 1.1 filtering by rows ----

# rows 1 - 10
deaths[1:10, ]

# subsetting to Friday crashes, which involved a Bus and a truck
friday_bus_deaths <- deaths[deaths$Dayweek == 'Friday' & deaths$Bus..Involvement == "Yes", ]


# 1.2 Selecting columns ----

# View the first 5 rows of the friday_bus_deaths to see what time of day they occurred 
friday_bus_deats[1:5, ]
