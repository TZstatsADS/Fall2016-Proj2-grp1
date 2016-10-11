library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
##Zeng's data
setwd('~/Desktop/Project 2 APP')
data <- read.csv("oil.csv")
data <- data[-(1:725),]
names(data)
attach(data)
data <- data.frame(Facility.Address ,#1
                   Natural.Gas.Utility..Con.Edison.or.National.Grid ,#2
                   Number.of.identical.boilers ,#3
                   Boiler.capacity..Gross.BTU. ,#4
                   Boiler.Installation.Date ,#5
                   Is.boiler.dual.fuel.capable. ,#6
                   Age.range.of.boiler ,#7
                   Primary.Fuel ,#8
                   Total.Estimated.Cosumption...High.Estimate..Gallons. ,#9
                   Total.Estimated.Cosumption...Low.Estimate..Gallons. ,#10  Consumption
                   Needs.to.comply.with.Greener.Greater.Buildings.Laws. ,#11
                   Building.Type ,#12
                   Total.area.of.buildings.on.lot ,#13
                   Number.of.buildings.on.property..tax.lot. ,#14
                   Number.of.floors ,#15
                   Number.of.total.units ,#16
                   Year.constructed)  #17
                                      #18  Long
                                      #19  Lat
data <- data[-which(data[,17]==0),]
levels(data[,11]) <- c("No","Yes")
data <- data[-which(data[,10]==0),]
levels(data[,6]) <- c("","No","Yes")  #  dual fuel  only yes and no!
data <- data.frame(data,rep(0,nrow(data)),rep(0,nrow(data)))
names(data)[18:19] <- c("Long","Lat")
names(data)[10] <- "Consumption"
for(i in 1:nrow(data))
{
  data[i,18] <- as.numeric(sub("\\).*", "", sub(".*\\,", "", data[i,1])))
  data[i,19] <- as.numeric(sub("\\,.*", "", sub(".*\\(", "", data[i,1])))
}
data <- data[which(data[,18]<0),]
row.names(data) <- 1:7900

data <- data.frame(data,rep(0,nrow(data)))
names(data)[20] <- "age.index"
index <- c(0,13,18,23,28,33,38,43,48,8,3,53)
for(i in 1:nrow(data))
{
  data[i,20] <- index[which(levels(data[,7])==data[i,7])]
}


## William's data
oil_raw <- read.csv('oil.csv', sep = ',', header = TRUE)
oil_raw <- oil_raw[-(1:725),]
oil <- oil_raw[, c(11, 14, 15, 17, 19, 22, 26, 27, 28, 30)]

oil_names <- c("Boiler_Capacity", "Dual", 'Boiler_Age', "Fuel_Type", 'Low_Gallons', 
               "Green", 'Total_Area', 'No_Buildings', "No_Floors", 'No_Units')

colnames(oil) <- oil_names

oil <- oil %>%
  mutate(Total_Area_Tile = ntile(Total_Area, 8))


oil$Boiler_Age <- ordered(oil$Boiler_Age, 
                          levels = c('Fewer than 5 years old', '5 to 10 years old',
                                     '11 to 15 years old', '16 to 20 years old',
                                     '21 to 26 years old', '26 to 30 years old',
                                     '31 to 35 years old', '36 - 40 year old',
                                     '41 - 45 years old', '46 - 50 years old',
                                     'Over 50 years'))
## Sun's data
df <- data

## Zhao's data
oil1 <- read.csv('oil.csv', sep = ',', header = TRUE)
oil1 <- oil_raw[-(1:725),]
names(oil1)[2] <- "Facility Address"
names(oil1)[3] <- "Natural Gas Utility"
names(oil1)[10] <- "Number of Boilers"
names(oil1)[11] <- "Boiler Capacity"
names(oil1)[12] <- "Boiler Installation Date" 
names(oil1)[14] <- "DUAL FUEL"
names(oil1)[15] <- "Age of Boiler"
names(oil1)[17] <- "Primary Fuel"
names(oil1)[19] <- "Oil Consumption"
names(oil1)[22] <- "GGB Law"
names(oil1)[24] <- "Building Type"
names(oil1)[26] <- "Area of Building"
names(oil1)[28] <- "Number of floors"
names(oil1)[30] <- "Number of total units"
names(oil1)[31] <- "Year Constructed"