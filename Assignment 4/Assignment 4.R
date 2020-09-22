## Course Project

## Loading Necessary Package
library(dplyr)
library(ggplot2)
setwd("~/Documents/Coursera/Reading Material/Data Science/C.4-Exploratory Data Analysis/Assignment 4")
options(scipen = 999)


## Reading Data
NEI <- readRDS("./exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata_data_NEI_data/Source_Classification_Code.rds")


## Problem 1
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
Total_PM2.5 <- tapply(NEI$Emissions, NEI$year, sum)
Year <- unique(NEI$year)
png("Plot_1.PNG", width = 800, height = 600)
plot(Year, Total_PM2.5, type = "l", ylim = c(0, 8000000), font.lab = 2, main = "Total US Emissions by Year")
points(Year, Total_PM2.5, pch = 19)
dev.off()

## Problem 2
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
NEI_Baltimore <- NEI[NEI$fips == "24510", ]
Total_PM2.5_Baltimore <- tapply(NEI_Baltimore$Emissions, NEI_Baltimore$year, sum)
png("Plot_2.PNG", width = 800, height = 600)
plot(Year, Total_PM2.5_Baltimore, type = "l", ylim = c(0, 4000), font.lab = 2, main = "Total US Emissions by Year for Baltimore City (fips == 24510)")
points(Year, Total_PM2.5_Baltimore, pch = 19)
dev.off()

## changing default font and adjusting it to center for ggplot2 library
theme_update(plot.title = element_text(face = "bold", hjust = 0.5), axis.title = element_text(face = "bold", hjust = 0.5))

## Problem 3
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
NEI_Baltimore_Year_Type <- NEI_Baltimore %>% group_by(year, type) %>% summarise(Total_PM2.5 = sum(Emissions), .groups = "keep")
png("Plot_3.PNG", width = 800, height = 600)
qplot(year, Total_PM2.5, data = NEI_Baltimore_Year_Type, geom = c("point", "line") , facets = .~type, xlab = "Year", main = "PM2.5 by Source for Baltimore City, 1999-2008")
dev.off()

## Problem 4
## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
Coal_Combustion_Sources <- SCC %>% filter(grepl("Combustion", SCC.Level.One) | grepl("Combustion", SCC.Level.Two)) %>% filter(grepl("Coal", SCC.Level.Three) | grepl("Coal", SCC.Level.Four)) %>% summarise(SCC)
Coal_Combustion_Year <- NEI %>% group_by(type, year) %>% inner_join(Coal_Combustion_Sources) %>% summarise(Total_PM2.5 = sum(Emissions), .group = "keep")
png("Plot_4.PNG", width = 800, height = 600)
qplot(year, Total_PM2.5, data = Coal_Combustion_Year, col = type, geom = c("point", "line"), ylim = c(0, 600000), xlab = "Year", main = "US Emissions from Coal Sources by Year")
dev.off()

## Problem 5
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
Motor_Vehicle_Sources <- SCC %>% filter(grepl("Vehicle", Short.Name)) %>% summarise(SCC)
Baltimore_Vehicle_Year <- NEI %>% inner_join(Motor_Vehicle_Sources) %>% filter(fips == "24510") %>% group_by(year) %>% summarise(Total_PM2.5 = sum(Emissions), .groups = "keep")
png("Plot_5.PNG", width = 800, height = 600)
qplot(year, Total_PM2.5,data = Baltimore_Vehicle_Year, geom = c("point", "line"), ylim = c(0, 80), xlab = "Year", main = "Motor Vehicle Emissions in Baltimore City by Year")
dev.off()

## Problem 6
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
Baltimore_LosAngeles_Vehicle_Year <- NEI %>% inner_join(Motor_Vehicle_Sources) %>% filter(fips == "24510" | fips == "06037") %>% group_by(city = ifelse(fips == "24510", "Baltimore, MD", "Los Angeles, CA"), year) %>% summarise(Total_PM2.5 = sum(Emissions), .groups = "keep")
png("Plot_6.PNG", width = 800, height = 600)
qplot(year, Total_PM2.5,data = Baltimore_LosAngeles_Vehicle_Year, facets = .~city, geom = c("point", "line"), xlab = "Year", main = "Motor Vehicle Emissions in Baltimore City by Year")
dev.off()

