################################################################################
### Cos Fi
### Data Science, EDA, Course Project 2
### June 2014
################################################################################

setwd("/Users/cosfi/Desktop/MOOCs/Coursera/DataScience/EDA/Week3")

dir()

# Load the data and examine struture of the datasets

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI, 6)
tail(NEI,6)

str(NEI)
str(SCC)

summary(SCC$SCC)
summary(SCC$Short.Name)

#############################################################################################
# Question 3: Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 

library(dplyr)


BaltimoreEmissions <- filter(NEI, fips == "24510")

detach(package:dplyr, unload=TRUE)

# Group BaltimoreEmissions by type; Sum type by year
# Reference 1: http://stackoverflow.com/questions/24070714/r-how-to-group-data-by-column-find-min-value-in-each-group-then-extract-3rd-c
# Reference 2: http://www.slideshare.net/jeffreybreen/grouping-summarizing-data-in-r

library(plyr)

totalTypeBaltimore <- ddply(BaltimoreEmissions, .(type, year), summarise, Emissions = sum(Emissions))
head(totalTypeBaltimore, 16)

png("plot3.png", width=640, height=640)

library(ggplot2)

g <- qplot(year, Emissions, data = totalTypeBaltimore, color=type)

# facet_grid(type ~ .)  did not produce a nice a plot as (.~ type)
g + facet_grid (. ~ type) + geom_smooth(method = "lm", color = "blue") + labs(title = "Emissions from PM2.5 in Baltimore by Type") + labs(x = "Year", y = "Total Emmission [in tons]")

# Point has seen an increase in emissions from 1999-2008 for Baltimore City.
# NonPoint has seen a decrease in emissions from 1999-2008 for Baltimore City.
# On-Road has seen a decrease in emissions from 1999-2008 for Baltimore City.
# NonRoad has seen a decrease in emissions from 1999-2008 for Baltimore City.

dev.off()

