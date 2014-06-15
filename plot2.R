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


#############################################################################################################################

# Question 2: Have total emissions from PM2.5 decreased in Baltimore City (fips = 24510) decreased from 1999 to 2008?

# isolate data for Baltimore; examine the structure and aggregate Emissions values by Year

library(dplyr)

BaltimoreEmissions <- filter(NEI, fips == "24510")
str(BaltimoreEmissions) # exploring the data; 
totalBaltimore <- aggregate(BaltimoreEmissions$Emissions, by=list(BaltimoreEmissions$year), sum)
colnames(totalBaltimore) <- c("Year", "totalBaltimoreEmissions")
totalBaltimore

png("plot2.png", width=560, height=560)

with(totalBaltimore, plot(Year, totalBaltimoreEmissions, xlab = "Year", ylab = "Total Baltimore Emissions (tons)", main = "Total Emissions from PM2.5 in Baltimore", col = "pink", pch=20, cex=3))

model <- lm(totalBaltimoreEmissions ~ Year, totalBaltimore) # Examine the trend of the relationship

abline(model, col="blue", lwd=2) 

# the plot and the linear (OLS) model show that Total Emissions in Baltimore has decreased from 1999 to 2008.
# however, there was an uptick in total emissions from 2002 to 2005.

dev.off()