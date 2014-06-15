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

########################################################################################################
# Compare emissions from motor vehicles sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County
# fips == "06037". Which city has seen greater changes over time in motor vehicle emissions?

###########################################
###########################################

# First, let's pull out Los Angeles and Baltimore Data

library(dplyr)

LAEmissions <- filter(NEI, fips == "06037")
str(LAEmissions)

BaltimoreEmissions <- filter(NEI, fips == "24510")
str(BaltimoreEmissions)

detach(package:dplyr, unload=TRUE)

LABaltimoreEmissions <- data.frame(rbind(LAEmissions, BaltimoreEmissions))

motorVehiclesGrep <- NEI$SCC[grep("Mobile - On-Road", SCC$EI.Sector, ignore.case=FALSE)]
str(motorVehiclesGrep)
summary(motorVehiclesGrep)

motorVehiclesLABaltimore <- LABaltimoreEmissions[LABaltimoreEmissions$SCC %in% motorVehiclesGrep,]
str(motorVehiclesLABaltimore)                           # There are 4050 observations

# Or we can use the following with filter() in the dplyr package
# filter(LABaltimoreEmissions, SCC == motorVehiclesGrep) and we get the same result as the outcome with grep()

# Compute total Motor Vehicles Emissions in LA-Baltimore by year and then plot to examine trend

library(plyr)

totalMotorVLABaltimore <- ddply(motorVehiclesLABaltimore, .(year, fips), summarise, Emissions = sum(Emissions))
head(totalMotorVLABaltimore, 8)

# Lets plot and examine trends

png("plot6.png", width=640, height=640)

# par(mar = c(5,5,5,2))

library(ggplot2)

g <- qplot(year, Emissions, data = totalMotorVLABaltimore, color = fips)

g +  facet_grid (.~ fips) + geom_smooth(method = "lm") + labs(title = "Emissions from Motor Vehicles in LA & Baltimore [1999, 2002, 2005, 2008]") + labs(x = "Year", y = "Total Emissions [in tons]")

# The rate of change of emissions from motor vehicles in LA is greater than the rate of change in Baltimore.
# The magnitude of emissions from motor vehicles in LA is far greater than those in Baltimore.
# Baltimore saw an increase in the emissions from motor vehicles from 2002 to 2005. LA saw an increase in their emissions from 2005 to 2008.

dev.off()

