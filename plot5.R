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


# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(dplyr)


BaltimoreEmissions <- filter(NEI, fips == "24510") # Create a subset of NEI for Baltimore

detach(package:dplyr, unload=TRUE)

# Group BaltimoreEmissions by motor vehicles; Sum type by year
# Reference 1: http://stackoverflow.com/questions/24070714/r-how-to-group-data-by-column-find-min-value-in-each-group-then-extract-3rd-c
# Reference 2: http://www.slideshare.net/jeffreybreen/grouping-summarizing-data-in-r
# Reference 3: http://www.epa.gov/ttn/chief/eis/gateway/index.html

#########################################################################################

# Reference 4: http://www.epa.gov/ttn/chief/net/2011nei/2011_neiv1_tsd_draft.pdf

# On page 210 (of pdf) Or page 194 of hard-copy of Reference 4 above states: 

# <Start quoted text>

# "4.6 On-road – all Diesel and Gasoline vehicles
# This section includes the description of four EIS sectors:
#	•	Mobile – On-road – Diesel Heavy Duty Vehicles 
#	•	Mobile – On-road – Diesel Light Duty Vehicles 
#	•	Mobile – On-road – Gasoline Heavy Duty Vehicles 
#	•	Mobile – On-road – Gasoline Light Duty Vehicles

# They are treated here in a single section because the methods used are the same across all sectors.

# 4.6.1 Sector Description 
# The four sectors for on-road mobile sources include emissions from motorized vehicles that are normally operated on public roadways. This includes passenger cars, motorcycles, minivans, sport-utility vehicles, light- duty trucks, heavy-duty trucks, and buses. The sectors include emissions from parking areas as well as emissions while the vehicles are moving. 
# SCCs starting with 22010 define the light duty gasoline vehicles including motorcycles, with the exception of SCCs starting with 220107, which define the heavy duty gasoline vehicles. SCCs starting with 22300 define the light duty diesel vehicles, with the exception of SCCs starting with 223007 that define the heavy duty diesel vehicles."

# <End quoted text>

############################################################################################

# Based on the literature, I searched for "Mobile - On-Road" in SCC$EI.Sector to isolate the emissions from motor vehicles

motorVehiclesGrep <- NEI$SCC[grep("Mobile - On-Road", SCC$EI.Sector, ignore.case=FALSE)]
str(motorVehiclesGrep)
summary(motorVehiclesGrep)

motorVehiclesBaltimore <- BaltimoreEmissions[BaltimoreEmissions$SCC %in% motorVehiclesGrep,] # Subset the Baltimore for motor vehicle emissions
str(motorVehiclesBaltimore)

# Or we can use the following with filter() in the dplyr package
# filter(BaltimoreEmissions, SCC == motorVehiclesGrep) and we get the same result as the outcome with grep()

# Compute total Motor Vehicles Emissions in Baltimore by year and then plot to examine trend

library(plyr)

totalMotorVBaltimore <- ddply(motorVehiclesBaltimore, .(year), summarise, Emissions = sum(Emissions))
head(totalMotorVBaltimore)

png("plot5.png", width=560, height=560)

library(ggplot2)

g <- qplot(year, Emissions, data = totalMotorVBaltimore)
g + geom_point(color = "steelblue", size = 3) + geom_smooth(method = "lm", color = "red") + labs(title = "Emissions from Motor Vehicles in Baltimore [1999, 2002, 2005, 2008]") + labs(x = "Year", y = "Total Emmission [in tons]")

# Emissions from motor vehicle sources have decreased generally from 1999–2008 in Baltimore City. The city saw an uptick in motor vehicle emissions from 2002  to 2005.
dev.off()
