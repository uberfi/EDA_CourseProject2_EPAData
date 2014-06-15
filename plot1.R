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

##############################################################################################################################

# Question 1: Have total emissions from PM2.5 decreased in the United Stated from 1999 to 2008?

# We are to use R's base plotting system

# Need to combine the disparate measures per year into total emmisions per year
# Reference: http://stackoverflow.com/questions/17903205/sum-multiple-columns-by-group-with-tapply

totalEmissions <- aggregate(NEI$Emissions, by=list(NEI$year), sum)
str(totalEmissions)
summary(totalEmissions)
totalEmissions
colnames(totalEmissions) <- c("Year", "totalEmissions")
totalEmissions


# Do not connect dots (points(Year, totalEmissions)) since the data is discrete

png("plot1.png", width=640, height=640)

with(totalEmissions, plot(Year, totalEmissions, xlab = "Year", ylab = "Total Emissions (tons)", main = "Total Emissions from PM2.5 in the US", col = "blue", pch=20, cex=3))

model <- lm(totalEmissions ~ Year, totalEmissions) # Examine the trend of the relationship

abline(model, col="red", lwd=2) 

# the plot and the linear (OLS) model show that Total Emissions decreased from 1999 to 2008.
# Answer: Yes, total emissions from PM2.5 decreased in the United States from 1999 to 2008.

dev.off()
