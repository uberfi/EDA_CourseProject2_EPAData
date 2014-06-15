################################################################################
### Cos Fi
### Data Science, EDA, Course Project 2
### June 2014
################################################################################


setwd("/Users/cosfi/Desktop/MOOCs/Coursera/DataScience/EDA/Week3")


# Load the data and examine struture of the datasets

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(NEI, 6)
tail(NEI,6)

str(NEI)
str(SCC)

summary(SCC$SCC)

summary(SCC$Short.Name)

summary(SCC$EI.Sector)

#################################################################################################
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# Reference 1: http://www.state.gov/documents/organization/219472.pdf
# Reference 2: http://www.epa.gov/ttn/chief/net/2011nei/2011_neiv1_tsd_draft.pdf
# Reference 3. http://stackoverflow.com/questions/4220631/how-do-i-grep-in-r

# Here are some of the processes we deployed to isolate the data for emissions from Coal combustions:

# 1. According to the literature (see the three pdf links in this thread), there are four sectors (EI.Sector) dealing with Coal combustion:
## Fuel Comb - Comm/Institutional - Coal
## Fuel Comb - Electric Generation - Coal
## Fuel Comb - Industrial Boilers, ICEs - Coal
## Fuel Comb - Residential - Other

# 2.  Subsetted the SCC dataset on the four Coal combustion sectors

# 3. Examines structure and full records of the subsets and found that two of the four SCCs in the Fuel Comb -Residential - Other are not related to Coal Combustion.

# 4. Combined (rbind()) the subsets into a one subset of the SCC dataset on Coal Combustion; lets call this X. X has 101 SCCs.

# 5. Conducted a search of "Coal" in SCC$SCC.Level.Three

# 6. Subsetted SCC on the outcome of the search in [5] above; lets call this Y. Y has 172 SCCs.

# 7. Examine the discrepancies  between X and Y using x <- xX$CC %in% Y$SCC, y <- Y$SCC %in% X$SCC; X[!x]; Y[!y];

# 8. Examined the differences datasets and of course accepted all of X since they are explicitly coded as Coal combustion; and found seven SCCs in Y but not in X that deal with Coal combustion

# 9. Created a new X to include the seven SCCs

# 10. Subsetted NEI on the SCCs of new X in step [9]

# 11. Integrity check issues: There are 108 unique SCCs in new X; but there are only 89 unique SCCs in the subset of NEI (see step [10]) based on the 108 SCCs.

# 12. Final subset of NEI has 40,639 observations on unique SCCs.

# While working on fisnishing up the task, I received a related query about searching the Short.Name feature for "Coal" and "Comb". 
# What follows is a feedback from the exercise

# F1. I searched SCC$Short.Name as was uggested by atopos: grep("Comb.*Coal|Coal.*Comb", SCC$Short.Name, ignore.case=FALSE)

# F2. The search returned 91 SCCs in the SCC data frame:  SCC$SCC[grep("Comb.*Coal|Coal.*Comb", SCC$Short.Name, ignore.case=FALSE)]

# F3. Only two of the 91 SCCs were not in the list of SCCs gotten from sector search (see previous post on sectored (EI.Sector) search)

# F4. The two SCCs deal with Coal Combustion; hence they were added to the sectored outcome; So we are up to 103 = (101 + 2) SCCs. See step 4 above.

# F5.  I subsetted NEI data frame on those 103 SCCs.

# F6. This is were things got interesting:
### I got 40,475 observations on 83 unique SCCs from the search of Short.Name and Sector
### Recall that the search of the sectors given in step 1 (above) augmented by a search for "Coal" in SCC.Level.Three had returned 40,639 observations on 89 unique SCCs. 

### A comparison of the sectored approach SCCs (in steps 1-12) and the Short.Named (Short.Name) approach (in steps F1-F5) showed that the Short.Named approach returns a subset of the sectored SCCs.
### The following six SCCs in the sectored search are not in the Short.Named search: 39000299, 39000289, 39000203, 39000201, 39000199, & 39000189

#################################################################################################################

### Execute Steps 1-12

### Pull coal combustion sources from EI.Sector in the SCC dataset, examine their structures

a <- SCC[SCC$EI.Sector == "Fuel Comb - Comm/Institutional - Coal", ]
str(a)
summary(a)
a                        
# The number of cases are small enough that I was able to inspect all of them; more generally, we would sample cases to examine the quality and "validity" of the subsetted data
# There are 31 observations in the "Fuel Comb - Comm/Institutional - Coal" sector.

b <- SCC[SCC$EI.Sector == "Fuel Comb - Electric Generation - Coal", ]
str(b)
summary(b)
b                        
# The number of cases are small enough that I was able to inspect all of them; more generally, we would sample cases to examine the quality and "validity" of the subsetted data
# There are 35 observations in the "Fuel Comb - Electric Generation - Coal" sector.


c <- SCC[SCC$EI.Sector == "Fuel Comb - Industrial Boilers, ICEs - Coal", ]
str(c)
summary(c)
c                        
# The number of cases are small enough that I was able to inspect all of them; more generally, we would sample cases to examine the quality and "validity" of the subsetted data
# There are 33 observations in the "Fuel Comb - Industrial Boilers, ICEs - Coal" sector

d <- SCC[SCC$EI.Sector == "Fuel Comb - Residential - Other", ]
str(d)
summary(d)
d                       
# The number of cases are small enough that I was able to inspect all of them; more generally, we would sample cases to examine the quality and "validity" of the subsetted data
# There are four cases in "Fuel Comb - Residential - Other" sector, two d[1:2, ] are related to Coal combustion: Anthracite Coal; Bituminous/Subbituminous Coal

# Extract data related to coal combustion from d: Anthracite Coal; Bituminous/Subbituminous Coal
dCoal <- d[1:2, ]
dCoal


# combine a, b, c and dCoal into one dataset of emissions from motor vehicles 

coalCombBySector <- data.frame(rbind(a, b, c, dCoal)) # Concludes Steps 1-4 (Sector search)
str(coalCombBySector)

# There are 101 SCCs identified as coal combustion sources in the SCC dataset via the method above
# Note that searching the word "Coal" in EI.Sector would have yielded 99 SCCs, because two SCCs are embedded in the "Fuel - Comb - Residential - Other" sector and cannot be retrieved by searching at the sector level.
# By examining the cases in the "Fuel - Comb - Residential - Other" sector we were able to identify the two SCCs associated with coal combustion.

##########################################################################################################

# One can also isolate "Coal" combustion related sources from SCC$SCC.Level.Three

testCoalName <- SCC$SCC[grep("Coal", SCC$SCC.Level.Three, ignore.case=FALSE)]

coalCombByName <- SCC[SCC$SCC %in% testCoalName, ] # The idea here is to find the SCCs by searching the word "Coal" in SCC$SCC.Level.Three
str(coalCombByName)

# Searching by word "Coal" in SCC$SCC.Level.Three returns 172 SCCs; 
# Next, we should investigate the discrepancy between sector search and word search.

##########################################################################################################

# We want to examine the discrepancy between "sector" search and "Coal" search in SCC.Level.Three

sector <- coalCombBySector
name <- coalCombByName

# In coalCombBySector and in coalCombByName

x <- sector$SCC %in% name$SCC

# In coalCombByName and coalCombBySector

y <- name$SCC %in% sector$SCC

# Where are they different?

# In sector but not in name

sectorOnly <- sector[!x, ]

# In name but not in sector

nameOnly <- name[!y, ]


str(sectorOnly)
str(nameOnly)

sectorOnly # These items are related to Coal Combustion

nameOnly
tail(nameOnly, 10)

# The tail seven SCCs in name are not in sector (i.e., the last seven observations in nameOnly) seems related to Coal Combustion and thus should be added onto coalCombBySector to yield coalCombSCC

# extract the last seven cases in nameOnly

nameOnlyCoal <- nameOnly[88:94, ]
str(nameOnlyCoal)
nameOnlyCoal

# We will combine coalCombBySector, and nameOnlyCoal to form the dataset of emissions from coal combustion

coalCombSCC <- data.frame(rbind(coalCombBySector, nameOnlyCoal))

############################################################################

### Steps F1-F5 explored alternative way to extract the emissions from coal combustion

# I undertook F1-F5  in response to a request from a colleague to check the SCCs returned in steps 1-12
# In F1-F5 we search "Coal" and "Comb" in SCC$Short.Name and extract the SCCs from the outcome

coalShortName <- SCC$SCC[grep("Comb.*Coal|Coal.*Comb", SCC$Short.Name, ignore.case=FALSE)] # The idea here is to find the SCCs by searching the word "Coal" and "Comb" in SCC$Short.Name
str(coalShortName)

coalCombShortName <- SCC[SCC$SCC %in% coalShortName, ] # Subset the SCC for the outcome of the immediate previous search
str(coalCombShortName)


sector <- coalCombBySector                 # Sector search (see Steps 1-4)
shortName <- coalCombShortName     # Short.Name search (see F1-F5)

# In coalCombBySector and in coalCombShortName

x <- sector$SCC %in% shortName$SCC

# In coalCombShortName and in coalCombBySector

y <- shortName$SCC %in% sector$SCC

# Where are they different?

# In sector but not in shortName

sectorOnly <- sector[!x, ]

# In shortName but not in sector

shortNameOnly <- shortName[!y, ]


str(sectorOnly)
str(shortNameOnly)
sectorOnly
shortNameOnly  

z <- shortNameOnly                                 # SCCs: 20100301,  10101901
z

sectorPlusShortName <- data.frame(rbind(coalCombBySector, z))  # 103 SCCs
str(sectorPlusShortName)
length(unique(sectorPlusShortName$SCC))

NEIShortName <- NEI[NEI$SCC %in% sectorPlusShortName$SCC, ]
str(NEIShortName)
length(unique(NEIShortName$SCC))
################################################################################################################################

# Final two-way comparison

U <- coalCombSCC$SCC                      # This is the set of SCCs returned from the four sectors on Coal combustion augmented by searching "Coal" in SCC.Level.Three

V <- sectorPlusShortName$SCC            # This is the set of SCCs returned from SCC$SCC[grep("Comb.*Coal|Coal.*Comb", SCC$Short.Name, ignore.case=FALSE)]
                                                            # augmented by SCCs in the four sectors but that are not in the search of "Coal" and "Comb" in the Short.Name

# In U and in V

u <- U %in% V

# In V and in U

v <- V %in% U

# Where are they different?

# In sector but not in shortName

UOnly <- coalCombSCC[!u, ]

# In shortName but not in sector

VOnly <- sectorPlusShortName[!v, ]

str(UOnly)          # Has seven SCCs
str(VOnly)          # Has two SCCs

VOnly               # Contains two SCCs that are not U: 20100301, 10101901
UOnly              # Contains seven SCCs that are not in V: 39000299, 39000289, 39000288, 39000203, 39000201, 39000199, & 39000189 

which(V == 20100301)
which(U == 20100301)
which(V == 10101901)
which(U == 10101901)

#######################################################################################################################
#######################################################################################################################


# Putting all together to subset NEI and investigate trends

coalCombSCC <- data.frame(rbind(coalCombBySector, nameOnlyCoal))
str(coalCombSCC)
length(coalCombSCC$SCC)
length(unique(coalCombSCC$SCC))

CoalUS <- NEI[NEI$SCC %in% coalCombSCC$SCC, ]
str(CoalUS)

length(CoalUS$SCC)
length(unique(CoalUS$SCC))

# Or we can use the following with filter() in the dplyr package
# filter(NEI, SCC == coalUS) and we get the same result as the outcome with grep()

# Compute total Coal by year ane then plot to examine trend

library(plyr)

totalCoal <- ddply(CoalUS, .(year), summarise, Emissions = sum(Emissions))
head(totalCoal)

png("plot4.png", width=560, height=560)

library(ggplot2)

g <- qplot(year, Emissions, data = totalCoal)
g + geom_point (color = "steelblue") + geom_smooth (method = "lm", color = "pink") + labs(title = "Emissions from Coal Combustion in US") + labs(x = "Year", y = "Total Emmission from Coal Combustion [in tons]")

# Overall, we have a drecrease in emissions from coal combustion in the US. However, there was an increase in emissions from 2002 to 2005.

dev.off()

####################################################################################################################
# Done!
                        
                        
                