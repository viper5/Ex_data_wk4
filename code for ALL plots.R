library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## 
# Question 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from 
# all sources for each of the years 1999, 2002, 2005, and 2008.

# aggregate to have data to plot
aggr <- with(NEI, aggregate(Emissions, by=list(year), sum, na.rm=TRUE))
names(aggr) <- c("Year", "Emissions")

plot(aggr$Year, aggr$Emissions, ylab="Emissions", xlab = "Year", type = "l", main="Total emissions yearly", xaxt ='n')

# Adding axis labels
axis(1, at = as.integer(aggr$Year))

dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off() ## Close the PNG device


##
# Question 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# limiting to Baltimore City
lim <- subset(NEI, fips == "24510")

# aggregate to have data to plot
aggr <- with(lim, aggregate(Emissions, by=list(year), sum, na.rm=TRUE))
names(aggr) <- c("Year", "Emissions")

plot(aggr$Year, aggr$Emissions, ylab="Emissions", xlab = "Year", type = "l", main="Total emissions yearly in Baltimore City", xaxt ='n')

# Adding axis labels
axis(1, at = as.integer(aggr$Year))

dev.copy(png, file = "plot2.png", width = 480, height = 480)
dev.off() ## Close the PNG device


##
# Question 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
# a plot answer this question.

# limiting set to Baltimore and aggregating to have data to plot, including also type of Emission
lim <- subset(NEI, fips == "24510")
aggr <- with(lim, aggregate(Emissions, by = list(type, year), sum, na.rm=TRUE))
names(aggr) <- c("Type", "Year", "Emissions")

g <- ggplot(aggr, aes(Year, Emissions))
g + geom_line( aes(color = Type), size = 1) + ggtitle("Emissions yearly in Baltimore City")

dev.copy(png, file = "plot3.png", width = 480, height = 480)
dev.off() ## Close the PNG device


##
# Question 4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

# Finding which SCC should fit the "coal combustion-related sources"
limited <- subset(SCC, grepl("^Fuel Comb.*Coal$", EI.Sector) )

# reducing NEI data, including only if in scope, SCC in list
dataset4 <- subset(NEI, SCC %in% limited$SCC)
head(dataset4)

aggr <- with(dataset4, aggregate(Emissions, by = list(year), sum, na.rm=TRUE))
names(aggr) <- c("Year", "Emissions")
g <- ggplot(aggr, aes(Year, Emissions))
g + geom_line(size = 1) + ggtitle("Coal combustion-related Emissions in U.S. 1999-2008")

dev.copy(png, file = "plot4.png", width = 480, height = 480)
dev.off() ## Close the PNG device


##
# Question 5
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

# Finding which SCC is for motor vehicles
limited <- subset(SCC, grepl(".*Vehicles$", EI.Sector) )

# reducing NEI data, including only if in scope, SCC in list
dataset5 <- subset(NEI, SCC %in% limited$SCC & fips == "24510")
head(dataset5)

aggr <- with(dataset5, aggregate(Emissions, by = list(year), sum, na.rm=TRUE))
names(aggr) <- c("Year", "Emissions")
g <- ggplot(aggr, aes(Year, Emissions))
g + geom_line( size = 1) + ggtitle("Motor Vehicle Emissions in Baltimore City 1999-2008")

dev.copy(png, file = "plot5.png", width = 480, height = 480)
dev.off() ## Close the PNG device


##
# Question 6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from 
# motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# Finding which SCC is for motor vehicles
limited <- subset(SCC, grepl(".*Vehicles$", EI.Sector) )

# reducing NEI data, including only if in scope, SCC in list
dataset6 <- subset(NEI, SCC %in% limited$SCC & (fips == "24510" | fips == "06037"))

aggr <- with(dataset6, aggregate(Emissions, by = list(fips, year), sum, na.rm=TRUE))
names(aggr) <- c("City", "Year", "Emissions")

g <- ggplot(aggr, aes(Year, Emissions))
g + geom_line( aes(color = factor(City, labels = c("Los Angeles County", "Baltimore City") )), size = 1) + ggtitle("Motor Vehicle Emissions in Baltimore City and LA County 1999-2008") + labs(color = "City")

dev.copy(png, file = "plot6.png", width = 480, height = 480)
dev.off() ## Close the PNG device
