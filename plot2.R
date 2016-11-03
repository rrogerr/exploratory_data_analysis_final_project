########################## PLOT2 ##########################
# Have total emissions from PM2.5 decreased in the 
# Baltimore City, Maryland (fips == "24510") from 1999 to 
# 2008? Use the base plotting system to make a plot answering 
# this question.


######################### IMPORTS #########################

library(dplyr)

p <- "/home/rogelio/Desktop/datasciencecoursera/"
ath <- "data/eda_final_project"
path <- paste0(p, ath)

setwd(path)
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



######################## SUBSETTING ########################

NEI1 <- NEI %>% filter(fips == "24510") %>% 
        aggregate(Emissions ~ year, data = ., sum)



####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot2.png")
with(NEI1, plot(year, Emissions, xlab = "year", 
                      ylab = "Emissions (tons)",
                      pch = 19,
                      col = "red",
                      xaxt = "n"))

# assigning ticks to x axis with the years of
# interest
axis(side = 1, at = c(1999,2002,2005,2008))

# plotting linear model to show decreasing tendency
model <- lm(NEI1$Emissions ~ NEI1$year)
abline(model, lwd = 2)

title("Emissions in Baltimore City (all sources)")

dev.off()
