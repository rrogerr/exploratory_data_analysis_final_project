########################## PLOT1 ##########################
# Have total emissions from PM2.5 decreased in the United 
# States from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008


######################### IMPORTS #########################

p <- "/home/rogelio/Desktop/datasciencecoursera/"
ath <- "data/eda_final_project"
path <- paste0(p, ath)

setwd(path)
NEI <- readRDS("./summarySCC_PM25.rds")


#################### COLLAPSING BY YEAR ####################

# collapse emissions by year, taking sum
em_by_year <- aggregate(NEI$Emissions, list(NEI$year), sum)
names(em_by_year) <- c("year", "total_emissions")


####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot1.png")
with(em_by_year, plot(year, total_emissions, xlab = "year", 
                      ylab = "total emissions (tons)",
                      pch = 19,
                      col = "red",
                      xaxt = "n"))

# assigning ticks to x axis with the years of
# interest
axis(side = 1, at = c(1999,2002,2005,2008))

# plotting linear model to show decreasing tendency
model <- lm(em_by_year$total_emissions ~ em_by_year$year)
abline(model, lwd = 2)

title("total emissions by year (all counties, all sources)")

dev.off()