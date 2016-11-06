########################## PLOT6 ##########################
# Compare emissions from motor vehicle sources in Baltimore 
# City with emissions from motor vehicle sources in Los 
# Angeles County, California (fips == "06037"). Which city 
# has seen greater changes over time in motor vehicle 
# emissions?


######################### IMPORTS #########################

library(dplyr)
library(ggplot2)

p <- "/home/rogelio/Desktop/datasciencecoursera/"
ath <- "data/eda_final_project"
path <- paste0(p, ath)

setwd(path)
NEI <- readRDS("./summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


######################## SUBSETTING ########################
# WARNING! I'm assuming that looking for the regex
# "[Vv]ehicle" in the column Short.Name of SCC will give me 
# all the motor vehicle sources

SCC_motor <- grepl("[Vv]ehicle", SCC$Short.Name)

# getting the codes to subset in NEI
codes_motor <- SCC$SCC[SCC_motor]

# subsetting and collapsing NEI for Baltimore City
NEI_BC <- filter(NEI, SCC %in% codes_motor & fips == "24510") %>% 
        aggregate(Emissions ~ year, data = ., sum)

# subsetting and collapsing NEI for Los Angeles
NEI_LA <- filter(NEI, SCC %in% codes_motor & fips == "06037") %>% 
        aggregate(Emissions ~ year, data = ., sum)


####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot6.png")
p <- ggplot(NEI_LA, aes(year, Emissions)) +
        geom_point(aes(color = "Los Angeles"), size = 3) +
        geom_smooth(method = "lm") +
        geom_point(data = NEI_BC, aes(color = "Baltimore City"), size = 3) +
        geom_smooth(data = NEI_BC, method = "lm") +
        labs(title = "emissions from motor vehicle sources\n(Baltimore City vs. Los Angeles)") +
        scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
        labs(y = "Emissions (tons)") +
        theme_set(theme_gray(base_size = 14)) +
        theme(plot.margin = unit(c(2.5,1.5,1.5,0.5), "lines"))
print(p)
dev.off()