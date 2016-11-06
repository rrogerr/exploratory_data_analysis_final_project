########################## PLOT5 ##########################
# How have emissions from motor vehicle sources changed 
# from 1999–2008 in Baltimore City?


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
# WARNING! I'm assuming that looking for the regex "[Mm]otor
# [Vvehicle]" in the column Short.Name of SCC will give me 
# all the motor vehicle sources

SCC_motor <- grepl("[Mm]otor [Vv]ehicle", SCC$Short.Name)

# getting the codes to subset in NEI
codes_motor <- SCC$SCC[SCC_motor]

# subsetting and collapsing NEI
NEI_motor <- filter(NEI, SCC %in% codes_motor & fips == "24510") %>% 
        aggregate(Emissions ~ year, data = ., sum)

# IMPORTANT: It seems that there is no data available for 
# motor vehicle sources in Baltimore City in years 1999
# and 2008


####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot4.png")
p <- ggplot(NEI_motor, aes(year, Emissions)) +
        geom_point(col = "red", size = 3) +
        geom_smooth(method = "lm") +
        labs(title = "emissions from motor vehicle sources in Baltimore City") +
        scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
        labs(y = "Emissions (tons)") +
        theme_set(theme_gray(base_size = 14)) +
        theme(plot.margin = unit(c(1.5,1.5,1.5,0.5), "lines"))
print(p)
dev.off()

