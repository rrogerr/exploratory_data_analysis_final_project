########################## PLOT4 ##########################
# Across the United States, how have emissions from coal 
# combustion-related sources changed from 1999–2008?


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
# WARNING! I'm assuming that looking for the regex "[Cc]oal" 
# in the column Short.Name of SCC will give me all the coal
# related sources

SC_coal <- grepl("[Cc]oal", SCC$Short.Name, perl = TRUE)

# getting the codes to subset in NEI
codes_coal <- SCC$SCC[SC_coal]

# subsetting and collapsing NEI
NEI_coal <- filter(NEI, SCC %in% codes_coal) %>% 
        aggregate(Emissions ~ year, data = ., sum)

####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot4.png")
p <- ggplot(NEI_coal, aes(year, Emissions)) +
        geom_point(col = "red", size = 3) +
        geom_smooth(method = "lm") +
        labs(title = "National emissions (coal combustion-related)") +
        scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
        labs(y = "Emissions (tons)") +
        theme_set(theme_gray(base_size = 14)) +
        theme(plot.margin = unit(c(1.5,1.5,1.5,0.5), "lines"))
print(p)
dev.off()