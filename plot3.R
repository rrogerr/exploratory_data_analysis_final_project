########################## PLOT3 ##########################
# Of the four types of sources indicated by the type (point, 
# nonpoint, onroad, nonroad) variable, which of these four 
# sources have seen decreases in emissions from 1999–2008 
# for Baltimore City? Which have seen increases in emissions 
# from 1999–2008? Use the ggplot2 plotting system to make a 
# plot answer this question.


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

NEI1 <- NEI %>% filter(fips == "24510") %>% 
        aggregate(Emissions ~ year + type, data = ., sum)



####################### PLOT DEVICE #######################

ath <- "exploratory_data_analysis_final_project"
path <- paste0(p, ath)
setwd(path)

png("./plot3.png")

p <- ggplot(NEI1, aes(year, Emissions))
p <- p + geom_point() + geom_smooth(method = "lm") +
        facet_wrap(~type, ncol = 2) +
        labs(title = "Emissions by year and by source (Baltimore City)") +
        scale_x_discrete(limits = c(1999, 2002, 2005, 2008)) +
        theme(panel.margin = unit(3, "lines")) +
        theme(plot.margin = unit(c(1.5,1.5,1.5,0.5), "lines"))+
        theme_set(theme_gray(base_size = 13))

print(p)

dev.off()
