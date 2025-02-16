#Initial script

#Load libraries

install.packages("egg")

library(tidyverse)
library(here)
library(egg)

#Load data

if(!dir.exists("./data")){dir.create(here("./data"))}#Set-up directory for work and data

#Download data

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
temp <- tempfile() #Temporary variable to save zip file
download.file(url,temp)

#Export data

repdata <- read.table(temp, sep = ",", header = TRUE)

unlink(temp); rm(temp, url) #Delete temporary files

str(repdata)

#Across the United States, which types of events (as indicated 
#in the EVTYPEEVTYPE variable) are most harmful with respect to population health?

eventype <- repdata %>% 
        group_by(EVTYPE) %>%
        summarise(fatalities = sum(FATALITIES), 
                  mag = sum(MAG),
                  injuries = sum(INJURIES),
                  propmg = sum(PROPDMG))

fatalities <- eventype %>% filter(fatalities != 0)

fat <- eventype %>% 
        top_n(20, fatalities) %>%
        ggplot() + 
        geom_bar(aes(x=fatalities, 
                     y=reorder(EVTYPE,+fatalities)),
                 stat="identity") +
        labs(title="Fatalities",x="",y="")

inj <- eventype %>% 
        top_n(20, injuries) %>%
        ggplot() + 
        geom_bar(aes(x=injuries, 
                     y=reorder(EVTYPE,+injuries)),
                 stat="identity") +
        labs(title="Injuries",x="",y="")

#Across the United States, which types of events have the greatest economic consequences?

propmg <- eventype %>% filter(propmg != 0)

propmg %>% 
        top_n(20, propmg) %>%
        ggplot() + 
        geom_bar(aes(x=propmg, 
                     y=reorder(EVTYPE,+propmg)),
                 stat="identity") +
        labs(title="Property Damage",x="",y="")

