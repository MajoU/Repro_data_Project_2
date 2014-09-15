library(plyr)
library(data.table) # package 1.9.3

# DATA PROCESSING

col_names <- c("EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
# data <- data.table(read.csv(bzfile("./repdata_data_StormData.csv.bz2"))[,col_names])
data <- fread("./Storm_data.csv")

# Change data colnames to lowercase
setnames(data, tolower(names(data)))
# Change all variables in evtype column to lower case
data[,evtype := tolower(evtype)]

# How much repetitive is variables in the evtype column?
head(arrange(data[, .N, by = evtype], desc(N)), 40)

# How much repetitive is some event with similar names?
head(arrange(data[grep("wind", evtype), .N, by = evtype], desc(N)),20)
head(arrange(data[grep("flood", evtype), .N, by = evtype], desc(N)),20)

# The most influential source of event. I try to join some similar events with
# influence on health and damage. 
events <- list("heavy rain|rain", "winter events|winter|frost|cold|snow|freez|ice|blizzard", 
    "hurricane events|hurricane|typhoon|tropical|waterspout", "heat|warm", "hail", 
    "flood events|flood|surf|surge", "wind events|wind", "tornado", "rip", "fog", "fire")

# this is complex line that deal with data , grep every element of list within data 
# column evtype and change this grep variables to first element of name from event list 
# through gsub function. the solution is within the data because data.table package (different than data.frame).
tmp <- lapply(events, function(x) data[grep(x, evtype), evtype := gsub("\\|+[a-z]+", "", x)])

# FIRST PART

harm_sum <- data[, list(fatal = sum(fatalities), injur = sum(injuries)), by = evtype]
head(harm_sum[order(-fatal), fatal, evtype],15)
head(harm_sum[order(-injur), injur, evtype],15)

# SECOND PART

prop_token <- data.table(propdmgexp = c("K", "M", "B"), dollars = as.numeric(c("1000", "10e5", "10e8")))
prop_merge <- merge(prop_token, data, by = "propdmgexp")

#------------------------------------------------------------------------------------
#  ALTERNATIVE

# prop_merge <- merge(setkey(prop_token, propdmgexp), setkey(data, propdmgexp))

# Find propdmgexp tokens through through regex

# data[propdmgexp %like% "K|M|B", ]

#------------------------------------------------------------------------------------

prop_sum <- prop_merge[, list(prop = sum(propdmg * dollars)), by = evtype]

#---------------------------------------------------------
#  ALTERNATIVE

# prop_sum <- ddply(prop_merge[, prop := propdmg * dollars],.(evtype), summarize, prop = sum(prop))

#--------------------------------------------------------

prop_sort <- arrange(prop_sum, desc(prop))



crop_token <- data.table(cropdmgexp = c("K", "M", "B"), dollars = as.numeric(c("1000", "10e5", "10e8")))
crop_merge <- merge(crop_token, data, by = "cropdmgexp")
crop_sum <- crop_merge[, list(crop = sum(cropdmg * dollars)), by = evtype]
crop_sort <- arrange(crop_sum, desc(crop))



# Other codes for learning

# add new column sum_fatal with sum of FATALITIES by evtype column source
data[,sum_fatal := sum(fatalities), by = evtype]

# add new column event that contains sum of FATALITIES by evtype column
# sources selected by grepl("FLOOD") name.
data[,event := sum(fatalities), by = grepl("flood",evtype)]

