library(plyr)
data <- read.csv(bzfile("./repdata_data_StormData.csv.bz2"))[,c("EVTYPE", "FATALITIES", "INJURIES")]
weather <- ddply(data, .(EVTYPE), summarize, fatal = sum(FATALITIES), injur = sum(INJURIES))
tabs <- arrange(weather, desc(fatal), desc(injur))
names <- list("STORM", "WIND","TORNADO","WINTER","HEAT","FLOOD","LIGHTNING","RIP","COLD","FOG","FIRE","HAIL")
grep_names <- lapply(names, function(x) tabs[tabs$EVTYPE %in% grep(x,tabs$EVTYPE, value=T,ignore.case=T),])
sum_names <- lapply(grep_names, function(x) colSums(Filter(is.numeric, x)))
weather_df <- data.frame(weather = unlist(names), ldply(sum_names))

# Second Part

# use merge function for replace PROPDMGEXP sign to dollar values

money_df <- data.frame(signs = c("K", "M", "B"), dollars = c("1000", "10e5", "10e8"))

merge_money <- merge(data, money_df, by.x = "PROPDMGEXP", by.y = "signs")

# Interesting Data table and some subsetting

# add new column sum_fatal with sum of FATALITIES by EVTYPE column source
data[,sum_fatal := sum(FATALITIES), by = EVTYPE]

# add new column event that contains sum of FATALITIES by EVTYPE column
# sources selected by grepl("FLOOD") name.
data[,event := sum(FATALITIES), by = grepl("FLOOD",EVTYPE)]

# Subset data

# Subset data with grep("WIND") in EVTYPE
data[grep("WIND", data$EVTYPE, ignore.case = T)]

# Subset column 2 and 3 in data with grep("WIND") in EVTYPE 
data[grep("WIND", data$EVTYPE, ignore.case = T)][,c(2,3), with = F]

# Subset data by grep("WIND") in EVTYPE and create new column fats with
# sums of FATALITIES (as column in data)
data[grep("WIND", data$EVTYPE, ignore.case=T), fats := sum(FATALITIES)]

# Find the name of event in the EVTYPE and create fatal column as sum of
# FATALITIES within grep(names) variable
sum_event <- lapply(names,function(names) data[grep(names,data$EVTYPE,ignore.case=T),fatalit := sum(FATALITIES)])


test <- data.frame(data)
# subset EVTYPE, FATALITIES column by grep("WIND") in EVTYPE column
t <- test[grep("WIND", test$EVTYPE, ignore.case=T), c("EVTYPE","FATALITIES")]






