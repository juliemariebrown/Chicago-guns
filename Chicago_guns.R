setwd("/Users/brown/Documents/Berkeley/dataviz-fall-2013/Chicago-guns")

library(maptools)

library(RColorBrewer)
 
get_second_element <- function(item) {
  return (item[2])
}
 
get_first_element <- function(item) {
  return (item[1])
}
 
# load the data
data <- read.delim("county-data.txt", header=F, stringsAsFactors=F)
 
# rename it like a human
names(data) <- c("county_orig", "guns_orig")
 
# split it up based on parenthesis
split <- strsplit(data$county_orig, split="\\(")
 
#make a new field for state
data$state_clean <- sapply(split, get_second_element)
 
#clean guns
data$state_clean <- gsub("\\)", "", data$state_clean)
 
#make a new county
data$county_clean <- sapply(split, get_first_element)
 
#clean guns
data$guns_clean <- as.numeric(gsub(",", "", data$guns_orig)) 
 
#by state
agg <- aggregate(data$guns_clean, list(data$state_clean), sum)

#Rename fields 
names(agg) <- c("STATE_ABBR", "total_guns")

#make a map
states <- readShapePoly("nytlayout_state/nytlayout_state.shp")

#storing data from shape file
map_data <- data.frame(states)

#Strip out PR and Guam
agg <- subset(agg, STATE_ABBR%in%map_data$STATE_ABBR)

#Merged data
 merged_geo_data <- merge(map_data, agg, by="STATE_ABBR")

 #match order
 match_order <- match(map_data$STATE_ABBR, merged_geo_data$STATE_ABBR)

#reorder match 
 merged_geo_data <- merged_geo_data[match_order,]

#assign break values
break_values <- c(0, 500, 1000, 3000, 5000, 30000)

#merged geo data map bucket values
merged_geo_data$bucket <- as.numeric(cut(merged_geo_data$total_guns, breaks=break_values))

#assign color
mycols <- brewer.pal(5, "Greens")

#plot the map with the colors
plot(states, col=(mycols[merged_geo_data$bucket]))
