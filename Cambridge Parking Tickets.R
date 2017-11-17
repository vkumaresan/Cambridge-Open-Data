# Cambridge Open Data- Cambridge Parking Tickets
### Viggy Kumaresan###

#Import CSV File From Open Data Portal

#Import packages
library(stringr)
library(ggmap)
library(ggplot2)

#Extract and split lat/long values using gsub and str_split

# use gsub to create extracted long_lat variable
# use str_split to split the longitude and latitude

split_coords <- str_split(string = Cambridge_Parking_Tickets$long_lat, pattern = ',', n = 2, simplify = TRUE)

split_coords <- as.data.frame(split_coords)
split_coords$lat <- as.numeric(as.character(split_coords$V1))
split_coords$long <- as.numeric(as.character(split_coords$V2))

str(split_coords)

Cambridge_Parking_Tickets$long <- split_coords$lat
Cambridge_Parking_Tickets$lat <- split_coords$long

split_coords <- as.data.frame(split_coords)
split_coords$lat <- as.numeric(as.character(split_coords$V1))
split_coords$long <- as.numeric(as.character(split_coords$V2))

# Get Cambridge map(optional, for later R visualization. Use Tableau for now)
cambridge.limits <- geocode(c("Cambridge, USA"))
cambridge.limits

mapcambridge <- get_map(location = c(lon = mean(split_coords$long), lat = mean(split_coords$long)), zoom = 225,
                      maptype = "satellite", scale = 10)

ggmap(mapcambridge) +
  geom_point(data = split_coords, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

# Exploratory Analysis
table(Cambridge_Parking_Tickets$`Violation Description`)

library(ggplot2)
Cambridge_Parking_Tickets$`Violation Description` <- as.factor(Cambridge_Parking_Tickets$`Violation Description`)
ggplot(data=Cambridge_Parking_Tickets) + geom_bar(mapping = aes(x = `Violation Description`))


# Analysis of Squares
harvard <- subset(Cambridge_Parking_Tickets, (long > 42.37 & long < 42.38) & (lat > -71.12 & lat < -71.11))
View(harvard)
table(harvard$`Violation Description`)

central <- subset(Cambridge_Parking_Tickets, (long > 42.36 & long < 42.37) & (lat > -71.105 & lat < -71.104))
table(central$`Violation Description`)

porter <- subset(Cambridge_Parking_Tickets, (long > 42.38 & long < 42.39) & (lat > -71.13 & lat < -71.12))
table(porter$`Violation Description`)

kendall <- subset(Cambridge_Parking_Tickets, (long > 42.36 & long < 42.37) & (lat > -71.10 & lat < -71.09))
table(kendall$`Violation Description`)

davis <- subset(Cambridge_Parking_Tickets, (long > 42.39 & long < 42.40) & (lat > -71.13 & lat < -71.12))
table(davis$`Violation Description`)



# Write CSV files for Tableau Visualization
write.csv(split_coords, file='/Users/Viggy/Desktop/Data Science/R/Cambridge Open Data/Cambridge_Parking_Tickets_splitcoords.csv')

write.csv(Cambridge_Parking_Tickets, file='/Users/Viggy/Desktop/Data Science/R/Cambridge Open Data/Cambridge_Parking_Tickets_longlat.csv')


