# This code will organize the RAMS VRS Messenger dataset into a more workable format
# Code developed by John Cabiles


library(tidyr)
library(dplyr)
library(zoo) # makes it easier to work with time series data
library(xts) # makes it easier to work with time series data
library(ggmap) # gets GPS coordinates from addresses


# import vrs data
input_file <- read.csv("/Users/John/dataprojects/VRS Messenger Run Log/Messenger Run Log Statistics (2018-05-12).csv",
         check.names = FALSE)

# import addresses
input_file_address <- read.csv("/Users/John/dataprojects/VRS Messenger Run Log/siteaddresses2.csv",
                               check.names = FALSE, stringsAsFactors = FALSE)
# remove NA columns
input_file_address <- input_file_address[,colSums(is.na(input_file_address))<nrow(input_file_address)]



## convert cross table into list view
## more info here: https://mgimond.github.io/ES218/Week03b.html
messenger.data <- gather(input_file, key = "Date", value="Pickup_Status", -1)



## create column for pickup_needed
messenger.data$Pickup_Needed[messenger.data$Pickup_Status==0] <- 1
messenger.data$Pickup_Needed[messenger.data$Pickup_Status==1] <- 1
messenger.data$Pickup_Needed[is.na(messenger.data$Pickup_Needed)] <- 0



### make sure each column is the correct data type

#converts 0s and 1s into numbers instead of strings
messenger.data$Pickup_Needed <- as.numeric(as.character(messenger.data$Pickup_Needed))

# makes it easier to work with dates (especially for weekday() function)
messenger.data$Date <- strptime(messenger.data$Date, "%m/%d/%y")  

# add day of the week column
messenger.data$DayofWeek <- weekdays(as.Date(messenger.data$Date))

# add column that labels each wee (Week 1, Week 2, etc.)
messenger.data$Week <- findInterval(as.Date(messenger.data$Date), seq(from=as.Date(messenger.data$Date[1]),
                                                                      to=as.Date(messenger.data$Date[length(messenger.data$Date)]),
                                                                      by="1 week"))

# add column for months and years
messenger.data$Month <- months(messenger.data$Date)





##### Analyze % of Successful Pickups (when pickups were actually needed) #####

successful <- messenger.data[messenger.data$Pickup_Needed==1,]
successful$Pickup_Status <- as.numeric(as.character(successful$Pickup_Status)) #convert into numeric for calculation



# proportion of successful pickups
sum(successful$Pickup_Status)/sum(successful$Pickup_Needed)


### proportion of successful pickups by location
location.success <- aggregate(cbind(successful$Pickup_Status,
                                    successful$Pickup_Needed) ~ Location, successful, sum)
colnames(location.success) <- c("Location", "Pickups_Successful", "Pickups_Needed")
#add proportion column
location.success$Percentage_Success <- location.success$Pickups_Successful/location.success$Pickups_Needed




#### Add Geolocation based on Address ####

# add address to sites
location.success$Address <- input_file_address$Address[match(location.success$Location,input_file_address$Location)]

# add GPS coordinates based on address
for (i in 1:nrow(location.success))
{
  result <- geocode(location.success$Address[i], output = "latlona", source="google")
  location.success$lon[i] <- as.numeric(result[1])
  location.success$lat[i] <- as.numeric(result[2])

}




### proportion of successful pickups by day of the week
dayofweek.success <- aggregate(cbind(successful$Pickup_Status,
                                     successful$Pickup_Needed) ~ DayofWeek, successful, sum)
# organize in order of day of the week
dayofweek.success$DayofWeek <- factor(dayofweek.success$DayofWeek, levels= c("Monday", "Tuesday", 
                                                         "Wednesday", "Thursday", "Friday"))
colnames(dayofweek.success) <- c("DayofWeek", "Pickups_Successful", "Pickups_Needed")
# add proportion column
dayofweek.success$Percentage_Success <- dayofweek.success$Pickups_Successful/dayofweek.success$Pickups_Needed


### proportion of successful pickups BY WEEK
week.interval <- aggregate(cbind(successful$Pickup_Status,
                                     successful$Pickup_Needed) ~ Week, successful, sum)
colnames(week.interval) <- c("Week", "Pickups_Successful", "Pickups_Needed")
# add proportion column
week.interval$Percentage_Success <- week.interval$Pickups_Successful/week.interval$Pickups_Needed


# add first date of each week
#week.interval$Date <- messenger.data$Date


month.interval <- aggregate(cbind(successful$Pickup_Status,
                                  successful$Pickup_Needed)
                            ~ Month, successful, sum)
colnames(month.interval) <- c("Month", "Pickups_Successful", "Pickups_Needed")
month.interval$Pct_Success <- month.interval$Pickups_Successful/month.interval$Pickups_Needed





#### summarize data by location vs. dayoftheweek ####
# use dplyr
loc.by.day <- successful[,c(1,3,5)]
loc.by.day <- loc.by.day %>% group_by(Location,DayofWeek) %>%
              summarize(Number_of_Pickups = sum(Pickup_Status))

loc.by.day$AvgWeeklyPickups <- loc.by.day$Number_of_Pickups/length(unique(messenger.data$Date))*5




View(messenger.data)
View(successful)
View(location.success)
View(week.interval)
View(month.interval)
View(dayofweek.success)
View(loc.by.day)



#### Average Number of Pickups per week ####
average <- location.success[,c(1,2)]
average$AvgWeeklyPickups <- average$Pickups_Successful/length(unique(messenger.data$Date))*5


##### Export Data #####

write.csv(loc.by.day, file = "/Users/John/dataprojects/VRS Messenger Run Log/loc_by_dayofweek.csv")
write.csv(location.success, file = "/Users/John/dataprojects/VRS Messenger Run Log/location.csv")
