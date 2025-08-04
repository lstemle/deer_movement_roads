#code adapted from Brian Gerber github on Diel elephant activity
library(suncalc)
setwd("F:/gbr_postdoc/BaBA_paper")


### Add local time --NO NEED TO ADJUST IF IN ILLINOIS-- 
time.cat.date <- read.csv("time.cat.datesallBaBA.csv")
time.cat.date$date_local<-format(as.POSIXct(time.cat.date$start_time, tz="GMT"), tz="America/Chicago")

time.cat.date<-dfcam3#get for camera data

df7 <-na.omit(df6)#good
Timecat <-distinct(df7, burstID, .keep_all = TRUE) 

  time.cat.date 
#Get time period cutoffs for each date/time  using the package suncalc 
tod <- suncalc::getSunlightTimes(
  date = date(date(time.cat.date$date_local)),
  lat = 37.7,
  lon = -89.2,
  keep = c("sunrise", "night", "sunset","nightEnd"),
  tz = "America/Chicago" #make sure this is right when go through for real
)  #6 hours behind GMT 

#look at package
?getSunlightTimes

# Take these and extract the hours, minutes, and seconds    
time.subset <- data.frame(
  apply(
    tod[,4:7],
    2,
    strftime,format="%H:%M:%S"
  )
)

# Get deer times in hours, minutes, seconds
deer.times <- strftime(
  time.cat.date$date_local,
  format="%H:%M:%S"
)

# These should be the same length  
length(deer.times)==nrow(time.subset)
#> [1] TRUE

# Create a vector to store how each observation is defined- twilight, daytime, and nighttime.
time.cat <- rep(
  "nighttime",
  nrow(time.subset)
)

# Find times that are b/w dawn and dusk and define that as daytime  
index.day <- which(
  deer.times > time.subset$sunrise & deer.times < time.subset$sunset
)

# Find times that are before dawn (beginning of daytime) and after the nightEnds
index.dawn <- which(
  deer.times < time.subset$sunrise & deer.times > time.subset$nightEnd
)

# Find times that are after dusk and before the beginning of night
index.dusk <- which(
  deer.times>time.subset$sunset & deer.times<time.subset$night
)

# Change vector elements correspond to daytime and twilight  
time.cat[index.day] <- "daytime"  
time.cat[c(index.dawn,index.dusk)] <- "twilight"  

# For the entire smapling period this is the frequency of classifications, disregarding particular sampling periods and behavioral state
table(time.cat)
#> time.cat - she I think with GMT not Central
#>   daytime nighttime  twilight 
#>     11399      8549      2186
   
#time.cat siu
#time.cat
# daytime nighttime  twilight 
#7942      4701      2400
#cameras
#time.cat
#daytime nighttime  twilight 
#391        30       117 


# Create a new data.frame of dates, states, and time category and then split data into a list of elements
# Month and Year of sampling
#states = #HMM states
#could do with BaBA encounters

Month.Year <- paste(
  lubridate::month(time.cat.date$date_local),
  "/",
  lubridate::year(time.cat.date$date_local),
  sep=""
)

#here add in other variables want to keep 
time.cat.dates <- data.frame(
  Month.Year, time.cat.date$eventTYPE,time.cat.date$date_local, time.cat.date$AnimalID, time.cat.date$burstID, 
  time.cat.date$X41, time.cat.date$X81, time.cat.date$X22, time.cat.date$easting, time.cat.date$northing,
  time.cat
)

#export
write.csv(time.cat.dates, "time.cat.datesSIUBaBA.csv")

time.cat.dates<- read.csv("time.cat.datesSIUBaBA.csv")
#do all!
time.cat.dates<- read.csv("time.cat.datesallBaBA.csv")


dfcam4<- left_join(dfcam3, time.cat.dates, by = c("burstID" ="time.cat.date.burstID"), multiple = "first")

time.cat.dates$date_local <-as.POSIXct(time.cat.dates$date_local,format="%m/%d/%Y %H:%M", tz="America/Chicago")

Month <- paste(
  lubridate::month(time.cat.dates$date_local)
)

#dat.list <- split(
#  time.cat.dates$time.cat ,
#  f = as.factor(
#    paste(
 #     time.cat.dates$Month, time.cat.dates$burstID,
#      sep="-"
 #   )
 # )
#)


##SEASON _____________________________________________
#data

seasons<- select(Timecat,season, burstID )
str(Timecat)
Timecat<-na.omit(Timecat)#89 k with unique burstid

df6$date_local2 <-as.POSIXct(df6$date_local,format="%m/%d/%Y %H:%M", tz="America/Chicago")

#adapted from coauhtor Michael Egan
df6$month <- lubridate::month(df6$date_local2)
df6$season <- ifelse(df6$month<10, "postfawn", "rut")
df6$season <- ifelse(df6$month < 7, "fawn", df6$season)
df6$season <- ifelse(df6$month < 5, "prefawn", df6$season)

#runs well
#from Michael 
test4$month <- lubridate::month(test4$date)
test4$season <- ifelse(test4$month<10, "postfawn", "rut")
test4$season <- ifelse(test4$month < 7, "fawn", test4$season)
test4$season <- ifelse(test4$month < 5, "prefawn", test4$season)

#export
write.csv(test4, "all BaBA most var with seasons.csv")

