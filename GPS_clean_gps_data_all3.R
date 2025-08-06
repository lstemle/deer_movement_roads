# GPS data cleaning prototype script
# Updated 2024-07-02
# Original author: Dani Berger (danielle.berger@usu.edu)
# Adapted by Kezia Manlove (kezia.manlove@usu.edu) for SCV2 data structure
#recieved from Tadao

# Script overview: 
# (Each section can be accessed using navigation drop-down located in the slider
# between this pane and the panel below)

# I. Load required libraries and data
# II. Specify cleaning parameters
# III. Set up data and reformat for AMT
# IV. AMT cleaning steps
### A. Remove fixes close to time of capture
### B. Remove fixes with high DOP
### C. Flag duplicates
### D. Flag Fast Steps
### E. Flag Fast Roundtrips
### F. Remove flagged rows
# V. Convert the dataset to a df and save as csv

setwd("C:/Users/Tadao/OneDrive - Southern Illinois University/Documents/Recreation Project/analysis/ch1/data")


# I. Load required libraries and data ----
library(devtools)
#devtools::install_github("jmsigner/amt") # pull in dev version of amt,
## which has better cleaning functions (esp the fast round trips function)
library(amt)
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(leaflet)
getwd()
# Read in gps locations dataset, labeled with deployment-tagged animal IDs
# (deployment data is ALREADY incorporated into this dataset)
#locs_in <- readRDS(file = "lasals_locations_20240702.rds")
locs_in <- read.csv("USDA_deer_Aug24_clean_all_TON2.csv")  #open gps file
locs_in <-data #straight from pullling
#read in reference data
ref_in <- read.csv("deer_meta.csv")  
#merge gps and reference data
locs_in <- merge(locs_in, ref_in, by.x="usdaID", by.y="usdaID")

table(locs_in$ID_year, locs_in$DeviceID)
dat_in <- locs_in 

#if skipping merge 
#dat_in <- webshelbydeer

# II. Specify cleaning parameters ----
# epsg codes for lat long and local UTMs
latlong_epsg <- 4326 # universal lat-long epsg code -correct IL
utm_epsg <- 32616 # SITE-SPECIFIC UTM epsg code -- REVISE FOR EACH SITE. # is WGS 72 BE 32416
# cut_days is days to cut off of trajectory after capture (i.e., capture-mediated movement)
cut_days <- 1
# value to replace DOP NAs with
dop_highval <- 25
# number of minutes between fixes to call a fix a "duplicate"
duplicate_interval_mins <- 10
# speed that classifies as step as "too fast" in kilometers / hr
speed_kph <- 3
# epsilon value for fast round-trips (default is 10 and that worked well for Utah)
epsilon_in <- 10
# specify path to directory for storing individual csvs
output_path <- "F://gbr_postdoc//data//final//"
getwd()

#dat_in<-
# III. Set up data and reformat for AMT ----
## A. Generic specification of sf object with lat-long ----
dat_in_sf <- st_as_sf(x = dat_in,
                      coords = c("Longitude.x", "Latitude.x"),
                      crs = latlong_epsg) 

# transforming the data from geographic coordinate system to projected coordinate system (UTMs)
dat_in_sf <- st_transform(dat_in_sf, crs = st_crs(utm_epsg))

# Make the point geometry into two separate coordinate columns
dat_in_coords <- as.data.frame(st_coordinates(dat_in_sf))

dat_in_sf$collarID_char <- as.character(dat_in_sf$DeviceID)
dat_in_sf$.x <- dat_in_coords$X
dat_in_sf$.y <- dat_in_coords$Y

#make datetime a POSIXct
dat_in_sf$timestamp <- as.POSIXct(dat_in_sf$date_gmt, format="%Y-%m-%d %H:%M:%S", tz="GMT", na.rm=TRUE)
#the data is in GMT/UTC but the deer feel Central...

#remove NAs
dat_in_sf <- dat_in_sf[!(is.na(dat_in_sf$timestamp)), ]
str(dat_in_sf)

## B. Reformat the data for AMT ----
# Make GPS data into a track for AMT
data_track <- dat_in_sf %>% 
  make_track(.x = .x, 
             .y = .y, 
             .t = timestamp, # previously "dt" 
             crs = utm_epsg, # Recall, geometry for this dataset is now in UTMs (this is the epsg)
             dop = PDOP, # 
             animal_id = ID.1, # Utah animal ID field is USU_ID in the native dataset
             age = Age.x,
             collar = DeviceID,
             collar_deployment_date = Capture_date.x, 
             collar_end_date = Fate_date.x
  )

# Group the track by animal_id and arrange chronologically within each individual
data_track <- data_track %>% 
  mutate(animal_id = as.factor(animal_id)) %>%
  group_by(animal_id) %>% 
  arrange(t_, .by_group = TRUE) %>% 
  ungroup()

# nest tracks by animal_id
data_track_nest <- data_track %>% nest(data = -"animal_id")

# check the data for the first individual
data_track_nest$animal_id[[1]]
#129 animals but need to remove those added in 2025


# IV. AMT cleaning steps ----
## A. Remove fixes close to time of capture ----
# Remove all fixes for the first cut_days following capture.
data_track_nest <- data_track_nest %>%
  mutate(data = map(data, function(x)
    x %>% remove_capture_effect(start = days(cut_days))))

# R won't accept NAs in the DOP column
# replace NA DOPs with dop_highval
data_track_nest1a <- data_track_nest %>% 
  mutate(data = map(data, function(x)
    x %>% replace_na(list(dop = dop_highval))))

## B. Flag duplicates (multiple fixes within 10 minutes) ----
data_track_nest1 <- data_track_nest1a %>%
  mutate(data = map(data, function(x)
    x %>% flag_duplicates(gamma = minutes(duplicate_interval_mins))))

# (should have an occasional true in at least a few animals)
table(data_track_nest1$data[[1]]$duplicate_)
table(data_track_nest1$data[[47]]$duplicate_)
table(data_track_nest1$data[[2]]$duplicate_)
table(data_track_nest1$data[[3]]$duplicate_)
table(data_track_nest1$data[[4]]$duplicate_)
table(data_track_nest1$data[[5]]$duplicate_)
table(data_track_nest1$data[[6]]$duplicate_)
table(data_track_nest1$data[[7]]$duplicate_)
table(data_track_nest1$data[[8]]$duplicate_)
table(data_track_nest1$data[[9]]$duplicate_)
table(data_track_nest1$data[[10]]$duplicate_)
table(data_track_nest1$data[[11]]$duplicate_)

# remove temporal duplicates
data_track_nest2 <- data_track_nest1 %>%   
  mutate(data = map(data, function(x)
    x %>% filter(duplicate_ == FALSE)))
nrow(do.call("rbind", data_track_nest1$data)) # number of original points
nrow(do.call("rbind", data_track_nest2$data)) # number of points after removing duplicates- 
#removed  50,000 points

## C. Flag Fast Steps ----
# flag all steps that are faster than speed_kph for removal
delta <- calculate_sdr(speed = speed_kph, # speed given in km/hr # Changed back to 3; may need something different for other sites
                       time = minutes(60),speed_unit = ("km/h"))
#TRYING TO FIX ROUND TRIP, ADDING SPPED UNIT, DOESN'T CHANGE DELTA
# Assuming speed is provided in km/h.
data_track_nest3 <- data_track_nest2 %>%
  mutate(data = map(data, function(x)
    x %>% flag_fast_steps(delta = delta)))


table(data_track_nest3$data[[1]]$fast_step_)
table(data_track_nest3$data[[11]]$fast_step_)
table(data_track_nest3$data[[21]]$fast_step_)
table(data_track_nest3$data[[23]]$fast_step_)
table(data_track_nest3$data[[22]]$fast_step_)
#very few, 1 or none


## D. Flag Fast Roundtrips ----
# The recommended default epsilon is 10.
data_track_nest4 <- data_track_nest3 %>%
  mutate(data = map(data, function(x)
    x %>% flag_roundtrips(delta = delta, 
                          epsilon = epsilon_in, 
                          time_unit = "hours")))
table(data_track_nest4$data[[1]]$fast_roundtrip_)
table(data_track_nest4$data[[7]]$fast_roundtrip_)
table(data_track_nest4$data[[47]]$fast_roundtrip_)
table(data_track_nest4$data[[48]]$fast_roundtrip_)
#there are a lot of these... maybe that is messing up the interval

## E. Remove rows that have TRUE flags ----
### Unnest the track, and remove individuals with 0 rows first:
gps_unnest <- data_track_nest3 %>% 
  mutate(nrow = map_dbl(data, nrow)) %>% 
  filter(nrow > 0) %>% 
  # Unnest the data frame
  unnest(cols = data) %>% 
  mutate(animal_id = factor(animal_id))

### Check that the number of animals is correct:
length(levels(gps_unnest$animal_id)) # should be as long as there are animals in your dataset
#129

### ii. drop duplicates, fast steps, and fast roundtrips
gps_flagdrop <- gps_unnest %>% 
  filter(duplicate_ == FALSE & 
           fast_step_ == FALSE)
           #fast_roundtrip_ == FALSE
  
# V. Convert the dataset to a df and save as individual-specific csvs ----
gps_cleaned_df <- as.data.frame(gps_flagdrop)

# convert back to lat-long and append lat-long so that coordinates are universal. 
## A. Convert gps_cleaned_df to sf ----
gps_cleaned_sf <- st_as_sf(x = gps_cleaned_df,
                           coords = c("x_", "y_"),
                           crs = utm_epsg)
## B. Extract lat-long coordinates and append to dataset ----
gps_cleaned_latlongs <- st_transform(gps_cleaned_sf, crs = st_crs(latlong_epsg))
# third store the point geometry into two separate coordinate columns
gps_latlong_coords <- as.data.frame(st_coordinates(gps_cleaned_latlongs))
# finally append lat and long columns to df
gps_cleaned_df <- cbind(gps_cleaned_df, gps_latlong_coords)
# rename coordinate fields for clarity
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "X")] <- "Longitude"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "Y")] <- "Latitude"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "x_")] <- "UTME"
names(gps_cleaned_df)[which(names(gps_cleaned_df) == "y_")] <- "UTMN"

#removed like 300 points but did not do fast round trip drop

## C. Save csv for each individual ----
##### tadao - for some reason there is a space before and after animal id in file name when saving 
# specify output path
individs <- levels(factor(gps_cleaned_df$animal_id))
for(i in 1:length(individs)){
  k <- subset(gps_cleaned_df, animal_id == individs[i])
  write.csv(k, paste(output_path, individs[i], "_track2.csv"))
  print(i)
}

getwd()
# Save csv for full data set
write.csv(gps_cleaned_df, "all_23-25_GPS_clean.csv")
write_rds(gps_cleaned_sf,"all_23-25_GPS_clean.rds" )
write.csv(gps_unnest, "all_23-25_GPS_unestTF.csv")
write_rds(gps_cleaned_sf,"gps_cleaned_sfall_23-25_GPS_clean.rds" )
write_rds(gps_cleaned_df,"all_23-25_GPS_cleandf.rds" )
saveRDS(gps_cleaned_sf,"gps_cleaned_sfall_23-25_GPS_clean2.rds" )
getwd()
print(individs)
print(i)
print(output_path)

#2 deer
web10 <- subset(gps_cleaned_df, gps_cleaned_df$animal_id=="D1060"| gps_cleaned_df$animal_id=="D1009") 
str(gps_cleaned_sf)

individs
web <- web[,c("usdaID", "DeviceID", "RecDateTime", "Latitude.x", "Longitude.x", "Altitude", "Capture_date", "Fate_date", "Collar_make", "Collar_model", "DOP_type", "PDOP", "Species", "Study_area", "Sex", "Age")]


#check traj stuff from class
###tell R the date format

#create trajectory obj

?as.ltraj
tral <-as.ltraj(st_coordinates(gps_cleaned_sf), gps_cleaned_sf$t_, id = gps_cleaned_sf$animal_id)#doesn't work bc some duplicate
#this is with already cleaned data so should not be duplicates

#show data
data<-ld(tral)#converting new data frame with more data
head(data)

#r2n is displacement
#cannot get turning angle at first step so rel.angle is NA

##3 looks like there is a gap in the data 
#bounding box


#looking at speed

data$speed <- data$dist/data$dt #this is meters by seconds
data$speed <- data$dist/data$dt*3.6 #km/hr
hist(data$speed, main = "speed km/hr", breaks = 20)#these look okay
quantile(data$speed , na.rm = T)

##plotltr - investigate the data
plotltr(tral[1], which="dist")#one point in fall
plotltr(tral[3], which="dist")#looks good
plotltr(tral[13], which="dist")#two points in feb and two in april but not big dist 1500
plotltr(tral[31], which="dist")#looks okay
plotltr(tral[2], which="dist")#looks okay
plotltr(tral[4], which="dist")#one point in fall 2023 spikes wrong
plotltr(tral[5], which="dist")#looks okay
plotltr(tral[6], which="dist")#looks okay
plotltr(tral[7], which="dist")#looks okay
plotltr(tral[8], which="dist")#looks okay
plotltr(tral[9], which="dist")#looks okay
plotltr(tral[10], which="dist")#looks okay
plotltr(tral[11], which="dist")#looks okay
plotltr(tral[12], which="dist")#looks okay
plotltr(tral[14], which="dist")#looks okay
plotltr(tral[15], which="dist")#looks okay
plotltr(tral[16], which="dist")#looks okay
plotltr(tral[17], which="dist")#looks okay
plotltr(tral[18], which="dist")#looks okay
plotltr(tral[19], which="dist")#looks okay
plotltr(tral[20], which="dist")#looks okay
plotltr(tral[21], which="dist")#looks okay
plotltr(tral[22], which="dist")#looks okay
plotltr(tral[24], which="dist")#looks okay
plotltr(tral[25], which="dist")#looks okay
plotltr(tral[26], which="dist")#looks okay
plotltr(tral[27], which="dist")#looks okay
plotltr(tral[28], which="dist")#looks okay
plotltr(tral[29], which="dist")#NOT MOVING
individs <- levels(factor(gps_cleaned_sf$animal_id))
individs[[29]]
individs[[23]]
plotltr(tral[23], which="dist")#doesn't move until march 1, 15 days none
plotltr(tral[30], which="dist")#looks okay
plotltr(tral[31], which="dist")#looks okay
plotltr(tral[32], which="dist")#one spike in march but okay it only 1500
plotltr(tral[34], which="dist")#one spike in march 4000 m 
plotltr(tral[35], which="dist")#looks okay
plotltr(tral[36], which="dist")#looks okay
plotltr(tral[37], which="dist")#looks okay
plotltr(tral[38], which="dist")#looks okay
plotltr(tral[39], which="dist")#looks okay
plotltr(tral[33], which="dist")#a few spikes 1500 though so okay
plotltr(tral[40], which="dist")#looks okay
plotltr(tral[41], which="dist")#looks okay
plotltr(tral[42], which="dist")#looks okay
plotltr(tral[44], which="dist")#some GAPS in august to jan 
individs[[44]]
plotltr(tral[45], which="dist")#looks okay
plotltr(tral[46], which="dist")#looks okay
plotltr(tral[47], which="dist")#looks okay
plotltr(tral[48], which="dist")#looks okay
plotltr(tral[49], which="dist")#too little data probably, looks like a gap
individs[[49]]
individs[[43]]
plotltr(tral[43], which="dist")#NOT MOVING
plotltr(tral[50], which="dist")#looks okay
plotltr(tral[51], which="dist")#few spikes but all under 1500
plotltr(tral[52], which="dist")#looks okay
plotltr(tral[54], which="dist")#looks okay
plotltr(tral[55], which="dist")#looks okay
plotltr(tral[56], which="dist")#looks okay
individs[[57]]
plotltr(tral[57], which="dist")#one spike in june 10,000 m
plotltr(tral[58], which="dist")#looks okay
plotltr(tral[59], which="dist")#GAP bettween june and july
individs[[59]]
plotltr(tral[53], which="dist")#looks okay
plotltr(tral[63], which="dist")#looks okay
plotltr(tral[60], which="dist")#looks okay
plotltr(tral[61], which="dist")#looks okay
plotltr(tral[62], which="dist")#looks okay
plotltr(tral[64], which="dist")#looks okay
plotltr(tral[65], which="dist")#looks okay
plotltr(tral[66], which="dist")#looks okay
plotltr(tral[67], which="dist")#looks okay
plotltr(tral[68], which="dist")#looks okay
plotltr(tral[69], which="dist")#looks okay
plotltr(tral[73], which="dist")#looks okay
plotltr(tral[70], which="dist")#spikes at 5000 m in May or so
plotltr(tral[71], which="dist")#looks okay
plotltr(tral[72], which="dist")#looks okay
plotltr(tral[74], which="dist")#4000 m spike in Dec or so 
plotltr(tral[75], which="dist")#looks okay
plotltr(tral[76], which="dist")#looks okay
plotltr(tral[77], which="dist")#looks okay
plotltr(tral[78], which="dist")#looks okay
plotltr(tral[79], which="dist")#looks okay
plotltr(tral[83], which="dist")#looks okay
plotltr(tral[80], which="dist")#looks okay
plotltr(tral[81], which="dist")#looks okay
plotltr(tral[82], which="dist")#looks okay
plotltr(tral[84], which="dist")#looks okay
plotltr(tral[85], which="dist")#looks okay
plotltr(tral[86], which="dist")#looks okay
plotltr(tral[87], which="dist")#looks okay
plotltr(tral[88], which="dist")#gaps in fall to spring 24-25
individs[[88]]
plotltr(tral[89], which="dist")#looks okay
plotltr(tral[93], which="dist")#looks okay
plotltr(tral[90], which="dist")#3000 m spike in fall 2024
plotltr(tral[91], which="dist")#looks okay
plotltr(tral[92], which="dist")#looks okay
plotltr(tral[94], which="dist")#looks okay
plotltr(tral[95], which="dist")#looks okay
plotltr(tral[96], which="dist")#looks okay
plotltr(tral[97], which="dist")#gap in end of jan
individs[[97]]
plotltr(tral[98], which="dist")#looks okay
plotltr(tral[99], which="dist")#looks okay
plotltr(tral[103], which="dist")#looks okay
plotltr(tral[100], which="dist")#looks okay
plotltr(tral[101], which="dist")#looks okay
plotltr(tral[102], which="dist")#looks okay
plotltr(tral[104], which="dist")#looks okay
plotltr(tral[105], which="dist")#October 10,000 m
individs[[105]]
individs[[106]]
plotltr(tral[106], which="dist")#mid Jan 2025 spike 10,000 m
plotltr(tral[107], which="dist")#looks okay
plotltr(tral[108], which="dist")#looks okay
plotltr(tral[109], which="dist")#jan 2025 spike too 10000 m 
individs[[109]]
plotltr(tral[113], which="dist")#looks okay
plotltr(tral[110], which="dist")#looks okay
plotltr(tral[111], which="dist")#looks okay
plotltr(tral[112], which="dist")#looks okay
plotltr(tral[114], which="dist")#looks okay
plotltr(tral[115], which="dist")#spikes in april 8000 m
individs[[115]]
plotltr(tral[116], which="dist")#looks okay
plotltr(tral[117], which="dist")#end of jan 2025 spike 15000 m
individs[[117]]
plotltr(tral[118], which="dist")#looks okay
plotltr(tral[119], which="dist")#looks good
plotltr(tral[123], which="dist")#looks good
plotltr(tral[120], which="dist")#looks okay
plotltr(tral[121], which="dist")#looks okay
plotltr(tral[122], which="dist")#looks okay
plotltr(tral[124], which="dist")#looks okay
plotltr(tral[125], which="dist")#looks okay
plotltr(tral[126], which="dist")#looks okay
plotltr(tral[127], which="dist")#probably not enough data 
individs[[127]]
individs[[128]]
plotltr(tral[128], which="dist")#looks like a month gap 
plotltr(tral[129], which="dist")#looks okay
#

plotltr(tral[1], which="dt")##time interval
plotltr(tral[1], which="dt/3600")##time interval not distance -hrs
plotltr(tral[3], which="dt/3600")#3 big gaps in data
plotltr(tral[31], which="dt/3600")#3 big gaps in data

#turn these into bursts 
#cutltraj - create burst when data are missing
traj2<-cutltraj(tral, "dt>24*3600")#cut when missing a whole day
traj2

#round/set time 
?sett0
refda <-strptime("00:30", format = "%H:%M") #set reference data
traj3<-sett0(traj2, refda, dt = 3600, tol = 1500, units ="sec")  
traj3
head(la(traj3))

##set NA -add missing locations -interpolation 
traj4<-setNA(traj3, refda, dt= 3600, tol=300, units="sec")
traj4
is.regular(traj4)

##subsample --- not here 
?subsample

traj5<-subsample(traj4, dt=3600*2)#sample every2 hours
head(ld(traj5))


##
df3<- left_join(gps_cleaned_df, SWEL_master_data_Deer_BaBA, by = c("animal_id"), relationship = "many-to-many")

gps_cleaned_SIU <- subset(df3,df3$Study_area=="SIUC") 
getwd()
write.csv(gps_cleaned_SIU, "gps_cleaned_SIU.csv")
gps_cleaned_TON <- subset(df3,df3$Study_area=="Touch of Nature") 
write.csv(gps_cleaned_TON, "gps_cleaned_TON.csv")

gps_cleaned_She <- subset(df3,df3$Study_area=="Shelbyville") 

write.csv(gps_cleaned_She, "gps_cleaned_She.csv")
S