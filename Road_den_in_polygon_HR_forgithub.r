# How to compute and map rail density using R
# Adapted from Milos Popovic github

#load libraries
library(plyr, quietly=T)
library(tidyverse, quietly=T) 
library(sf, quietly=T)
library(ggplot2, quietly=T) 
library(dplyr, quietly=T)
library(classInt, quietly=T)

set.seed(20210523)


#start!!!!!!!!!!!!!!!!!!!!!!!!!!!

#use USGS roads

#create copies
r <- shp_roadsN #roads
ind <- st_read("homerange95_she.shp")#use shapefile not sp, home ranges
#ind <- as.data.frame( Khref952)

crs(Khref952) #making sure they are in same crs
crs(r)
class(Khref95) 
# intersect rails by subdistrict polys and compute length
ints <- st_intersection(r, ind) %>% 
        dplyr::mutate(len_m = sf::st_length(geometry)) %>% # returns length in meters
        dplyr::group_by(id)

int <- as.data.frame(as.matrix(ints)) # place it into data.frame
int$len_m <- as.numeric(as.character(int$len_m)) #length must be numeric
int$id <- as.character(int$id) #subdistrict code must be string
ii <- ddply(int, "id", summarise, lr_m = sum(len_m, na.rm = TRUE)) #aggregate length by subdistrict

# join  by animal id code
df <- merge(ind, ii, by='id', all.x=T)

df$lr_km <- df$lr_m/1000 #st_area returns square meters so we get square km by dividing the result by 1000 squared

# let's calculate den size in square kilometers
df$rdkm_sqkm <- df$lr_km / df$area 

#export
write.csv(df, "She_rdkm_sqkm.csv")

#get some stats
mean(df$rdkm_sqkm)
sd_value <- sd(df$rdkm_sqkm)#std dev
n <-length(df$rdkm_sqkm)
se <- sd_value/sqrt(10)
se


#do for TON site too
#!!!!!!!!!!!!!!!!!!!!!!!!!!!

#use USGS roads 

#create copies
r <- Sroads_utm
ind <- st_read("homerange95_TON.shp")#use shapefile not sp
ind <- st_transform(ind, crs = 32616) #transform to make it in same crs
#ind <- as.data.frame( Khref952)

crs(ind)
crs(r)
class(Khref95)
# intersect rds by hr polys and compute length
ints <- st_intersection(r, ind) %>% 
        dplyr::mutate(len_m = sf::st_length(geometry)) %>% # returns length in meters
        dplyr::group_by(id)



int <- as.data.frame(as.matrix(ints)) # place it into data.frame
int$len_m <- as.numeric(as.character(int$len_m)) #length must be numeric
int$id <- as.character(int$id) #subdistrict code must be string
ii <- ddply(int, "id", summarise, lr_m = sum(len_m, na.rm = TRUE)) #aggregate length by subdistrict

# join  by id code
df <- merge(ind, ii, by='id', all.x=T)

#change to km
df$lr_km <- df$lr_m/1000 #st_area returns square meters so we get square km by dividing the result by 1000 squared

# let's calculate den in square kilometers
df$rdkm_sqkm <- df$lr_km / df$area 

### Select relevant meta columns --ADJUST BELOW--  
#need to modify so multipolygon doesn't list geometry 
#df2 <-df[,c("id", "area","lr_km", "lr_m", "rdkm_sqkm")]
#df2 <-as.data.frame(df)   #these still add multipoly
write.csv(df2, "TON_rdkm_sqkm.csv")
write_rds(df2, "TON_rdkm_sqkm.rds")


#stats
mean(df$rdkm_sqkm)
sd_value <- sd(df$rdkm_sqkm)#std dev
n <-length(df$rdkm_sqkm)
se <- sd_value/sqrt(10)
se
min(df$rdkm_sqkm)
max(df$rdkm_sqkm)
