

#CODE added to and adopted from Xu BaBA papers (Xu et al. 2021)
#altered by Leyna Stemle

###SPLIT LINES FOR BETTER REP

##split roads first so that they are not just segmented based on road name
library(sf)
st_crs(shp_roadSouth)#make sure crs is the same between road file and baba files
class(shp_roadSouth)
st_sfc

st_geometry_type(shp_roadSouth)
# Assume 'roads_sf' is your sf object with a LINESTRING geometry
# and 'split_distance' is the desired distance to split the roads by (e.g., XX meters)
distance <-2000
split_distance <-2000

library(dplyr)
library(lwgeom)
library(purrr)


# Assuming your `roads` object has an ID column, e.g., `road_id`
# 2. Add original ID and cast to LINESTRING (splits MULTILINESTRINGs)
roads_linestring <- shp_roadSouth%>%
  mutate(original_id = ObjectID_1) %>%
  st_cast("LINESTRING", group_or_split = TRUE)

# 3. Add vertices every 2000 meters
roads_segmentized <- st_segmentize(roads_linestring, dfMaxLength = 2000)

# 4. Split each linestring into segments between vertices
split_line <- function(geometry) {
  coords <- st_coordinates(geometry)
  if (nrow(coords) < 2) return(NULL) # skip empty or invalid
  segments <- map2(1:(nrow(coords) - 1), 2:nrow(coords), ~{
    st_linestring(rbind(coords[.x, 1:2], coords[.y, 1:2]))
  })
  st_sfc(segments, crs = st_crs(geometry))
}

# 5. Apply splitting across all lines
split_segments_list <- roads_segmentized %>%
  mutate(road_index = row_number()) %>%
  rowwise() %>%
  mutate(geometry_list = list(split_line(geometry))) %>%
  ungroup() %>%
  filter(!sapply(geometry_list, is.null)) %>%
  select(road_index, original_id, geometry_list) %>%
  tidyr::unnest(geometry_list) %>%
  mutate(geometry = geometry_list) %>%
  st_as_sf() %>%
  select(-geometry_list)

# 6. Add unique segment ID
split_segments <- split_segments_list %>%
  mutate(segment_id = row_number())

saveRDS(split_segments, "south_roads_split.rds")

st_write(split_segments, "south_roads_split.shp")
split_segments<-readRDS("south_roads_split.rds")
class(split_segments)
st_crs(split_segments)

#Baba function
range01 <- function(x){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

##Ranks from Xu papers - origionally Xu et al. 2021
Barrier_ID <- fences$FID_Fence0
Rank <- BaRanking(pronghorn_BaBA, fences, d, Barrier_ID, min_total_enc = 0, 
                  index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped)/total_enc)*unique_ind), 
                  show_plot = F)
getwd()
setwd("F:/gbr_postdoc/data_TON")

#save
library(writexl)
write_rds(TON_BaBA_50_mc0, "BABATON_50_mc0.rds")

TON_BaBA_50_mc0df<- as.data.frame(TON_BaBA_50_mc0)
write.csv(TON_BaBA_50_mc0df, "BaBa_TON_50_mc0.excel")

#trying on site data specifically 
getwd()
setwd("G://postdoc_GBR//scripts to look at//BaBA_dryad_data")

?BaRanking
d<-50 #distance in Meters

Rank_SIUC <- BaRanking(Dsiuc_BaBA_50$classification,split_segments, d, Barrier_ID = "segment_id", min_total_enc = 2, 
                      index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped+Quick_Cross)/total_enc)*unique_ind), 
                      show_plot = T)

#no quick cross - orginial rank equation
Rank_SIUC2 <- BaRanking(Dsiuc_BaBA_50$classification,split_segments, d, Barrier_ID = "segment_id", min_total_enc = 2, 
                       index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped)/total_enc)*unique_ind), 
                       show_plot =  F)

BaRanking()
write.csv(Rank_SIUC2, "RankSIU_splitnoqc.csv")
?write_rds()
library(tidyverse)
write_rds(Rank_SIUC2, "RankSIU_splitnoqc.rds")
RankTON_splitnoqc<-readRDS("F:/gbr_postdoc/data_TON/RankTON_splitnoqc.rds")
 
Rank_SIUC2 <-read_rds("RankSIU_splitnoqc.rds")
Rank_SIUC <- BaRanking(Dsiuc_BaBA_50$classification,shp_roadSouth, d, Barrier_ID = "ObjectID_1", min_total_enc = 2, 
                       index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped+Quick_Cross)/total_enc)*unique_ind), 
                       show_plot = T)

Rank_SIUC <- BaRanking(TON_BaBA_50_mc0$classification,Sroads, d, Barrier_ID = "LINEARID", min_total_enc = 2, 
                      index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped)/total_enc)*unique_ind), 
                      show_plot = T)
#plot
library(mapview)
mapview(Rank_TON['index'])
mapview(RankTON_splitnoqc['index'])
RankTON <- RankTON_splitnoqc#make shorter so the legend is smaller

#add animal crashes over it - old ones
setwd("~/postdoc_gbr/Deer/spatial")
Animal_crashes<-read_sf("Animal_crashes.shp")
st_crs(Animal_crashes)
class(shp_roadSouth)

st_crs(RankTON_splitnoqc)
#try again with right projection
setwd("F:/gbr_postdoc/GIS layers")
Animal_crashes<-read_sf("Animal_crashes84.shp")

mapview(RankTON['index'])+mapview(Animal_crashes)#display


#SIUC site
setwd("F:/gbr_postdoc/BaBA_paper/final/from one drive remote in/SIU_final")
RankSIU <- readRDS("RankSIU_splitnoqc.rds")

mapview(RankSIU['index'])+mapview(Animal_crashes)#display



#looks like need to move the function too not just expression
hist(Rank_SIUC2$index)
plot(Rank_SIUC2['index'])

RankSIUC3<-na.omit(Rank_SIUC2)
write.csv(RandSIUC2, "RankSIU_splitnoqc_naomit.csv")
RandSIUC2<-st_drop_geometry(RankSIUC3)
getwd()

#make sure geometry does not save weird
RankTON<-na.omit(RankTON_splitnoqc)
write.csv(Rank_TON2, "RankTON_splitnoqc_naomit.csv")
Rank_TON2<-st_drop_geometry(RankTON)

RankSHE<-na.omit(RankShe_splitnoqc)
write.csv(RankShe_splitnoqc, "RankShe_splitnoqc_naomit.csv")
RankShe_splitnoqc<-st_drop_geometry(RankSHE)


allrank <- read.csv("Rankall_splitnoqc_naomit.csv")


library(ggplot2)
# density plots
#this one
p <- ggplot(allrank, aes(x=index, fill =site)) + 
    geom_density(alpha=0.6)+
  labs(title="Impermeability index density curve",x="Road impermeability index", y = "Density")

p+scale_fill_brewer(palette = "Dark2") +scale_color_brewer(palette = "Dark2") #or BrBG
p+scale_fill_brewer(palette = "BrBG") +scale_color_brewer(palette = "BrBG") 


# Change line color and fill color
ggplot(RankSHE, aes(x=index))+
  geom_density(color="darkblue", fill="lightblue")

mapview(Rank_SIUC2['index'])

RankTON_splitnoqc <- read_rds("RankTON_splitnoqc.rds")
RankShe_splitnoqc <- read_rds("RankShe_splitnoqc.rds")
hist(RankTON_splitnoqc$index)
hist(RankShe_splitnoqc$index)

library(mapview)
mapview(RankShe_splitnoqc['index'])

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### index
## default index_fun is to calculate the index used in Xu et al. 2021
BaRanking <- function(classification, barrier, d, Barrier_ID, min_total_enc = 0, 
                      index_fun = expression(((Bounce + Back_n_forth + Trace + Trapped)/total_enc)*unique_ind), 
                      show_plot = F) {
 
  Barrier_ID <- rlang::sym(Barrier_ID)
  
  ## make classification spatial
  classification_sf <- sf::st_as_sf(classification, coords = c("easting", "northing"), crs = sf::st_crs(barrier))
  
  ## spatial join by the rd buffer distance used in BaBA()
  barrier_sf_joined <- sf::st_join(barrier, classification_sf, join = sf::st_is_within_distance, dist = d)
  
  ## calculate # of each encounter event types
  by_type <- 
    barrier_sf_joined %>%
    ## only keep those with animal encounters
    dplyr::filter(!is.na(barrier_sf_joined$AnimalID)) %>% 
    ## The bang-bang operator !! forces a single object. One common case for !! is to substitute an environment-variable (created with <-) with a data-variable (inside a data.frame).
    dplyr::group_by(!!Barrier_ID, eventTYPE, .drop = F) %>%    
    dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>% 
    ## No longer want a spatial object
    sf::st_drop_geometry() %>% 
    ## change to wide format with one column per fence segment x eventTYPE
    tidyr::pivot_wider(names_from = 'eventTYPE', values_from = 'count') %>%
    replace(is.na(.), 0)
  
  ## ensure all behaviore types are listed in by_type, filling in 0s for any that are missing
  ba.all <- c("Quick_Cross", "Bounce", "Average_Movement", "Back_n_forth", "Trace", "Trapped", "unknown") # a full list of names
  ba.miss <- ba.all[!(ba.all %in% names(by_type))]
  add.df <- as.data.frame(matrix(0, nrow = nrow(by_type), ncol = length(ba.miss)))
  colnames(add.df) <- ba.miss
  by_type <- cbind(by_type, add.df)
  
  ## calculate # of unique individuals encountering each fence
  by_ID <- 
    barrier_sf_joined %>% 
    dplyr::filter(!is.na(barrier_sf_joined$AnimalID))  %>% 
    dplyr::group_by(!!Barrier_ID, .drop = F) %>%
    dplyr::summarise(unique_ind = length(unique(AnimalID)), .groups = 'drop') %>% 
    sf::st_drop_geometry()
  
  ## combine the two tibbles
  barrier_encounters <- 
    by_type %>% 
    dplyr::left_join(by_ID, by = rlang::as_name(Barrier_ID)) %>% 
    replace(is.na(.), 0) %>%
    dplyr::mutate(
      ## calculate total encounters
      total_enc = Bounce + Quick_Cross + Average_Movement + Back_n_forth + Trace + unknown + Trapped,
      ## calculate the impermeability index based on the user-set expression for all fence segments with sufficient encounters (total_enc >= min_total_enc).
      ## this must be split in two steps so that rescaling the index only considers values where total_enc >= min_total_enc.
      calc_expr = dplyr::if_else(total_enc >= min_total_enc, eval(index_fun), NA),
      index = range01(calc_expr)) %>% 
    ## calc_expr is no longer needed  
    dplyr::select(-calc_expr)
  
  ## put back into spatial format
  barrier_encounters_sf <- merge(barrier, barrier_encounters, by = rlang::as_name(Barrier_ID), all.x = TRUE)
  
  if(show_plot) {
    plot(sf::st_geometry(barrier_encounters_sf), col = "grey")
    plot(barrier_encounters_sf['index'], add = TRUE)
  }
  
  return(barrier_encounters_sf)
  
}
