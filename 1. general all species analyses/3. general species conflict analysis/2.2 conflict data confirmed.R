# Filter conflict data for analysis: ----------------------------------------

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)

# Bring in our previous Data --------------------------------------------
conflict.bhb <- st_read("data/processed/conflict_bhb_no_filt.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
can.ccs.shp<- st_make_valid(st_read("Data/original/lccs000b21a_e.shp"))

# Filter data to confirmed and probable occurrences (let's see if this is enough):
# Just confirmed and probable:
conflict.data.conf <- dplyr::filter(conflict.bhb, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE")

sum(conflict.data.conf$OCC_SPECIES == "BLACK BEAR") #  175 b bear
sum(conflict.data.conf$OCC_SPECIES == "WOLF") # 21 wolf
sum(conflict.data.conf$OCC_SPECIES == "COUGAR") # 36 cougar

# See if we can further expand this: NOTE: may use this later on if we bigger samples for running models
conflict.data.expanded <- dplyr::filter(conflict.bhb, OCC_VALIDITY_INFORMATION == "CONFIRMED" | OCC_VALIDITY_INFORMATION == "PROBABLE" | OCC_VALIDITY_INFORMATION == "CANNOT BE JUDGED")

sum(conflict.data.expanded$OCC_SPECIES == "BLACK BEAR") #  199 b bear
sum(conflict.data.expanded$OCC_SPECIES == "WOLF") # 29 wolf
sum(conflict.data.expanded$OCC_SPECIES == "COUGAR") # 82 cougar

conflict.data.missing <- dplyr::filter(conflict.bhb, OCC_VALIDITY_INFORMATION == "" | OCC_VALIDITY_INFORMATION == "DOUBTFUL" | OCC_VALIDITY_INFORMATION == "CANNOT BE JUDGED")

# Try verifying points by proximity to confirmed ones ---------------------
unique(conflict.bhb$OCC_SPECIES)

# Pull out specific species confirmed and unconfirmed reports
# bears
conf.bears <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPECIES == "BLACK BEAR")
missing.bears <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPECIES == "BLACK BEAR")
# wolves
conf.wolves <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPECIES == "WOLF")
missing.wolves <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPECIES == "WOLF")
# cougar
conf.cougar <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPECIES == "COUGAR")
missing.cougars <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPECIES == "COUGAR")

# buffer to make a 5km radius
bears.buf <- st_buffer(conf.bears, 5000)
wolves.buf <- st_buffer(conf.wolves, 10000)
cougars.buf <- st_buffer(conf.cougar, 5000)

# See how many fall within the buffer of confirmed reports
# bears
bears.within <- st_intersection(missing.bears, bears.buf)
bears.within <- bears.within %>% distinct(OCC_FILE_NUMBER, .keep_all = TRUE) #rid of duplicates - now 409
bears.within <- bears.within %>% 
  dplyr::select(., -c(18:35))

# wolves
wolves.within <- st_intersection(missing.wolves, wolves.buf)
wolves.within <- wolves.within %>% distinct(OCC_FILE_NUMBER, .keep_all = TRUE) #rid of duplicates - now 409
wolves.within <- wolves.within %>% 
  dplyr::select(., -c(18:35))

# cougar
cougar.within <- st_intersection(missing.cougars, cougars.buf)
cougar.within <- cougar.within %>% distinct(OCC_FILE_NUMBER, .keep_all = TRUE) #rid of duplicates - now 409
cougar.within <- cougar.within %>% 
  dplyr::select(., -c(18:35))

# DOUBLE CHECK
plot(st_geometry(conf.bears), col = "black")
plot(st_geometry(bears.within), col = "red", add=TRUE)

# re-label the data-frame inputs
bears.within$OCC_VALIDITY_INFORMATION <- "PROBABLE"
wolves.within$OCC_VALIDITY_INFORMATION <- "PROBABLE"
cougar.within$OCC_VALIDITY_INFORMATION <- "PROBABLE"

# JOIN these back into "confirmed" dataset
bears.conf.join <- rbind(conflict.data.conf, bears.within) # join points
wolf.conf.join <- rbind(bears.conf.join, wolves.within) # join points using previous df to build
cougar.conf.join <- rbind(wolf.conf.join, cougar.within) # join points using previous df to build

