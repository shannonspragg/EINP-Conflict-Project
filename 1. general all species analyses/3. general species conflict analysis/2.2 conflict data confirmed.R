# Filter conflict data for analysis: ----------------------------------------

# Loadport Packages -------------------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(terra)
library(units)
library(dplyr)

# Bring in our previous Data --------------------------------------------
conflict.bhb <- st_read("data/processed/conflict_bhb_no_filt.shp")
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
can.ccs.shp<- st_make_valid(st_read("Data/original/lccs000b21a_e.shp"))

conflict.bhb$OCC_VAL <- conflict.bhb$OCC_VAL %>% replace_na("UNKNOWN")

# Filter data to confirmed and probable occurrences (let's see if this is enough):
# Just confirmed and probable:
conflict.data.conf <- dplyr::filter(conflict.bhb, OCC_VAL == "CONFIRMED" | OCC_VAL == "PROBABLE")

sum(conflict.data.conf$OCC_SPE == "BLACK BEAR") #  175 b bear
sum(conflict.data.conf$OCC_SPE == "WOLF") # 21 wolf
sum(conflict.data.conf$OCC_SPE == "COUGAR") # 36 cougar

# See if we can further expand this: NOTE: may use this later on if we bigger samples for running models
conflict.data.expanded <- dplyr::filter(conflict.bhb, OCC_VAL == "CONFIRMED" | OCC_VAL == "PROBABLE" | OCC_VAL == "CANNOT BE JUDGED")

sum(conflict.data.expanded$OCC_SPE == "BLACK BEAR") #  199 b bear
sum(conflict.data.expanded$OCC_SPE == "WOLF") # 29 wolf
sum(conflict.data.expanded$OCC_SPE == "COUGAR") # 82 cougar

# Look at reports without assigned validity
conflict.data.missing <- dplyr::filter(conflict.bhb, OCC_VAL == "UNKNOWN" | OCC_VAL == "DOUBTFUL" | OCC_VAL == "CANNOT BE JUDGED")

# Try verifying points by proximity to confirmed ones ---------------------
unique(conflict.bhb$OCC_SPE) # theres like 92...

# Pull out specific species confirmed and unconfirmed reports
# bears
conf.bears <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "BLACK BEAR")
missing.bears <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "BLACK BEAR")
# wolves
conf.wolves <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "WOLF")
missing.wolves <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "WOLF")
# cougar
conf.cougar <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "COUGAR")
missing.cougars <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "COUGAR")

# buffer to make a 5km radius
bears.buf <- st_buffer(conf.bears, 5000)
wolves.buf <- st_buffer(conf.wolves, 10000)
cougars.buf <- st_buffer(conf.cougar, 5000)

# See how many fall within the buffer of confirmed reports
# bears
bears.within <- st_intersection(missing.bears, bears.buf)
bears.within <- bears.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
bears.within <- bears.within %>% 
  dplyr::select(., -c(18:35))

# wolves
wolves.within <- st_intersection(missing.wolves, wolves.buf)
wolves.within <- wolves.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
wolves.within <- wolves.within %>% 
  dplyr::select(., -c(18:35))

# cougar
cougar.within <- st_intersection(missing.cougars, cougars.buf)
cougar.within <- cougar.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
cougar.within <- cougar.within %>% 
  dplyr::select(., -c(18:35))

# DOUBLE CHECK
plot(st_geometry(conf.bears), col = "black")
plot(st_geometry(bears.within), col = "red", add=TRUE)

plot(st_geometry(conf.wolves), col = "black")
plot(st_geometry(wolves.within), col = "red", add=TRUE)

plot(st_geometry(conf.cougar), col = "black")
plot(st_geometry(cougar.within), col = "red", add=TRUE)

# re-label the data-frame inputs
bears.within$OCC_VAL <- "PROBABLE" # another 409
wolves.within$OCC_VAL <- "PROBABLE" # another 33
cougar.within$OCC_VAL <- "PROBABLE" # another 187

# JOIN these back into "confirmed" data set
carnivore.conf.join <- rbind(conflict.data.conf, bears.within, wolves.within, cougar.within) # join points

saveRDS(carnivore.conf.join, "data/processed/carnivore_validated_bhb.rds")

### NOTE ... now do this for all the other species ?

#st_write(final.conf.join, "data/processed/full_confirmed_conflict_df.shp")