# Filter conflict data for analysis: ----------------------------------------
    # WARNING: THIS IS A LONG SCRIPT (until revised with a loop function)

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
bears.within <- bears.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
bears.within <- bears.within %>% 
  dplyr::select(., -c(18:35))

# wolves
wolves.within <- st_intersection(missing.wolves, wolves.buf)
wolves.within <- wolves.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
wolves.within <- wolves.within %>% 
  dplyr::select(., -c(18:35))

# cougar
cougar.within <- st_intersection(missing.cougars, cougars.buf)
cougar.within <- cougar.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
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

# Do the above process for all other species ------------------------------
    # coyote
conf.coyote <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "COYOTE")
missing.coyote <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "COYOTE")
coyote.buf <- st_buffer(conf.coyote, 5000)
coyote.within <- st_intersection(missing.coyote, coyote.buf)
coyote.within <- coyote.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
coyote.within <- coyote.within %>% 
  dplyr::select(., -c(18:35))
coyote.within$OCC_VAL <- "PROBABLE" # another 595
# lets do a join every 5 or so

# moose
conf.moose <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "MOOSE")
missing.moose <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "MOOSE")
moose.buf <- st_buffer(conf.moose, 5000)
moose.within <- st_intersection(missing.moose, moose.buf)
moose.within <- moose.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
moose.within <- moose.within %>% 
  dplyr::select(., -c(18:35))
moose.within$OCC_VAL <- "PROBABLE" # another 963

# canada.goose
conf.canada.goose <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "CANADA GOOSE")
missing.canada.goose <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "CANADA GOOSE")
canada.goose.buf <- st_buffer(conf.canada.goose, 5000)
canada.goose.within <- st_intersection(missing.canada.goose, canada.goose.buf)
canada.goose.within <- canada.goose.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
canada.goose.within <- canada.goose.within %>% 
  dplyr::select(., -c(18:35))
canada.goose.within$OCC_VAL <- "PROBABLE" # another 60

# mule.deer
conf.mule.deer <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "MULE DEER")
missing.mule.deer <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "MULE DEER")
mule.deer.buf <- st_buffer(conf.mule.deer, 5000)
mule.deer.within <- st_intersection(missing.mule.deer, mule.deer.buf)
mule.deer.within <- mule.deer.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
mule.deer.within <- mule.deer.within %>% 
  dplyr::select(., -c(18:35))
mule.deer.within$OCC_VAL <- "PROBABLE" # another 136

# wt.deer
conf.wt.deer <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "WHITE-TAILED DEER")
missing.wt.deer <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "WHITE-TAILED DEER")
wt.deer.buf <- st_buffer(conf.wt.deer, 5000)
wt.deer.within <- st_intersection(missing.wt.deer, wt.deer.buf)
wt.deer.within <- wt.deer.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
wt.deer.within <- wt.deer.within %>% 
  dplyr::select(., -c(18:35))
wt.deer.within$OCC_VAL <- "PROBABLE" # another 955

# duck
conf.duck <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "DUCK - MALLARD")
missing.duck <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "DUCK - MALLARD")
duck.buf <- st_buffer(conf.duck, 5000)
duck.within <- st_intersection(missing.duck, duck.buf)
duck.within <- duck.within %>% distinct(OCC_FIL, .keep_all = TRUE) 
duck.within <- duck.within %>% 
  dplyr::select(., -c(18:35))
duck.within$OCC_VAL <- "PROBABLE" # another 7

# hawk rt
conf.hawk <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "HAWKS - RED-TAILED")
missing.hawk <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "HAWKS - RED-TAILED")
hawk.buf <- st_buffer(conf.hawk, 10000)
hawk.within <- st_intersection(missing.hawk, hawk.buf)
hawk.within <- hawk.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
hawk.within <- hawk.within %>% 
  dplyr::select(., -c(18:35))
hawk.within$OCC_VAL <- "PROBABLE" # another 8

# hawk.other
conf.hawk.o <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "HAWKS - OTHER")
missing.hawk.o <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "HAWKS - OTHER")
hawk.o.buf <- st_buffer(conf.hawk.o, 10000)
hawk.o.within <- st_intersection(missing.hawk.o, hawk.o.buf)
hawk.o.within <- hawk.o.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
hawk.o.within <- hawk.o.within %>% 
  dplyr::select(., -c(18:35))
hawk.o.within$OCC_VAL <- "PROBABLE" # another 13

# JOIN these back into "confirmed" data set (doing this as we go)
P1.conf.join <- rbind(carnivore.conf.join, coyote.within, moose.within, canada.goose.within, mule.deer.within, wt.deer.within,
                      duck.within, hawk.o.within, hawk.within) # join points - 4432 now

# osprey
conf.osprey <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OSPREY")
missing.osprey <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OSPREY")
osprey.buf <- st_buffer(conf.osprey, 10000)
osprey.within <- st_intersection(missing.osprey, osprey.buf)
# osprey.within <- osprey.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# osprey.within <- osprey.within %>% 
#   dplyr::select(., -c(18:35))
# osprey.within$OCC_VAL <- "PROBABLE" # no obs - leave this out

# bald.eagle
conf.bald.eagle <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "EAGLE - BALD")
missing.bald.eagle <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "EAGLE - BALD")
bald.eagle.buf <- st_buffer(conf.bald.eagle, 10000)
bald.eagle.within <- st_intersection(missing.bald.eagle, bald.eagle.buf)
bald.eagle.within <- bald.eagle.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
bald.eagle.within <- bald.eagle.within %>% 
  dplyr::select(., -c(18:35))
bald.eagle.within$OCC_VAL <- "PROBABLE" # another 1

# owls.o
conf.owls.o <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OWLS - OTHER")
missing.owls.o <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OWLS - OTHER")
owls.o.buf <- st_buffer(conf.owls.o, 10000)
owls.o.within <- st_intersection(missing.owls.o, owls.o.buf)
owls.o.within <- owls.o.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
owls.o.within <- owls.o.within %>% 
  dplyr::select(., -c(18:35))
owls.o.within$OCC_VAL <- "PROBABLE" # another 9

# goose
conf.goose <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "GOOSE")
missing.goose <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "GOOSE")
goose.buf <- st_buffer(conf.goose, 10000)
goose.within <- st_intersection(missing.goose, goose.buf)
goose.within <- goose.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
goose.within <- goose.within %>% 
  dplyr::select(., -c(18:35))
goose.within$OCC_VAL <- "PROBABLE" # another 33

# beaver
conf.beaver <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "BEAVER")
missing.beaver <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "BEAVER")
beaver.buf <- st_buffer(conf.beaver, 5000)
beaver.within <- st_intersection(missing.beaver, beaver.buf)
beaver.within <- beaver.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
beaver.within <- beaver.within %>% 
  dplyr::select(., -c(18:35))
beaver.within$OCC_VAL <- "PROBABLE" # another 5

# rough legged hawk
conf.rl.hawk <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "HAWKS - ROUGH-LEGGED")
missing.rl.hawk <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "HAWKS - ROUGH-LEGGED")
rl.hawk.buf <- st_buffer(conf.rl.hawk, 10000)
rl.hawk.within <- st_intersection(missing.rl.hawk, rl.hawk.buf)
# rl.hawk.within <- rl.hawk.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# rl.hawk.within <- rl.hawk.within %>% 
#   dplyr::select(., -c(18:35))
# rl.hawk.within$OCC_VAL <- "PROBABLE" # 0 - leave this out

# fox
conf.fox <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "FOX")
missing.fox <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "FOX")
fox.buf <- st_buffer(conf.fox, 5000)
fox.within <- st_intersection(missing.fox, fox.buf)
fox.within <- fox.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
fox.within <- fox.within %>% 
  dplyr::select(., -c(18:35))
fox.within$OCC_VAL <- "PROBABLE" # another 39

# owl great grey
conf.owl.gg <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OWLS - GREAT GREY")
missing.owl.gg <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OWLS - GREAT GREY")
owl.gg.buf <- st_buffer(conf.owl.gg, 10000)
owl.gg.within <- st_intersection(missing.owl.gg, owl.gg.buf)
# owl.gg.within <- owl.gg.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# owl.gg.within <- owl.gg.within %>% 
#   dplyr::select(., -c(18:35))
# owl.gg.within$OCC_VAL <- "PROBABLE" # 0 - leave out

# magpie
conf.magpie <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "MAGPIE")
missing.magpie <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "MAGPIE")
magpie.buf <- st_buffer(conf.magpie, 10000)
magpie.within <- st_intersection(missing.magpie, magpie.buf)
magpie.within <- magpie.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
magpie.within <- magpie.within %>% 
  dplyr::select(., -c(18:35))
magpie.within$OCC_VAL <- "PROBABLE" # another 4

# raven
conf.raven <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "RAVEN")
missing.raven <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "RAVEN")
raven.buf <- st_buffer(conf.raven, 10000)
raven.within <- st_intersection(missing.raven, raven.buf)
# raven.within <- raven.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# raven.within <- raven.within %>% 
#   dplyr::select(., -c(18:35))
# raven.within$OCC_VAL <- "PROBABLE" # 0

# owl.snowy
conf.owl.sn <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OWLS - SNOWY")
missing.owl.sn <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OWLS - SNOWY")
owl.sn.buf <- st_buffer(conf.owl.sn, 10000)
owl.sn.within <- st_intersection(missing.owl.sn, owl.sn.buf)
owl.sn.within <- owl.sn.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
owl.sn.within <- owl.sn.within %>% 
  dplyr::select(., -c(18:35))
owl.sn.within$OCC_VAL <- "PROBABLE" # another 1

# skunk
conf.skunk <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "SKUNK")
missing.skunk <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "SKUNK")
skunk.buf <- st_buffer(conf.skunk, 5000)
skunk.within <- st_intersection(missing.skunk, skunk.buf)
skunk.within <- skunk.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
skunk.within <- skunk.within %>% 
  dplyr::select(., -c(18:35))
skunk.within$OCC_VAL <- "PROBABLE" # another 12

# elk
conf.elk <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "ELK")
missing.elk <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "ELK")
elk.buf <- st_buffer(conf.elk, 5000)
elk.within <- st_intersection(missing.elk, elk.buf)
elk.within <- elk.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
elk.within <- elk.within %>% 
  dplyr::select(., -c(18:35))
elk.within$OCC_VAL <- "PROBABLE" # another 18


# JOIN these back into "confirmed" data set (doing this as we go)
P2.conf.join <- rbind(P1.conf.join, bald.eagle.within, owls.o.within, goose.within, beaver.within, fox.within, magpie.within,
                      owl.sn.within, skunk.within, elk.within) # join points - 4454 now

# owl.sw
conf.owl.sw <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OWLS - SAW WHET")
missing.owl.sw <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OWLS - SAW WHET")
owl.sw.buf <- st_buffer(conf.owl.sw, 10000)
owl.sw.within <- st_intersection(missing.owl.sw, owl.sw.buf)
# owl.sw.within <- owl.sw.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# owl.sw.within <- owl.sw.within %>% 
#   dplyr::select(., -c(18:35))
# owl.sw.within$OCC_VAL <- "PROBABLE" # 0

# fisher
conf.fisher <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "FISHER")
missing.fisher <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "FISHER")
fisher.buf <- st_buffer(conf.fisher, 5000)
fisher.within <- st_intersection(missing.fisher, fisher.buf)
# fisher.within <- fisher.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# fisher.within <- fisher.within %>% 
#   dplyr::select(., -c(18:35))
# fisher.within$OCC_VAL <- "PROBABLE" # 0

# porcupine
conf.porcupine <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "PORCUPINE")
missing.porcupine <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "PORCUPINE")
porcupine.buf <- st_buffer(conf.porcupine, 5000)
porcupine.within <- st_intersection(missing.porcupine, porcupine.buf)
porcupine.within <- porcupine.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
porcupine.within <- porcupine.within %>% 
  dplyr::select(., -c(18:35))
porcupine.within$OCC_VAL <- "PROBABLE" # another 3

# squirrel
conf.squirrel <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "SQUIRREL")
missing.squirrel <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "SQUIRREL")
squirrel.buf <- st_buffer(conf.squirrel, 5000)
squirrel.within <- st_intersection(missing.squirrel, squirrel.buf)
squirrel.within <- squirrel.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
squirrel.within <- squirrel.within %>% 
  dplyr::select(., -c(18:35))
squirrel.within$OCC_VAL <- "PROBABLE" # another 217

# peregrine.falcon
conf.peregrine.falcon <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "PEREGRINE FALCON")
missing.peregrine.falcon <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "PEREGRINE FALCON")
peregrine.falcon.buf <- st_buffer(conf.peregrine.falcon, 10000)
# peregrine.falcon.within <- st_intersection(missing.peregrine.falcon, peregrine.falcon.buf)
# peregrine.falcon.within <- peregrine.falcon.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# peregrine.falcon.within <- peregrine.falcon.within %>% 
#   dplyr::select(., -c(18:35))
# peregrine.falcon.within$OCC_VAL <- "PROBABLE" # 0

# groundhog
conf.groundhog <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "WOODCHUCK/GROUND HOG")
missing.groundhog <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "WOODCHUCK/GROUND HOG")
# groundhog.buf <- st_buffer(conf.groundhog, 5000)
# groundhog.within <- st_intersection(missing.groundhog, groundhog.buf)
# groundhog.within <- groundhog.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# groundhog.within <- groundhog.within %>% 
#   dplyr::select(., -c(18:35))
# groundhog.within$OCC_VAL <- "PROBABLE" # 0

# gld.eagle
conf.gld.eagle <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "EAGLE - GOLDEN")
missing.gld.eagle <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "EAGLE - GOLDEN")
gld.eagle.buf <- st_buffer(conf.gld.eagle, 10000)
gld.eagle.within <- st_intersection(missing.gld.eagle, gld.eagle.buf)
# gld.eagle.within <- gld.eagle.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# gld.eagle.within <- gld.eagle.within %>% 
#   dplyr::select(., -c(18:35))
# gld.eagle.within$OCC_VAL <- "PROBABLE" # 0

# racoon
conf.racoon <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "RACCOON")
missing.racoon <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "RACCOON")
racoon.buf <- st_buffer(conf.racoon, 5000)
racoon.within <- st_intersection(missing.racoon, racoon.buf)
racoon.within <- racoon.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
racoon.within <- racoon.within %>% 
  dplyr::select(., -c(18:35))
racoon.within$OCC_VAL <- "PROBABLE" # another 2

# snake.o
conf.snake.o <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OTHER - SNAKE")
missing.snake.o <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OTHER - SNAKE")
snake.o.buf <- st_buffer(conf.snake.o, 5000)
# snake.o.within <- st_intersection(missing.snake.o, snake.o.buf)
# snake.o.within <- snake.o.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# snake.o.within <- snake.o.within %>% 
#   dplyr::select(., -c(18:35))
# snake.o.within$OCC_VAL <- "PROBABLE" # 0

# owl.gh
conf.owl.gh <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "OWLS - GREAT HORNED")
missing.owl.gh <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "OWLS - GREAT HORNED")
owl.gh.buf <- st_buffer(conf.owl.gh, 10000)
owl.gh.within <- st_intersection(missing.owl.gh, owl.gh.buf)
owl.gh.within <- owl.gh.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
owl.gh.within <- owl.gh.within %>% 
  dplyr::select(., -c(18:35))
owl.gh.within$OCC_VAL <- "PROBABLE" # another 34

# crow
conf.crow <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "crow")
missing.crow <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "crow")
# crow.buf <- st_buffer(conf.crow, 10000)
# crow.within <- st_intersection(missing.crow, crow.buf)
# crow.within <- crow.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# crow.within <- crow.within %>% 
#   dplyr::select(., -c(18:35))
# crow.within$OCC_VAL <- "PROBABLE" # 0

# muskrat
conf.muskrat <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "MUSKRAT")
missing.muskrat <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "MUSKRAT")
# muskrat.buf <- st_buffer(conf.muskrat, 5000)
# muskrat.within <- st_intersection(missing.muskrat, muskrat.buf)
# muskrat.within <- muskrat.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# muskrat.within <- muskrat.within %>% 
#   dplyr::select(., -c(18:35))
# muskrat.within$OCC_VAL <- "PROBABLE" # 0

# eagle
conf.eagle <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "EAGLE")
missing.eagle <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "EAGLE")
eagle.buf <- st_buffer(conf.eagle, 10000)
eagle.within <- st_intersection(missing.eagle, eagle.buf)
# eagle.within <- eagle.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# eagle.within <- eagle.within %>% 
#   dplyr::select(., -c(18:35))
# eagle.within$OCC_VAL <- "PROBABLE" # 0

# duck
conf.duck <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "DUCK")
missing.duck <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "DUCK")
duck.buf <- st_buffer(conf.duck, 5000)
duck.within <- st_intersection(missing.duck, duck.buf)
# duck.within <- duck.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# duck.within <- duck.within %>% 
#   dplyr::select(., -c(18:35))
# duck.within$OCC_VAL <- "PROBABLE" # 0

# pelican
conf.pelican <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "PELICAN/CORMORANT")
missing.pelican <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "PELICAN/CORMORANT")
pelican.buf <- st_buffer(conf.pelican, 10000)
pelican.within <- st_intersection(missing.pelican, pelican.buf)
# pelican.within <- pelican.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# pelican.within <- pelican.within %>% 
#   dplyr::select(., -c(18:35))
# pelican.within$OCC_VAL <- "PROBABLE" # 0

# coopers hawk
conf.hawk.coop <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "HAWK - COOPER'S")
missing.hawk.coop <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "HAWK - COOPER'S")
hawk.coop.buf <- st_buffer(conf.hawk.coop, 5000)
# hawk.coop.within <- st_intersection(missing.hawk.coop, hawk.coop.buf)
# hawk.coop.within <- hawk.coop.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# hawk.coop.within <- hawk.coop.within %>% 
#   dplyr::select(., -c(18:35))
# hawk.coop.within$OCC_VAL <- "PROBABLE" # 0

# lynx
conf.lynx <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "LYNX")
missing.lynx <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "LYNX")
lynx.buf <- st_buffer(conf.lynx, 10000)
lynx.within <- st_intersection(missing.lynx, lynx.buf)
# lynx.within <- lynx.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# lynx.within <- lynx.within %>% 
#   dplyr::select(., -c(18:35))
# lynx.within$OCC_VAL <- "PROBABLE" # 0

# weasel
conf.weasel <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "WEASEL")
missing.weasel <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "WEASEL")
weasel.buf <- st_buffer(conf.weasel, 5000)
weasel.within <- st_intersection(missing.weasel, weasel.buf)
# weasel.within <- weasel.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# weasel.within <- weasel.within %>% 
#   dplyr::select(., -c(18:35))
# weasel.within$OCC_VAL <- "PROBABLE" # a0

# t.swan
conf.t.swan <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "TRUMPETER SWAN")
missing.t.swan <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "TRUMPETER SWAN")
t.swan.buf <- st_buffer(conf.t.swan, 5000)
t.swan.within <- st_intersection(missing.t.swan, t.swan.buf)
# t.swan.within <- t.swan.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# t.swan.within <- t.swan.within %>% 
#   dplyr::select(., -c(18:35))
# t.swan.within$OCC_VAL <- "PROBABLE" # 0

# sn.goose
conf.sn.goose <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "SNOW GOOSE")
missing.sn.goose <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "SNOW GOOSE")
sn.goose.buf <- st_buffer(conf.sn.goose, 5000)
sn.goose.within <- st_intersection(missing.sn.goose, sn.goose.buf)
# sn.goose.within <- sn.goose.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# sn.goose.within <- sn.goose.within %>% 
#   dplyr::select(., -c(18:35))
# sn.goose.within$OCC_VAL <- "PROBABLE" # 0

# grizzly
conf.grizzly <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "GRIZZLY BEAR")
missing.grizzly <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "GRIZZLY BEAR")
grizzly.buf <- st_buffer(conf.grizzly, 10000)
grizzly.within <- st_intersection(missing.grizzly, grizzly.buf)
grizzly.within <- grizzly.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
grizzly.within <- grizzly.within %>% 
  dplyr::select(., -c(18:35))
grizzly.within$OCC_VAL <- "PROBABLE" # another 1

# rabbit
conf.rabbit <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "RABBIT")
missing.rabbit <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "RABBIT")
rabbit.buf <- st_buffer(conf.rabbit, 5000)
rabbit.within <- st_intersection(missing.rabbit, rabbit.buf)
rabbit.within <- rabbit.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
rabbit.within <- rabbit.within %>% 
  dplyr::select(., -c(18:35))
rabbit.within$OCC_VAL <- "PROBABLE" # another 2

# walleye
conf.walleye <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "WALLEYE")
missing.walleye <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "WALLEYE")
walleye.buf <- st_buffer(conf.walleye, 5000)
walleye.within <- st_intersection(missing.walleye, walleye.buf)
# walleye.within <- walleye.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# walleye.within <- walleye.within %>% 
#   dplyr::select(., -c(18:35))
# walleye.within$OCC_VAL <- "PROBABLE" # 0

# cormorant
conf.cormorant <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "CORMORANT")
missing.cormorant <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "CORMORANT")
cormorant.buf <- st_buffer(conf.cormorant, 10000)
cormorant.within <- st_intersection(missing.cormorant, cormorant.buf)
# cormorant.within <- cormorant.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# cormorant.within <- cormorant.within %>% 
#   dplyr::select(., -c(18:35))
# cormorant.within$OCC_VAL <- "PROBABLE" # 0

# bat
conf.bat <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "BAT")
missing.bat <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "BAT")
bat.buf <- st_buffer(conf.bat, 10000)
bat.within <- st_intersection(missing.bat, bat.buf)
# bat.within <- bat.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# bat.within <- bat.within %>% 
#   dplyr::select(., -c(18:35))
# bat.within$OCC_VAL <- "PROBABLE" # 0

# antelope
conf.antelope <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "ANTELOPE")
missing.antelope <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "ANTELOPE")
antelope.buf <- st_buffer(conf.antelope, 5000)
antelope.within <- st_intersection(missing.antelope, antelope.buf)
# antelope.within <- antelope.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
# antelope.within <- antelope.within %>% 
#   dplyr::select(., -c(18:35))
# antelope.within$OCC_VAL <- "PROBABLE" # 0

# badger
conf.badger <- conflict.data.conf %>%
  subset(conflict.data.conf$OCC_SPE == "BADGER")
missing.badger <- conflict.data.missing %>% subset(conflict.data.missing$OCC_SPE == "BADGER")
badger.buf <- st_buffer(conf.badger, 5000)
badger.within <- st_intersection(missing.badger, badger.buf)
badger.within <- badger.within %>% distinct(OCC_FIL, .keep_all = TRUE) #rid of duplicates - now 409
badger.within <- badger.within %>% 
  dplyr::select(., -c(18:35))
badger.within$OCC_VAL <- "PROBABLE" # another 4

# JOIN these back into "confirmed" data set (doing this as we go)
P3.conf.join <- rbind(P2.conf.join, porcupine.within, squirrel.within, racoon.within, owl.gh.within, grizzly.within, rabbit.within,
                      badger.within) # join points - 4817 now




#st_write(final.conf.join, "data/processed/full_confirmed_conflict_df.shp")