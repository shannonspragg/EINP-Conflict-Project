
# Pres Abs Dataframe Prep -------------------------------------------------

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)

# Bring in Data: ----------------------------------------------------------
bhb.50k.buf <- st_read("data/processed/bhb_50km.shp")
conflict.cougar <-st_read("data/processed/cougar_conflict_confirmed_dataframe.shp") 
temp.rast <- rast("data/processed/dist2pa_km_bhb.tif")

# Mask our temp rast to the bhb boundary:
bhb.50km.v <- vect(bhb.50k.buf)
temp.rast.bhb <- terra::mask(temp.rast, bhb.50km.v)

# Plot our points and BHB watershed:
plot(st_geometry(bhb.50k.buf))
plot(st_geometry(conflict.cougar, add=TRUE)) #1086 reports

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(raster(temp.rast.bhb), 2175) # double the conflict reports

# Make this a data frame:
abs.pts.df <- data.frame(p.abs.pts)

# Make these spatial points:
abs.pts.sf <- st_as_sf(abs.pts.df, coords= c("x","y"), crs= st_crs(temp.rast))

# Add the missing columns:
# Let's try this manually:
abs.pts.sf['id'] <- NA
abs.pts.sf['OCC_FIL'] <- NA
abs.pts.sf['OCCURRE'] <- NA
abs.pts.sf['ACTION_'] <- NA
abs.pts.sf['OCC_CIT'] <- NA
abs.pts.sf['OCC_POS'] <- NA
abs.pts.sf['OCC_WMU'] <- NA
abs.pts.sf['OCC_SPE'] <- NA
abs.pts.sf['OCC_NUM'] <- 0
abs.pts.sf['OCC_PRI'] <- NA
abs.pts.sf['OCC_VAL'] <- NA
abs.pts.sf['OCC_OCC'] <- NA
abs.pts.sf['SITE_NA'] <- NA
abs.pts.sf['bears'] <- 0
abs.pts.sf['wolves'] <- 0
abs.pts.sf['cougars'] <- 0  # so this shows as absences
abs.pts.sf['AREA_HA'] <- NA

# Reorder the columns to match:
abs.pts.sf <- abs.pts.sf[ , c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,1)]


# Assign CCS regions: -----------------------------------------------------

# Write this as a .shp for later:
ab.ccs <- st_read("data/processed/AB_CCS.shp")
ab.ccs.reproj <- st_transform(ab.ccs, st_crs(bhb.50k.buf))
presabs.reproj <- st_transform(abs.pts.sf, st_crs(bhb.50k.buf))

# Assign our points to a CCS category:
presabs.ccs.join <- st_join(presabs.reproj, left = TRUE, ab.ccs.reproj) # join points

head(presabs.ccs.join) # Assigned points to a CCS category

presabs.ccs.join <- presabs.ccs.join %>% 
  dplyr::select(., -c(22,21,19))

# Add a column for conflict absence:
presabs.ccs.join$cnflct_ <- 0
presabs.ccs.join <- presabs.ccs.join[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,21,20)]

conflict.cougar$cnflct_ <- 1

# Restructure Data frame: --------------------------------------------------
## Here we add our presence points to our absences
# Join our all species presence points with the absence points:
st_crs(conflict.cougar) == st_crs(presabs.ccs.join)

conf.conflict.pts.w.abs <- rbind(conflict.cougar, presabs.ccs.join) # 3261 points total

# Plot these to check:
plot(st_geometry(bhb.50k.buf))
plot(st_geometry(conf.conflict.pts.w.abs), add=TRUE)

# Save as New Df: ---------------------------------------------------------
st_write(conf.conflict.pts.w.abs, "Data/processed/cougar_conflict_pres_abs_df.shp", append=FALSE)

