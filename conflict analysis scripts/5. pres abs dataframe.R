
# Pres Abs Dataframe Prep -------------------------------------------------

# Load Packages: ----------------------------------------------------------
library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(dismo)

# Bring in Data: ----------------------------------------------------------
bhb.10k.buf <- st_read("data/processed/bhb_10km.shp")
conflict.all <-st_read("data/processed/conflict_reports_bhb.shp") 
temp.rast <- rast("data/processed/bhb_ndvi.tif")

# Plot our points and BHB watershed:
plot(st_geometry(bhb.10k.buf))
plot(st_geometry(conflict.all, add=TRUE)) #2876 reports

# Generate Random Points for Pseudo-absences: -----------------------------
set.seed(2345)
p.abs.pts <- randomPoints(raster(temp.rast), 3000)

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
abs.pts.sf['bears'] <- 0
abs.pts.sf['wolves'] <- 0
abs.pts.sf['cougars'] <- 0  # so this shows as absences
abs.pts.sf['AREA'] <- NA
abs.pts.sf['Ar_sqkm'] <- NA

# Reorder the columns to match:
abs.pts.sf <- abs.pts.sf[ , c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,1)]


# Restructure Data frame: --------------------------------------------------
## Here we add our presence points to our absences
# Join our all species presence points with the absence points:
st_crs(conflict.all) == st_crs(abs.pts.sf)

abs.pts.reproj <- st_transform(abs.pts.sf, st_crs(conflict.all))
st_crs(abs.pts.reproj) == st_crs(conflict.all) # TRUE

all.conflict.pts.w.abs <- rbind(conflict.all, abs.pts.reproj)


# Plot these to check:
plot(st_geometry(bhb.10k.buf))
plot(st_geometry(all.conflict.pts.w.abs), add=TRUE)

# Save as New Df: ---------------------------------------------------------
st_write(all.conflict.pts.w.abs, "Data/processed/warp_pres.abs.shp", append=FALSE)

