# Build "Validated" Habitat Model for Wolves -----------------------------------------

## NOTE: the camera trap data is relatively limited for wolf sightings, and only covers an area surrounding EINP. Therefore, it may not be fully
## representative of habitat selection beyond this area

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(gdalUtilities)
library(dplyr)

# Bring in covariate data: -------------------------------------------------------------
