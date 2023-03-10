# EINP-Conflict-Project
This is a repository for the conflict and connectivity analysis of the Beaver Hills Watershed surrounding Elk Island National Park for black bears, wolves, and cougar.

##################
The following scripts include the steps for compiling conflict report data, producing habitat suitability models, creating connectivity resistance surfaces, running connectivity models, and validating model results. These scripts function in progression as listed below. As such, running the general wildlife scripts is important before running the carnivore models, since these are designed hierarchically.

########################### 1. General All Species Analysis ######################################################
This folder contains the different sets of scripts to produce the connectivity models fand conflict report models for all wildlife species in general. To replicate this analysis, complete the scripts in the following order:

1. general species data prep -- complete the numbered scripts to download and prep conflict data variables

2. general species connectivity scripts -- follow these scripts to produce the resistance surface inputs for running biophysical connectivity models in Omniscape (external)

4. general species conflict analysis scripts -- complete this series of scripts to prep the conflict report data, variables, and general wildlife conflict model analyses


########################### 2. Black Bear Analysis ######################################################
This folder contains the different sets of scripts to produce the habitat suitability model for bears, the connectivity models for bears, and the conflict report models for bears. To replicate this analysis, complete the scripts in the following order:

1. b bear habitat data scripts -- complete the numbered scripts to download and prep habitat data variables

2. b bear habitat model scripts -- complete this series of scripts to replicate the habitat suitability model for black bears

3. b bear connectivity scripts -- follow these scripts to produce the resistance surface inputs for running biophysical and social connectivity models in Omniscape (external)

4. b bear conflict analysis scripts -- complete this series of scripts to prep the conflict report data, variables, and bear conflict model analyses


########################### 3. Wolf Analysis ######################################################
This folder contains the different sets of scripts to produce the habitat suitability model for wolves, the connectivity models for wolves, and the conflict report models for wolves. To replicate this analysis, complete the scripts in the following order:

1. wolf habitat data scripts -- complete the numbered scripts to download and prep additional wolf habitat data variables (many were already produced in bear analysis)

2. wolf habitat model scripts -- complete this series of scripts to replicate the habitat suitability model for wolves

3. wolf connectivity scripts -- follow these scripts to produce the resistance surface inputs for running wolf biophysical and social connecitivity models in Omniscape (external)

4. wolf conflict analysis scripts -- complete this series of scripts to prep the conflict report data, variables, and general and wolf conflict model analyses


########################### 4. Cougar Analysis ######################################################
This folder contains the different sets of scripts to produce the habitat suitability model for cougar, the connectivity models for cougars, and the conflict report models for cougars. To replicate this analysis, complete the scripts in the following order:

1. cougar habitat data scripts -- complete the numbered scripts to download and prep habitat data variables

2. cougar habitat model scripts -- complete this series of scripts to replicate the habitat suitability model for cougars

3. cougar connectivity scripts -- follow these scripts to produce the resistance surface inputs for running biophysical and social connecitivity models in Omniscape (external)

4. cougar conflict analysis scripts -- complete this series of scripts to prep the conflict report data, variables, and general and cougar conflict model analyses

########################### Carnivore Summary Analysis ######################################################
This folder contains the different sets of scripts to produce the summarized connectivity model across all carnivore species. To replicate this analysis, complete the scripts in the following order:

1. carnivore connectivity -- use to produce the synthesized carnivore connectivity model, pulling in the connectivity model results from previous analyses (completed above)

