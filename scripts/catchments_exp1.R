#### EXPERIMENT 1. CATCHMENT CALCULATION FOR EACH DESTINATION #### 
#
# @author AJ

library(dplyr)

source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/distribute_population.R")



# Control variables -------------------------------------------------------

person_per_hh <- 2.6
total_pop <- 30000


share_land_for_dest <- 0.25
pixl_diameter <- 0.2
nbhd_diameter <- 0.8

catchment_treshold <- 0.95

densities <- seq(from = 15, to = 55, by = 2.5)

output_dir <- "./outputs/exp1/RUN001/" # CHANGE THIS FOR DIFFERENT RUNS

ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)
