# THIS IS THE CODE TO FIND MINIMUM DENSITY FOR FIXED NUMBER OF AMENITIES


# Packages
library(rdist)
library(dplyr)
library(ggplot2)
library(readr)

source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/distribute_population.R")

# TODO:
# 1. Adding those incorporating within the others
# 2. Adding those should be adjacent
# 3. Adding those should be at the town centre or futher away

destinations <- read_csv("../inputs/destinations_v3.csv", col_names = T)


person_per_hh <- 2.6
total_pop <- 10000
mutation_p <- 0.20
total_iters <- 500

share_land_for_dest <- 0.25
pixl_diameter <- 0.2
nbhd_diameter <- 1.6

catchment_treshold <- 0.8

dwelling_per_h <- 15

pixls_area <- 0.2*0.2
pixls_counter <- ceiling(total_pop * 0.01 / (dwelling_per_h * person_per_hh * pixls_area))

# How many pixels?
nbhds_area <- nbhd_diameter * nbhd_diameter # As squares (sq.km)
#nbhds_area <- 0.4*0.4*pi # As circles (sq.km)
nbhds_counter <- ceiling(total_pop * 0.01 / (dwelling_per_h * person_per_hh * nbhds_area))



score <- 