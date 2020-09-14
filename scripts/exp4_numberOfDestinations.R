
library(dplyr)
library(rdist)
library(sf)
library(readr)

source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/make_nbhds.R")
source("./functions/distribute_population.R")


person_per_hh <- 2.6
share_land_for_dest <- 0.25

densities <- seq(from = 15, to = 55, by = 2.5)
pixl_diameter <- 0.1
nbhd_diameter <- 0.8

