#### EXPERIMENT 1. CATCHMENT CALCULATION FOR EACH DESTINATION #### 
#
# @author AJ

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


# function getCatchment ---------------------------------------------------

getCatchment <- function(this_dest,loc){
  # this_dest <- dest[1,]
  
  dest_type <- this_dest$dest_type_id
  dest_cap <- this_dest$pop_req*1.5
  
  print(paste("destination:",dest_type, "; dwelling denisty:", dwelling_per_h, sep = " "))
  
  pixl_diameter <- 0.1
  nbhd_diameter <- 0.8
  
  # population will be the density
  total_pop <- this_dest$pop_req
  # Per destination
  catchment_treshold <- this_dest$coverage
  
  cat("", file = log_file, append = FALSE)
  cat(paste("Total Population", total_pop, sep = ","),  file = log_file, append = , sep="\n")
  cat(paste("Dweling Density", dwelling_per_h, sep = ","),  file = log_file, append = TRUE, sep="\n")
  cat(paste("***********", "***********", sep = ","),  file = log_file, append = TRUE, sep="\n")
  
  # Creating the neighbourhoods ---------------------------------------------
  nbhd_sq <- make_nbhds(nbhd_diameter)
  #plot(nbhd_sq)
  
  # Lets only keep one neighbourhood extra!
  nbhds_area <- nbhd_diameter * nbhd_diameter # As squares (sq.km)
  #nbhds_area <- 0.4*0.4*pi # As circles (sq.km)
  nbhds_counter <- round(total_pop * 0.01 / (dwelling_per_h * person_per_hh * nbhds_area))
  # nbhds_pop <- round(total_pop / nbhds_counter)
  study_area_d <- nbhd_diameter * (nbhds_counter + 2)
  
  pixls_sq <- make_pixels_df(pixl_diameter, share_land_for_dest, total_pop, dwelling_per_h, person_per_hh, study_area_d, nbhd_sq)
  #plot(pixls_sq)
  
  # Filling pixels with population ------------------------------------------

  
  pixls_area <- pixl_diameter*pixl_diameter
  
  #pixls_area <- pixl_diameter * pixl_diameter # As squares (sq.km)
  #pixls_area <- 0.4*0.4*pi # As circles (sq.km)
  pixls_counter <- ceiling (total_pop * 0.01 / (dwelling_per_h * person_per_hh * pixls_area))
  avg_px_pop <- ceiling (total_pop / pixls_counter)
  remaining_population <- total_pop
  for (nb in nbhd_sq$NBHD_ID) {
    my_pixls <- which(pixls_sq$NBHD_ID == nb)
    for (px in my_pixls){
      pixls_sq[px, "pop"] <- min(avg_px_pop, remaining_population)
      remaining_population <- remaining_population - pixls_sq$pop[px]
    }
  }
  
  
  # nbhd_sf <- nbhd %>% 
  #   st_as_sf(coords=c("x","y"), remove=F) %>% 
  #   mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
  #   arrange(dist_to_centre)
  # 
  # 
  # # Selecting only occupied pixels
  # pixls_sf <- pixls %>% 
  #   st_as_sf(coords=c("x","y"), remove=F) %>% 
  #   mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
  #   arrange(dist_to_centre)
  # 
  # plot(pixls_sf["dist_to_centre"])
  # plot(pixls_sf["pop"])
  

  #plot(nbhd_buffered)
  centre <- st_sfc(st_point(c(0,0)))
  
  twenityMin <- st_buffer(centre,0.4)
  
  st_write(pixls_sq, paste0(output_sub_dir, "exp1_pixles_",dest_type,"_",dwelling_per_h,".sqlite"), layer= "pixels" , layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  st_write(nbhd_sq, paste0(output_sub_dir, "exp1_pixles_",dest_type,"_",dwelling_per_h,".sqlite"), layer= "nbhds" , layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  st_write(centre, paste0(output_sub_dir, "exp1_pixles_",dest_type,"_",dwelling_per_h,".sqlite"), layer= "destination" , layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  st_write(twenityMin, paste0(output_sub_dir, "exp1_pixles_",dest_type,"_",dwelling_per_h,".sqlite"), layer= "twenityMin" , layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
  
}


# Main --------------------------------------------------------------------


person_per_hh <- 2.6

share_land_for_dest <- 0.25


densities <- seq(from = 15, to = 55, by = 2.5)

output_dir <- "../outputs/exp1_catchments/" # CHANGE THIS FOR DIFFERENT RUNS

ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

dwelling_per_h <- 29.5

dest <- read_csv("../inputs/destinations_v3.csv")

output_sub_dir <-  paste(output_dir, "Density_", dwelling_per_h, "/", sep = "")
ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE)

log_file <- paste(output_sub_dir, "output_log_D", dwelling_per_h, ".txt", sep = "") 
output_file <- paste(output_sub_dir, "output_decision_D", dwelling_per_h, ".csv", sep = "")
i <- 1
loc <- read_csv("../inputs/locations.csv")

for (i in 1:nrow(dest)){
  getCatchment(dest[i,],loc)
}






total_score_df <- data.frame(density = densities)
dwelling_per_h <- 15

for (dwelling_per_h in densities){
  print(paste("******************* DWELLING DENSITY:", dwelling_per_h, sep = " "))
  

  
  
}
