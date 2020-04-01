#### EXPERIMENT 1. CATCHMENT CALCULATION FOR EACH DESTINATION #### 
#
# @author AJ

library(dplyr)
library(rdist)

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

output_dir <- "../outputs/exp1/RUN001/" # CHANGE THIS FOR DIFFERENT RUNS

ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

person_per_hh <- 2.6
total_pop <- 40000

pixl_diameter <- 0.2
nbhd_diameter <- 0.8

catchment_treshold <- 0.95

consider_categories <- FALSE

densities <- seq(from = 15, to = 55, by = 2.5)

output_dir <- "../outputs/exp1_Run1/" # CHANGE THIS FOR DIFFERENT RUNS

ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)


total_scores_file <- paste(output_dir, "score_summary.csv", sep = "")
total_score_df <- data.frame(density = densities)
dwelling_per_h <- 15

for (dwelling_per_h in densities){
  print(paste("******************* DWELLING DENSITY:", dwelling_per_h, sep = " "))
  
  nbhd <- read.csv("../inputs/neighbourhoods.csv")
  loc <- read.csv("../inputs/locations.csv")
  dest <- read.csv("../inputs/destinations.csv")
  
  output_sub_dir <-  paste(output_dir, "Density_", dwelling_per_h, "/", sep = "")
  
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE)
  
  log_file <- paste(output_sub_dir, "output_log_D", dwelling_per_h, ".txt", sep = "") 
  
  output_file <- paste(output_sub_dir, "output_decision_D", dwelling_per_h, ".csv", sep = "")
  
  cat("", file = log_file, append = FALSE)
  
  # CALCUALTING NUMBER OF NEIGHBOURHOODS
  nbhds_area <- nbhd_diameter * nbhd_diameter # As squares (sq.km)
  #nbhds_area <- 0.4*0.4*pi # As circles (sq.km)
  nbhds_counter <- round(total_pop * 0.01 / (dwelling_per_h * person_per_hh * nbhds_area))
  nbhds_pop <- round(total_pop / nbhds_counter)
  
  cat(paste("Total Population", total_pop, sep = ","),  file = log_file, append = , sep="\n")
  cat(paste("Dweling Density", dwelling_per_h, sep = ","),  file = log_file, append = TRUE, sep="\n")
  cat(paste("***********", "***********", sep = ","),  file = log_file, append = TRUE, sep="\n")
  
  # Creating the neighbourhoods ---------------------------------------------
  
  pixls <- make_pixels_df(pixl_diameter, share_land_for_dest, total_pop, dwelling_per_h, person_per_hh, dest)
  
  
  pixls_area <- 0.2*0.2
  
  #pixls_area <- pixl_diameter * pixl_diameter # As squares (sq.km)
  #pixls_area <- 0.4*0.4*pi # As circles (sq.km)
  pixls_counter <- ceiling (total_pop * 0.01 / (dwelling_per_h * person_per_hh * pixls_area))
  avg_px_pop <- ceiling (total_pop / pixls_counter)
  
  remaining_population <- total_pop
  for (nb in 1:nrow(nbhd)) {
    my_pixls <- which(pixls$nbhd == nb)
    for (px in my_pixls){
      pixls[px, "pop"] <- min(avg_px_pop, remaining_population)
      remaining_population <- remaining_population - pixls$pop[px]
    }
    }
  
   # Selecting only occupied pixels
  library(sf)
  pixls_sf <- pixls %>% 
    st_as_sf(coords=c("x","y"), remove=F) %>% 
    mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
    arrange(dist_to_centre)
  
  plot(pixls_sf["dist_to_centre"])
  
  for(dest_row in 1:nrow(dest)){
    
    dest_type <- dest$dest_type_id[dest_row]
    dest_cap <- dest$pop_req[dest_row]*1.5
    
    print(paste("destination:",dest_type, "; dwelling denisty:", dwelling_per_h, sep = " "))
    
    # SERVING THE NEIGHBOURS 
    # finding those closest to this
  
    # Find all those in range and not fully served

      for (i in 1:nrow(pixls_sf)) {
        
        #temp_dist <- as.double(dists_ord[i])
        #temp_pixl_row <- which(iter_pixls$ID == colnames(dists_ord)[i]) 
        # If distance is less than 20 min given the mode speed and nb has unserved pop
        if (dest_cap > 0){
          # serve
          temp <- min(dest_cap, pixls_sf$pop[i])
          pixls_sf[i, paste0("pop_not_served_by_dest_", dest_type)] <- temp
          dest_cap <- dest_cap - temp
      } 
      }
  }
    st_write(pixls_sf, paste0(output_sub_dir, "exp1_pixles",dwelling_per_h,".sqlite"), layer_options = 'GEOMETRY=AS_XY', delete_layer = T)
    write.csv(st_drop_geometry(pixls_sf), paste0(output_sub_dir, "exp1_pixles",dwelling_per_h,".csv"))
}
