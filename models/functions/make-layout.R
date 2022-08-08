make_layout <- function(nbhd_d, 
                        share_land_for_resid, 
                        total_population, 
                        dph, 
                        pphh, 
                        pxl_d){
  
  source("./functions/make_nbhds.R")
  source("./functions/make_pixels_df.R")
  
  nbhd_dev_area <- nbhd_d^2 * share_land_for_resid # land for development per nbhd
  nbhd_n <- ceiling(total_population * 0.01 / (dph * pphh * nbhd_dev_area)) # number of nbhds
  study_area_d <- nbhd_d * (ceiling(sqrt(nbhd_n)) + 1) # Dimensions of the study area
  
  pxl_a <- pxl_d*pxl_d
  # area in each pixel for population (this is used for applying density)
  pxl_dev_a <- pxl_a * share_land_for_resid 
  
  # Creating the neighbourhoods 
  
  nbhds <- make_nbhds(nbhd_d, nbhd_n) # nbhds with geometries

  # Creating cells
 
  pxl_n <- ceiling(total_population * 0.01 / (dph * pphh * pxl_dev_a))
  cells <- make_pixels_df(pxl_d, total_population, dph, 
                          pphh, study_area_d, nbhds) # creating pixles
  
  # Replace nbhd coordinates with its centroid X and Y
  nbhds <- nbhds %>%
    mutate(nbhd_x = as.numeric(sf::st_coordinates(st_centroid(.))[, 1]), 
           nbhd_y = as.numeric(sf::st_coordinates(st_centroid(.))[, 2]))%>% 
    st_drop_geometry()  
  
  # Replace cell coordinates with its centroid X and Y
  cells_geom <- cells[, "id"]
  cells <- cells %>% 
    mutate(pxl_x = as.numeric(sf::st_coordinates(st_centroid(.))[, 1]), 
           pxl_y = as.numeric(sf::st_coordinates(st_centroid(.))[, 2])) %>% 
    st_drop_geometry()
  
  # Add distance to nbhd centre to cells
  cells <- cells %>% 
    left_join(nbhds[,c("nbhd_id","nbhd_x","nbhd_y")], by = c("nbhd_id")) %>% 
    mutate(nbhd_q = paste0(nbhd_id,"_1")) %>% 
    mutate(nbhd_q = ifelse(test = pxl_x>nbhd_x & pxl_y<nbhd_y,
                           yes  = paste0(nbhd_id,"_2"), 
                           no   = nbhd_q)) %>% 
    mutate(nbhd_q = ifelse(test = pxl_x<nbhd_x & pxl_y<nbhd_y,
                           yes  = paste0(nbhd_id,"_3"), 
                           no   = nbhd_q)) %>% 
    mutate(nbhd_q = ifelse(test = pxl_x<nbhd_x & pxl_y>nbhd_y,
                           yes  = paste0(nbhd_id,"_4"), 
                           no   = nbhd_q))  
  
  # Calculating probability for cell population
  cell_population_avg <- total_population / pxl_n
  cell_population_h <- ceiling(total_population / pxl_n) 
  cell_population_l <- floor(total_population / pxl_n) 
  # probability of population being the ceiling value 
  prob_h <- (cell_population_avg - cell_population_l) / (cell_population_h - cell_population_l) 
  # probability of population being the floor value
  prob_l <- (1 - prob_h) 
  
  remaining_population <- total_population 
  for (nb in nbhds$nbhd_id) { # populating the pixles
    my_cells <- which(cells$nbhd_id ==  nb)
    for (px in my_cells){
      rnd_number <- runif(1, 0, 1)
      if(rnd_number<prob_h){
        cells[px, "pxl_pop"] <- min(prob_h, remaining_population)
      }else{
        cells[px, "pxl_pop"] <- min(cell_population_l, remaining_population)
      }
      remaining_population <- remaining_population - cells$pxl_pop[px]
    }
  }
  
  cells <- cells %>% filter(pxl_pop > 0) %>% # Just keeping the pixels with pop
    mutate(type = "resid", dest_id = "NA", parent_dest_id = "NA", child_dest_id= "NA") 
  
  return(cells)
}