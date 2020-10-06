make_pixels_df <- function(pxl_d, share_land_for_dest, total_pop, dwelling_per_h, person_per_hh, study_area_d, nbhd_sq){
 
  # Making initial nbhds df
  my_seq <- seq(from = -(study_area_d/2)+pxl_d, to = (study_area_d/2)-pxl_d, by = pxl_d)
  pixls <- data.frame(matrix(nrow = length(my_seq)^2, ncol = 2))
  colnames(pixls) <- c("x", "y")
  my_row <- 1
  for(i in seq(from = -(study_area_d/2)+pxl_d, to = (study_area_d/2)-pxl_d, by = pxl_d)){
    for(j in seq(from = -(study_area_d/2)+pxl_d, to = (study_area_d/2)-pxl_d, by = pxl_d)){
      pixls$x[my_row] <- i
      pixls$y[my_row] <- j
      my_row <- my_row + 1
    }
  }

  # calculating distance to centre
  pixls <- pixls %>%
    mutate(x_dist = cdist(x,0, metric = "euclidean")) %>%
    mutate(y_dist = cdist(y,0, metric = "euclidean")) %>%
    mutate(dist_to_centre = x_dist + y_dist) %>%
    select(x, y, dist_to_centre)
  # Sorting   
  pixls <- pixls[order(pixls$dist_to_centre, pixls$x, pixls$y),]
  
  pixls_sf <- pixls %>% 
    st_as_sf(coords=c("x","y"), remove=F) %>% 
    mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
    arrange(dist_to_centre)
  
  #plot(pixls_sf)
  #plot(nbhd_sq)
  
  #st_write(pixls_sf, "pxl2.sqlite")
  #st_write(nbhd_sq, "nbhd2.sqlite")
  
  # Making the squares
  pixls_crs <- pixls_sf %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    purrr::pmap(function(X,Y){
       outer <- c(X, Y) %>% 
         rbind(c(X+pxl_d, Y)) %>% 
         rbind(c(X+pxl_d, Y+pxl_d)) %>% 
         rbind(c(X, Y+pxl_d)) %>% 
         rbind(c(X, Y))
      return(st_polygon(list(outer)))
    }) %>% 
    st_as_sfc() %>% 
    as.data.frame()
  
  pixls_centres <- pixls_crs %>% 
    mutate(ID = row_number()) %>% 
    st_as_sf() %>% 
    st_centroid() %>% 
    st_join(nbhd_sq, largest = T)
  
  pixls_sq <- pixls_sf %>% 
    st_drop_geometry() %>% 
    mutate(ID = row_number()) %>% 
    left_join(st_drop_geometry(pixls_centres)) %>% 
    cbind(pixls_crs) %>% 
    dplyr::select(ID, NBHD_ID, dist_to_centre, geometry) %>% 
    st_as_sf()
  
  # plot(pixls_sq)
  # points <- st_coordinates(pixls_sf) %>% as.data.frame()
  # 
  # pixls_crs <- purrr::pmap(points, function(X,Y){       
  #outer <- c(X+(pxl_d/2), Y-(pxl_d/2)) %>% 
  #  rbind(c(X-(pxl_d/2), Y-(pxl_d/2))) %>% 
  #  rbind(c(X-(pxl_d/2), Y+(pxl_d/2))) %>% 
  #  rbind(c(X+(pxl_d/2), Y+(pxl_d/2))) %>% 
  #  rbind(c(X+(pxl_d/2), Y-(pxl_d/2)))
  #   outer <- c(X, Y) %>% 
  #     rbind(c(X+pxl_d, Y)) %>% 
  #     rbind(c(X+pxl_d, Y+pxl_d)) %>% 
  #     rbind(c(X, Y+pxl_d)) %>% 
  #     rbind(c(X, Y))
  #   return(st_polygon(list(outer)))
  # }) %>% st_as_sfc() %>% as.data.frame()
  # 
  # pixls_squares <- pixls_sf %>% 
  #   st_drop_geometry() %>% 
  #   cbind(pixls_crs)
  # 
  # pixls_sq <- 
  # pixls_squares <- pixls_sf %>% 
  #   st_drop_geometry() %>% 
  #   cbind(pixls_crs)
  
  # Assigining Neighbourhoods ID
  # for (nb in 1:nrow(nbhd)){
  #   varname <- as.character(nb)
  #   pixls <- pixls %>%     
  #     mutate(x_dist = cdist(x,nbhd$x[nb], metric = "euclidean")) %>%
  #     mutate(y_dist = cdist(y,nbhd$y[nb], metric = "euclidean")) %>%
  #     mutate(!!varname := x_dist + y_dist) %>%
  #     select(-x_dist,-y_dist)
  # }
  # pixls <- pixls %>% mutate(nbhd = NA)
  # for (px in 1:nrow(pixls)){
  #   px_row <- pixls[px, ] %>% select(-c(x,y, nbhd, dist_to_centre))
  #   pixls$nbhd[px] <- colnames(px_row)[which.min(px_row)]
  # }
  
  # Adding IDs and filtering those beyond nbhds counter
  # pixls <- pixls %>% mutate(ID = seq(1:n())) %>% select(ID, x, y, nbhd)
  
  # Adding total_pop to neighbourhoods
  
  #remaining_population <- total_pop
  #for (i in 1:pixls_counter){
   # pixls[i,"pop"] <-  min(pixls_pop, remaining_population) # TODO create a basis for distributing this
  #  for(iter_dest_type in dest$dest_type_id){
   #   pixls[i, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] <- pixls$pop[i]
  #  }
  #  remaining_population <- remaining_population - pixls$pop[i]
  #}
  return(pixls_sq)
}