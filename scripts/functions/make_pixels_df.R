make_pixels_df <- function(pxl_d, total_pop, dwelling_per_h, person_per_hh, study_area_d, nbhd_sq){
 
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
  
  # Making the cells
  pixls_crs <- pixls_sf %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    pmap(function(X,Y){
       outer <- c(X, Y) %>% 
         rbind(c(X+pxl_d, Y)) %>% 
         rbind(c(X+pxl_d, Y+pxl_d)) %>% 
         rbind(c(X, Y+pxl_d)) %>% 
         rbind(c(X, Y))
      return(st_polygon(list(outer)))
    }) %>% 
    st_as_sfc() %>% 
    as.data.frame()
  
  # Getting the cell centre coordinates
  pixls_centres <- pixls_crs %>% 
    mutate(ID = row_number()) %>% 
    st_as_sf() %>% 
    st_centroid() %>% 
    st_join(nbhd_sq, largest = T)
  
  # Joining cells and  their centre coordinates
  pixls_sq <- pixls_sf %>% 
    st_drop_geometry() %>% 
    mutate(ID = row_number()) %>% 
    left_join(st_drop_geometry(pixls_centres)) %>% 
    cbind(pixls_crs) %>% 
    dplyr::select(ID, NBHD_ID, dist_to_centre, geometry) %>% 
    st_as_sf()
  
  return(pixls_sq)
}