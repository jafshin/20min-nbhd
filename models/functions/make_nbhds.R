make_nbhds <- function(nbhd_d, nbhd_n){
  c <- ceiling(sqrt(nbhd_n)/2) # c is the number of diameters at each dir from centre to create the study area
  nbhds <- tibble(.rows = 2*c*2*c)
  
  start_x<- -nbhd_d/2 + c*nbhd_d
  start_y <- -nbhd_d/2 + c*nbhd_d
  nbhd_sf <- expand.grid(x = seq(from = start_x, by = -nbhd_d, l = 2*c),
                        y = seq(from = start_y, by = -nbhd_d, l = 2*c)) %>%
    st_as_sf(coords=c("x","y"))  %>% 
    mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
    arrange(dist_to_centre) %>% 
    mutate(id=row_number())
    
  nbhd_crs <- nbhd_sf %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    purrr::pmap(function(X,Y){
      outer <- c(X+(nbhd_d/2), Y-(nbhd_d/2)) %>% 
        rbind(c(X-(nbhd_d/2), Y-(nbhd_d/2))) %>% 
        rbind(c(X-(nbhd_d/2), Y+(nbhd_d/2))) %>% 
        rbind(c(X+(nbhd_d/2), Y+(nbhd_d/2))) %>% 
        rbind(c(X+(nbhd_d/2), Y-(nbhd_d/2)))
      return(st_polygon(list(outer)))
    }) %>% 
    st_as_sfc() %>% 
    as.data.frame()  
  
  nbhd_sq <- nbhd_sf %>% 
    st_drop_geometry() %>% 
    cbind(nbhd_crs) %>% 
    dplyr::select(nbhd_id=id, geometry) %>% 
    st_as_sf()
  
  return(nbhd_sq)
}