

make_nbhds <- function(nbhd_diameter){
  # CALCUALTING NUMBER OF NEIGHBOURHOODS
  nbhd <- read_csv("../inputs/neighbourhoods.csv")
  

  nbhd_sf <- nbhd %>% 
    st_as_sf(coords=c("x","y"), remove=F) %>% 
    mutate(dist_to_centre=as.numeric(st_distance(.,st_point(c(0.0,0.0))))) %>% 
    arrange(dist_to_centre)
  
  # plot(nbhd_sf)
  
  nbhd_crs <- nbhd_sf %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    purrr::pmap(function(X,Y){
      outer <- c(X+(nbhd_diameter/2), Y-(nbhd_diameter/2)) %>% 
        rbind(c(X-(nbhd_diameter/2), Y-(nbhd_diameter/2))) %>% 
        rbind(c(X-(nbhd_diameter/2), Y+(nbhd_diameter/2))) %>% 
        rbind(c(X+(nbhd_diameter/2), Y+(nbhd_diameter/2))) %>% 
        rbind(c(X+(nbhd_diameter/2), Y-(nbhd_diameter/2)))
      return(st_polygon(list(outer)))
    }) %>% 
    st_as_sfc() %>% 
    as.data.frame()
  
  nbhd_sq <- nbhd_sf %>% 
    st_drop_geometry() %>% 
    cbind(nbhd_crs) %>% 
    dplyr::select(NBHD_ID=ID, geometry) %>% 
    st_as_sf()
  #plot(nbhd_sq["geometry"])
  return(nbhd_sq)
  
}