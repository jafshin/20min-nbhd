make_locations <- function(nbhd_sq,study_area_d){
  # Making initial locations df
  locs_d <- 0.4
  my_seq <- seq(from = -(study_area_d/2)+locs_d, to = (study_area_d/2)-locs_d, by = locs_d)
  locs <- data.frame(matrix(nrow = length(my_seq)^2, ncol = 2))
  colnames(locs) <- c("x", "y")
  my_row <- 1
  for(i in seq(from = -(study_area_d/2)+locs_d, to = (study_area_d/2)-locs_d, by = locs_d)){
    for(j in seq(from = -(study_area_d/2)+locs_d, to = (study_area_d/2)-locs_d, by = locs_d)){
      locs$x[my_row] <- i
      locs$y[my_row] <- j
      my_row <- my_row + 1
    }
  }
  
  locs_sf <- locs %>% 
    st_as_sf(coords=c("x","y"), remove=F) %>% 
    mutate(loc_id=row_number()) %>% 
    mutate(position="etc")
  
  # Assining different types of locations
  
  # finding near the local town centres (400m)
  loc_ltc <- locs_sf %>%  
    st_buffer(locs_d/10) %>%
    st_intersects(st_buffer(st_centroid(nbhd_sq),0.4))
  for(i in 1:nrow(locs_sf))  if(length(loc_ltc[[i]])>0) locs_sf[i,"position"]="nltc"
  
  # Finding at the the local town centres
  loc_ltc <- locs_sf %>%  
    st_buffer(locs_d/10) %>%
    st_intersects(st_buffer(st_centroid(nbhd_sq),locs_d/10))
  for(i in 1:nrow(locs_sf))  if(length(loc_ltc[[i]])>0) locs_sf[i,"position"]="ltc"
  
  # finiding at the major town centres 
  loc_nbhds <- locs_sf %>%  
    st_buffer(locs_d/10) %>% 
    st_intersects(nbhd_sq)
  for(i in 1:nrow(locs_sf)) if(length(loc_nbhds[[i]])==4) locs_sf[i,"position"]="mtc"   
  #plot(init_pixls$geometry, col="green", alpha=0.8)
  #plot(locs_sf$geometry, col = "blue", alpha=0.8, add =T)
  #plot(nbhd_sq$geometry, lwd = 2, add=T)
  
  st_write(locs_sf, "locs.sqlite", delete_dsn = T)
  return(locs_sf)
}