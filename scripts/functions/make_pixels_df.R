make_pixels_df <- function(pixl_diameter, share_land_for_dest, total_pop, dwelling_per_h, person_per_hh, dest){

  # Making initial nbhds df
  my_seq <- seq(from = -1.9, to = 1.9, by = 0.2)
  pixls <- data.frame(matrix(nrow = length(my_seq)^2, ncol = 2))
  colnames(pixls) <- c("x", "y")
  my_row <- 1
  for(i in seq(from = -1.9, to = 1.9, by = 0.2)){
    for(j in seq(from = -1.9, to = 1.9, by = 0.2)){
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
  
  # Assigining Neighbourhoods ID
  for (nb in 1:nrow(nbhd)){
    varname <- as.character(nb)
    pixls <- pixls %>%     
      mutate(x_dist = cdist(x,nbhd$x[nb], metric = "euclidean")) %>%
      mutate(y_dist = cdist(y,nbhd$y[nb], metric = "euclidean")) %>%
      mutate(!!varname := x_dist + y_dist) %>%
      select(-x_dist,-y_dist)
  }
  pixls <- pixls %>% mutate(nbhd = NA)
  for (px in 1:nrow(pixls)){
    px_row <- pixls[px, ] %>% select(-c(x,y, nbhd, dist_to_centre))
    pixls$nbhd[px] <- colnames(px_row)[which.min(px_row)]
  }
  
  # Adding IDs and filtering those beyond nbhds counter
  pixls <- pixls %>% mutate(ID = seq(1:n())) %>% select(ID, x, y, nbhd)
  
  # Adding total_pop to neighbourhoods
  
  #remaining_population <- total_pop
  #for (i in 1:pixls_counter){
   # pixls[i,"pop"] <-  min(pixls_pop, remaining_population) # TODO create a basis for distributing this
  #  for(iter_dest_type in dest$dest_type_id){
   #   pixls[i, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] <- pixls$pop[i]
  #  }
  #  remaining_population <- remaining_population - pixls$pop[i]
  #}
  return(pixls)
}