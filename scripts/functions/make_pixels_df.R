make_pixels_df <- function(pixl_diameter, share_land_for_dest, total_pop, dwelling_per_h, person_per_hh, init_dest){

  # Making initial nbhds df
  my_seq <- seq(from = -1.9, to = 1.9, by = 0.2)
  init_pixls <- data.frame(matrix(nrow = length(my_seq)^2, ncol = 2))
  colnames(init_pixls) <- c("x", "y")
  my_row <- 1
  for(i in seq(from = -1.9, to = 1.9, by = 0.2)){
    for(j in seq(from = -1.9, to = 1.9, by = 0.2)){
      init_pixls$x[my_row] <- i
      init_pixls$y[my_row] <- j
      my_row <- my_row + 1
    }
  }

  # calculating distance to centre
  init_pixls <- init_pixls %>%
    mutate(x_dist = cdist(x,0, metric = "euclidean")) %>%
    mutate(y_dist = cdist(y,0, metric = "euclidean")) %>%
    mutate(dist_to_centre = x_dist + y_dist) %>%
    select(x, y, dist_to_centre)
  # Sorting   
  init_pixls <- init_pixls[order(init_pixls$dist_to_centre, init_pixls$x, init_pixls$y),]
  
  # Assigining Neighbourhoods ID
  for (nb in 1:nrow(init_nbhd)){
    varname <- as.character(nb)
    init_pixls <- init_pixls %>%     
      mutate(x_dist = cdist(x,init_nbhd$x[nb], metric = "euclidean")) %>%
      mutate(y_dist = cdist(y,init_nbhd$y[nb], metric = "euclidean")) %>%
      mutate(!!varname := x_dist + y_dist) %>%
      select(-x_dist,-y_dist)
  }
  init_pixls <- init_pixls %>% mutate(nbhd = NA)
  for (px in 1:nrow(init_pixls)){
    px_row <- init_pixls[px, ] %>% select(-c(x,y, nbhd, dist_to_centre))
    init_pixls$nbhd[px] <- colnames(px_row)[which.min(px_row)]
  }
  
  # Adding IDs and filtering those beyond nbhds counter
  init_pixls <- init_pixls %>% mutate(ID = seq(1:n())) %>% select(ID, x, y, nbhd)
  
  # Adding total_pop to neighbourhoods
  
  #remaining_population <- total_pop
  #for (i in 1:pixls_counter){
   # init_pixls[i,"pop"] <-  min(pixls_pop, remaining_population) # TODO create a basis for distributing this
  #  for(iter_dest_type in init_dest$dest_type_id){
   #   init_pixls[i, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] <- init_pixls$pop[i]
  #  }
  #  remaining_population <- remaining_population - init_pixls$pop[i]
  #}
  return(init_pixls)
}