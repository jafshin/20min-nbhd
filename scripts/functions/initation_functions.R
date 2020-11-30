
make_decsion_df<-function(init_loc,init_dest){ # TODO make it efficient
  for (i in 1:nrow(init_dest)){
    init_deci <- init_loc %>% 
      mutate(!!paste0("num_dest_", init_dest$dest_type_id[i]):=0) %>% 
      mutate(!!paste0("pop_total_", init_dest$dest_type_id[i]):=0) %>% 
      mutate(!!paste0("pop_remaining_", init_dest$dest_type_id[i]):=0)
    }
  return(init_deci)
}


make_location_df_methdod_1 <- function(init_loc, init_dest, init_nbhd_with_pop ){
  
  for (i in 1:nrow(init_dest)){
    new_col_name <- paste("num_dest", init_dest$dest_type_id[i], sep = "_")
    init_loc[, get("new_col_name")] <- rep(0, nrow(init_loc))
  }
  
  # Filtering those not feasible
  # METHOD 1: AT least one part should be in the selected nbhds!
  for(i in 1:nrow(init_loc)){
    flag <- FALSE
    for(j in 1:nrow(init_nbhd_with_pop)){
      temp_loc_nbhds <- init_loc[i, c("NB1","NB2","NB3","NB4")] 
      temp_loc_nbhds <- temp_loc_nbhds[!is.na(temp_loc_nbhds)]
      
      if(init_nbhd_with_pop$ID[j] %in% temp_loc_nbhds){
        flag <- TRUE
      }
      
      #x_dist <- cdist(init_loc$x[i], init_nbhd$x[j], metric = "manhattan")
      #y_dist <- cdist(init_loc$y[i], init_nbhd$y[j], metric = "manhattan")
      #if(x_dist + y_dist < 1.33){
      #  flag <- TRUE
      #}
    }
    init_loc$feasible[i] <- flag
  }
  init_loc <- init_loc %>% filter(feasible == TRUE) 
  return(init_loc)
}

make_location_df_methdod_2 <- function(init_loc, init_dest, init_nbhd_with_pop ){
  # Filtering those not feasible
  # METHOD 2: At 0.8 km distance !
  for (i in 1:nrow(init_dest)){
    new_col_name <- paste("num_dest", init_dest$dest_type_id[i], sep = "_")
    init_loc[, get("new_col_name")] <- rep(0, nrow(init_loc))
  }
  for(i in 1:nrow(init_loc)){
    flag <- FALSE
    for(j in 1:nrow(init_nbhd_with_pop)){
      x_dist <- cdist(init_loc$x[i], init_nbhd_with_pop$x[j], metric = "manhattan")
      y_dist <- cdist(init_loc$y[i], init_nbhd_with_pop$y[j], metric = "manhattan")
      if(x_dist + y_dist < 0.8){
        flag <- TRUE
      }
    }
    init_loc$feasible[i] <- flag
    
  }
  init_loc <- init_loc %>% filter(feasible == TRUE) 
  return(init_loc)
}

add_distances <- function(init_deci, init_pixls){
  # Adding dists
  for (j in 1:nrow(init_pixls)){
    new_col_name <- paste0("distance_to_",as.character(init_pixls$ID[j]))
    init_deci <- init_deci %>% 
      mutate(!!new_col_name:=as.numeric(st_distance(.,init_pixls[j])))
  }  
  return(init_deci)
}

add_decision_vars <- function(init_deci, init_nbhd){
  # Adding iter_deci variables
  for (i in 1:nrow(init_deci)){
    for (j in 1:nrow(init_nbhd)){
      new_col_name <- paste("is_serving", init_nbhd$ID[j], sep = "_")
      init_deci[i, get("new_col_name")] <- 0
    }
  }
  return(init_deci)
}

