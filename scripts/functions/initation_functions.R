
make_decsion_df<-function(init_loc,init_dest){
  init_deci <- data.frame()
  for (i in 1:nrow(init_dest)){
    temp_df <- init_loc %>%
      select(loc_id, x, y, TYPE, NB1, NB2, NB3, NB4) %>%
      mutate(dest_type_id = init_dest$dest_type_id[i]) %>%
      mutate(dest_id = paste(loc_id,dest_type_id, sep = "_")) %>%
      mutate(num_open = 0, total_pop_cap = 0, remaining_pop_cap = 0)%>%
      mutate("dist_in_20_min" =  init_dest$dist_in_20_min[i])
    
    #temp_matrix <- matrix(rep(init_dest$dest_type_id[i], numReps), ncol=1)
    
    init_deci <- rbind(init_deci, temp_df)
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

add_distances <- function(init_deci, init_nbhd){
  # Adding dists
  for (i in 1:nrow(init_deci)){
    for (j in 1:nrow(init_nbhd)){
      new_col_name <- as.character(init_nbhd$ID[j])
      x_dist <- cdist(init_deci$x[i], init_nbhd$x[j], metric = "euclidean") # euclidean, manhattan, minkowski, etc.
      y_dist <- cdist(init_deci$y[i], init_nbhd$y[j], metric = "euclidean")
      init_deci[i, get("new_col_name")] <- x_dist + y_dist
    }
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

