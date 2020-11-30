
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(iter_pixls, iter_dest_code){
  unsrvd <- iter_pixls %>% 
    dplyr::select(paste("not_served_by_", iter_dest_code, sep = "")) %>% 
    st_drop_geometry() %>% 
    sum()
  return(unsrvd)
} 

# Function to add destinations to locations -------------------------------
add_destination_to_location <- function(iter_loc, new_dest_loc_id, iter_dest_code){
  
  loc_row <- which(iter_loc$loc_id == as.integer(new_dest_loc_id))
  loc_col <- paste0("num_dest_", iter_dest_code)
  
  iter_loc[loc_row,loc_col] <- iter_loc[loc_row,loc_col] + 1
  new_capacity <- iter_dest$pop_req[which(iter_dest$dest_code == iter_dest_code)]
  pop_total_col <- paste0("pop_total_",iter_dest_code)
  iter_loc[loc_row,pop_total_col] <- iter_loc[loc_row,pop_total_col] + new_capacity
  pop_remainder_col <- paste0("pop_remaining_",iter_dest_code)
  iter_loc[loc_row,pop_remainder_col] <- iter_loc[loc_row,pop_remainder_col] + new_capacity
  
  return(iter_loc)
}


# Function to find land contributors for each nbhd ------------------------
find_land_contributors <- function(iter_loc, new_dest_loc_id, iter_nbhds){
  loc_row <- which(iter_loc$loc_id == new_dest_loc_id)
  loc_nbhds <- iter_loc[loc_row,] %>% # Getting the nbhd intersecting with loc
    st_as_sf(coords=c("x","y"),remove=F) %>% 
    st_buffer(0.1) %>% 
    st_intersects(iter_nbhds)
  loc_nbhds <- loc_nbhds[!is.na(loc_nbhds)]
  return(unlist(loc_nbhds))
}


# Function to find out the remaining land is sufficent or not -------------
check_total_land <- function(land_to_occupy,loc_nbhds,iter_nbhds){
  total_land_in_nbhds <- 0
  for(this_nbhd in loc_nbhds){
    nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
    total_land_in_nbhds <- total_land_in_nbhds + iter_nbhds$remaining_land_for_dest[nbhd_row]
  }
  if(total_land_in_nbhds < land_to_occupy) return(FALSE) else return(TRUE)
}

# Function to occupy land in nbhds ----------------------------------------
occupy_land <- function(this_loc_nbhds, iter_nbhds, land_to_occupy, 
                        land_to_occupy_per_nbhd){
  land_to_occupy_per_nbhd <- land_to_occupy/length(this_loc_nbhds)
  for(this_nbhd in this_loc_nbhds){
    # Where is this neighbourhood?
    nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
    land_to_occupy_per_this <- min(land_to_occupy_per_nbhd, iter_nbhds$remaining_land_for_dest[nbhd_row] )
    iter_nbhds$remaining_land_for_dest[nbhd_row] <- iter_nbhds$remaining_land_for_dest[nbhd_row] - land_to_occupy_per_this
    land_to_occupy <- land_to_occupy - land_to_occupy_per_this
  }
  # if there is still land to occupy, neibhourhoods will contribute as much as they can (order is random)
  if(land_to_occupy >0){
    #print("donation process")
    while(land_to_occupy >0){
      this_nbhd <- sample(loc_nbhds, size = 1)
      nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
      land_to_occupy_per_this <- min(land_to_occupy, iter_nbhds$remaining_land_for_dest[nbhd_row] )
      iter_nbhds$remaining_land_for_dest[nbhd_row] <- iter_nbhds$remaining_land_for_dest[nbhd_row] - land_to_occupy_per_this
      land_to_occupy <- land_to_occupy - land_to_occupy_per_this
    }
  }
  return(iter_nbhds)
}

# Feasible Location Finder ------------------------------------------------
find_feasible_locs <- function(iter_loc, iter_pixls, iter_dest, 
                               iter_dest_row,iter_dest_position,consider_categories){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  
  if(consider_categories){
    if(iter_dest_position%in%c("ltc","nltc","etc")){
      feasible_locs <- iter_loc %>% # getting all the decision points for this dest
        filter(position==iter_dest_position) 
    }else{
      dest_col_name <- paste0("num_dest_",iter_dest_position)
      feasible_locs <- iter_loc[which(iter_loc[,dest_col_name]>0),]
    }
  }else{
    feasible_locs <- iter_loc 
  }
  
  # potential catchment (based on having less than 20 minutes access)
  for (i in 1:nrow(feasible_locs)){
    feasible_pxls <- feasible_locs[i,] %>% 
      st_as_sf(coords=c("x","y")) %>% 
      st_buffer(as.numeric(iter_dest[iter_dest_row,"dist_in_20_min"])) %>% 
      st_intersects(st_centroid(iter_pixls),sparse = F)

    if(length(feasible_pxls)>0){
      catchment_potential <- iter_pixls[feasible_pxls,] %>% 
        st_drop_geometry() %>% 
        select(notServedPop=paste0("not_served_by_", iter_dest_code)) %>%  
        summarise(total=sum(notServedPop)) %>% 
        as.numeric()
    }else catchment_potential <- 0
    
    feasible_locs$catchment_potential[i] <- min(iter_dest$pop_req[iter_dest_row], 
                                                catchment_potential) 
  }
  
  # Finding the potential catchment for each location
  # Remove those with catchment = 0
  feasible_locs <- feasible_locs %>% 
    filter(catchment_potential>0) %>% 
    arrange(desc(catchment_potential))
  # order by catchment
  return(feasible_locs)
}