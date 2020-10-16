
add_destination_to_decision <- function(iter_deci,new_deci_row,iter_dest_type){
  iter_deci$num_open[new_deci_row] <- iter_deci$num_open[new_deci_row] + 1
  
  # Adding new capacities
  new_capacity <- iter_dest$pop_req[which(iter_dest$dest_type_id == iter_dest_type)]
  iter_deci$total_pop_cap[new_deci_row] <- iter_deci$total_pop_cap[new_deci_row] + new_capacity
  iter_deci$remaining_pop_cap[new_deci_row] <- iter_deci$remaining_pop_cap[new_deci_row] + new_capacity
  return(iter_deci)
}


add_destination_to_location <- function(iter_loc, iter_deci, new_deci_row, iter_dest_type){
  # UPDATING NEIGHBOURHOODS REMAINING LAND WITH THE NEW DESTINATION ADDED
  temp_loc_row <- which(iter_loc$loc_id == iter_deci$loc_id[new_deci_row])
  iter_loc[temp_loc_row, paste("num_dest_", iter_dest_type, sep = "")] <- st_drop_geometry(iter_loc[temp_loc_row, paste("num_dest_", iter_dest_type, sep = "")]) + 1
  return(iter_loc)
}


find_land_contributors <- function(iter_loc, iter_deci, iter_nbhds, new_deci_row){
  # UPDATING NEIGHBOURHOODS REMAINING LAND WITH THE NEW DESTINATION ADDED
  temp_loc_row <- which(iter_loc$loc_id == iter_deci$loc_id[new_deci_row])
  temp_loc_nbhds <- iter_loc[temp_loc_row,] %>% # Getting the nbhd intersecting with loc
    st_buffer(0.1) %>% 
    st_intersects(iter_nbhds)
  temp_loc_nbhds <- temp_loc_nbhds[!is.na(temp_loc_nbhds)]
  return(unlist(temp_loc_nbhds))
}



check_total_land <- function(land_to_occupy,temp_loc_nbhds,iter_nbhds){
  # Total remaining land
  total_land_in_nbhds <- 0
  for(this_nbhd in temp_loc_nbhds){
    temp_nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
    total_land_in_nbhds <- total_land_in_nbhds + iter_nbhds$remaining_land_for_dest[temp_nbhd_row]
  }
  if(total_land_in_nbhds < land_to_occupy){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
  
occupy_land <- function(this_temp_loc_nbhds, iter_nbhds, land_to_occupy, land_to_occupy_per_nbhd){
  for(this_nbhd in this_temp_loc_nbhds){
    # Where is this neighbourhood?
    temp_nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
    land_to_occupy_per_this <- min(land_to_occupy_per_nbhd, iter_nbhds$remaining_land_for_dest[temp_nbhd_row] )
    iter_nbhds$remaining_land_for_dest[temp_nbhd_row] <- iter_nbhds$remaining_land_for_dest[temp_nbhd_row] - land_to_occupy_per_this
    land_to_occupy <- land_to_occupy - land_to_occupy_per_this
  }
  # if there is still land to occupy, neibhourhoods will contribute as much as they can (order is random)
  if(land_to_occupy >0){
    #print("donation process")
    while(land_to_occupy >0){
      this_nbhd <- sample(temp_loc_nbhds, size = 1)
      temp_nbhd_row <- which(iter_nbhds$NBHD_ID == this_nbhd)
      land_to_occupy_per_this <- min(land_to_occupy, iter_nbhds$remaining_land_for_dest[temp_nbhd_row] )
      iter_nbhds$remaining_land_for_dest[temp_nbhd_row] <- iter_nbhds$remaining_land_for_dest[temp_nbhd_row] - land_to_occupy_per_this
      land_to_occupy <- land_to_occupy - land_to_occupy_per_this
    }
  }
  return(iter_nbhds)
}



# Feasible Location Finder ------------------------------------------------
find_feasible_locs <- function(this_iter_deci, this_iter_pixls, this_iter_dest, this_dest_deci_rows, this_iter_dest_row){
  # a function to find feasbile decision locations, we need this to limit the search space
  #this_iter_deci <- iter_deci
  #this_iter_pixls <- iter_pixls
  #this_iter_dest <- iter_dest
  #this_iter_dest_row <- iter_dest_row
  
  feasible_locs <- this_iter_deci[this_dest_deci_rows,]
  
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 1.5* the 20 min access
  
  # potential catchment (based on having less than 20 minutes access)
  #i <- 113
  for (i in 1:nrow(feasible_locs)){
    feasible_pxls <- feasible_locs[i,] %>% 
      st_buffer(as.numeric(feasible_locs[i,"dist_in_20_min"])*2) %>% 
      st_intersects(iter_pixls) %>% 
      unlist()
    
    if(length(feasible_pxls)>0){
      catchment_potential <- this_iter_pixls %>% 
        st_drop_geometry() %>% 
        filter(ID %in% feasible_pxls) %>%  
        select(notServedPop=paste0("not_served_by_", iter_dest_type)) %>%  
        summarise(total=sum(notServedPop)) %>% 
        as.numeric()
    }else{
      catchment_potential <- 0
    }
    
    feasible_locs$catchment_potential[i] <- min(this_iter_dest$pop_req[this_iter_dest_row], 
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