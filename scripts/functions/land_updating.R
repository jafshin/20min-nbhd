
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
  iter_loc[temp_loc_row, paste("num_dest_", iter_dest_type, sep = "")] <- iter_loc[temp_loc_row, paste("num_dest_", iter_dest_type, sep = "")] + 1
  return(iter_loc)
}


find_land_contributors <- function(iter_loc, iter_deci, new_deci_row){
  # UPDATING NEIGHBOURHOODS REMAINING LAND WITH THE NEW DESTINATION ADDED
  temp_loc_row <- which(iter_loc$loc_id == iter_deci$loc_id[new_deci_row])
  temp_loc_nbhds <- iter_loc[temp_loc_row, c("NB1","NB2","NB3","NB4")] 
  temp_loc_nbhds <- temp_loc_nbhds[!is.na(temp_loc_nbhds)]
  return(temp_loc_nbhds)
  
}



check_total_land <- function(land_to_occupy,temp_loc_nbhds,iter_nbhds){
  # Total remaining land
  total_land_in_nbhds <- 0
  for(this_nbhd in temp_loc_nbhds){
    temp_nbhd_row <- which(iter_nbhds$ID == this_nbhd)
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
    temp_nbhd_row <- which(iter_nbhds$ID == this_nbhd)
    land_to_occupy_per_this <- min(land_to_occupy_per_nbhd, iter_nbhds$remaining_land_for_dest[temp_nbhd_row] )
    iter_nbhds$remaining_land_for_dest[temp_nbhd_row] <- iter_nbhds$remaining_land_for_dest[temp_nbhd_row] - land_to_occupy_per_this
    land_to_occupy <- land_to_occupy - land_to_occupy_per_this
  }
  # if there is still land to occupy, neibhourhoods will contribute as much as they can (order is random)
  if(land_to_occupy >0){
    #print("donation process")
    while(land_to_occupy >0){
      this_nbhd <- sample(temp_loc_nbhds, size = 1)
      temp_nbhd_row <- which(iter_nbhds$ID == this_nbhd)
      land_to_occupy_per_this <- min(land_to_occupy, iter_nbhds$remaining_land_for_dest[temp_nbhd_row] )
      iter_nbhds$remaining_land_for_dest[temp_nbhd_row] <- iter_nbhds$remaining_land_for_dest[temp_nbhd_row] - land_to_occupy_per_this
      land_to_occupy <- land_to_occupy - land_to_occupy_per_this
    }
  }
  return(iter_nbhds)
}



# Feasible Location Finder ------------------------------------------------
find_feasible_locs <- function(this_iter_deci, this_iter_pixls, this_iter_dest, this_dest_deci_rows, this_iter_dest_row){
  
  feasible_locs <- this_iter_deci[this_dest_deci_rows,]
  # potential catchment (based on having less than 20 minutes access)
  for (i in 1:nrow(feasible_locs)){
    #print(feasible_locs[i, grepl("^[0-9]", x = colnames(iter_deci))])
    feasbile_nbhd <- feasible_locs[i, grepl("^[0-9]", x = colnames(this_iter_deci))] # TODO it should be dynamic!!!!!
    feasbile_nbhd <- feasbile_nbhd[1,which(feasbile_nbhd < feasible_locs$dist_in_20_min[i])]
    
    catchment_potential <- 0
    for (px in colnames(feasbile_nbhd)){
      catchment_potential <- catchment_potential + this_iter_pixls[which(this_iter_pixls$ID == px),paste("pop_not_served_by_dest_", iter_dest_type, sep = "")]
    }
    feasible_locs$catchment_potential[i] <- min(this_iter_dest$pop_req[this_iter_dest_row], catchment_potential) 
  }
  # Selecting the location with highest catchment
  # Remove those with catchment = 0
  feasible_locs <- feasible_locs[which(feasible_locs$catchment_potential >0), ]
  
  # order by catchment
  feasible_locs <- feasible_locs[order(feasible_locs$catchment_potential, decreasing = T),]
  return(feasible_locs)
}