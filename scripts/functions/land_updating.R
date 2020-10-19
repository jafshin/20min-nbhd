
# Function to add destination to decision points --------------------------
add_destination_to_decision <- function(iter_deci,new_deci_row,iter_dest_type){
  iter_deci$num_open[new_deci_row] <- iter_deci$num_open[new_deci_row] + 1
  new_capacity <- iter_dest$pop_req[which(iter_dest$dest_type_id == iter_dest_type)]
  iter_deci$pop_total[new_deci_row] <- iter_deci$pop_total[new_deci_row] + new_capacity
  iter_deci$pop_remainder[new_deci_row] <- iter_deci$pop_remainder[new_deci_row] + new_capacity
  return(iter_deci)
}


# Function to add destinations to locations -------------------------------
add_destination_to_location <- function(iter_loc, iter_deci, 
                                        new_deci_row, iter_dest_type){
  loc_row <- which(iter_loc$loc_id == iter_deci$loc_id[new_deci_row])
  loc_col <- paste("num_dest_", iter_dest_type, sep = "")
  iter_loc[loc_row, loc_col] <- st_drop_geometry(iter_loc[loc_row, loc_col]) + 1
  return(iter_loc)
}


# Function to find land contributors for each nbhd ------------------------
find_land_contributors <- function(iter_loc, iter_deci, iter_nbhds, new_deci_row){
  loc_row <- which(iter_loc$loc_id == iter_deci$loc_id[new_deci_row])
  loc_nbhds <- iter_loc[loc_row,] %>% # Getting the nbhd intersecting with loc
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
  if(total_land_in_nbhds < land_to_occupy) print(FALSE) else print(TRUE)
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
find_feasible_locs <- function(this_iter_deci, this_iter_pixls, this_iter_dest, 
                               this_iter_dest_row){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 1.5* the 20 min access
  
  #this_iter_deci <- iter_deci
  #this_iter_pixls <- iter_pixls
  #this_iter_dest <- iter_dest
  #this_iter_dest_row <- iter_dest_row
  
  feasible_locs <- this_iter_deci %>% # getting all the decision points for this dest
    filter(dest_type_id == iter_dest_type) 
  
  # potential catchment (based on having less than 20 minutes access)
  #i <- 113
  for (i in 1:nrow(feasible_locs)){
    feasible_pxls <- feasible_locs[i,] %>% 
      st_buffer(as.numeric(feasible_locs[i,"dist_in_20_min"])) %>% 
      st_intersects(iter_pixls) %>% 
      unlist()
    
    if(length(feasible_pxls)>0){
      catchment_potential <- this_iter_pixls %>% 
        st_drop_geometry() %>% 
        filter(ID %in% feasible_pxls) %>%  
        select(notServedPop=paste0("not_served_by_", iter_dest_type)) %>%  
        summarise(total=sum(notServedPop)) %>% 
        as.numeric()
    }else catchment_potential <- 0
    
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