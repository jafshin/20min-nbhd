
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(iter_pixls, iter_dest_code){
  unsrvd <- iter_pixls %>% 
    dplyr::select(paste("not_served_by_", iter_dest_code, sep = "")) %>% 
    sum()
  return(unsrvd)
} 

# Function to add destinations to locations -------------------------------
add_destination_to_location <- function(iter_loc, new_dest_loc_id, iter_dest_code,
                                        iter_dest){
  
  loc_row <- which(iter_loc$loc_id == as.integer(new_dest_loc_id))
  loc_col <- paste0("num_dest_", iter_dest_code)
  
  iter_loc[loc_row,loc_col] <- iter_loc[loc_row,loc_col] + 1
  new_capacity <- iter_dest$pop_req[which(iter_dest$dest_code == iter_dest_code)] *1.5
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
      this_nbhd <- sample(this_loc_nbhds, size = 1)
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
                               iter_dest_row,iter_dest_position,consider_categories, iter_dest_code){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  
  if(consider_categories){
    if(iter_dest_position%in%c("ltc","nltc","etc")){
      feasible_locs <- iter_loc %>% # getting all the decision points for this dest
        filter(position==iter_dest_position) 
    }else if(iter_dest_position=="all"){
      feasible_locs <- iter_loc
    }
    else{
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


# Feasible Location Finder ------------------------------------------------
find_feasible_locs2 <- function(iter_pixls, iter_dest, iter_dest_row,
                                iter_dest_position,consider_categories, 
                                iter_dest_code){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  
  if(consider_categories){
    if(iter_dest_position%in%c("ltc","nltc","etc")){
      feasible_locs <- iter_pixls %>% # getting all the decision points for this dest
        filter(position==iter_dest_position) 
    }else if(iter_dest_position=="all"){
      feasible_locs <- iter_pixls
    }
    else{
      dest_col_name <- paste0("num_dest_",iter_dest_position)
      feasible_locs <- iter_pixls[which(iter_pixls[,dest_col_name]>0),]
    }
  }else{
    feasible_locs <- iter_pixls 
  }
  feasible_locs <- feasible_locs %>% 
    mutate(catchment_potential=0)
  # potential catchment (based on having less than 20 minutes access)
  #i=1
  for (i in 1:nrow(feasible_locs)){
  # for (i in 1:10){
    feasible_pxls <- feasible_locs[i,] %>% 
      st_as_sf(coords=c("pxl_x","pxl_y")) %>% 
      st_buffer(as.numeric(iter_dest[iter_dest_row,"dist_in_20_min"])) %>% 
      st_intersects(st_centroid(iter_pixls %>% st_as_sf(coords=c("pxl_x","pxl_y")))
                    ,sparse = F) 
    ## TO HERE
    
    if(length(feasible_pxls)>0){
      catchment_potential <- iter_pixls[feasible_pxls,] %>% 
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

findDestinationCells <- function(iter_pixls,iter_dest,cellsToOccupy,
                                iter_dest_code,pxl_a){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  
  
  feasible_nbhds <- iter_pixls %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    group_by(nbhdQ) %>% 
    summarise(catchment_potential = sum(.data[[paste0("not_served_by_", iter_dest_code)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(catchment_potential>0) %>% 
    arrange(desc(catchment_potential))  
  
  # selecting one from max catchments by random
  new_dest_nbhd <- feasible_nbhds %>%
    filter(catchment_potential == max(catchment_potential)) %>%
    dplyr::select(nbhdQ) %>% 
    sample_n(size = 1) %>% 
    as.character()
  

  centrePxl <- iter_pixls %>% 
    filter(nbhdQ==new_dest_nbhd) %>% 
    filter(type=="resid") %>% 
    sample_n(1) %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)
  
  closePxls <- centrePxl %>% 
    st_buffer(0.4) %>% 
    st_intersection(iter_pixls %>% 
                      filter(type=="resid") %>% 
                      st_as_sf(coords=c("pxl_x","pxl_y"))) %>% 
    mutate(dist2Pxl = st_distance(.,centrePxl)) %>% 
    arrange(dist2Pxl)
  
  plxs4Dest <- closePxls %>%  slice_head(n=cellsToOccupy)
  
  # st_write(centrePxl, "centrePxl.sqlite")
  # st_write(closePxls, "closePxls2.sqlite", delete_layer = T)
  # st_write(plxs4Dest, "plxs4Dest.sqlite", delete_layer = T)
  # order by catchment
  return(plxs4Dest$ID.1)
}