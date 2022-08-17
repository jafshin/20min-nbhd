
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(cells_temp, dest_code){
  unsrvd <- cells_temp %>% 
    dplyr::select(paste("not_served_by_", dest_code, sep = "")) %>% 
    sum()
  return(unsrvd)
} 

findSpace <- function(cells_temp,dest_code, cells_to_occupy, dest_lvl){
  
  if(dest_lvl==1) cells_temp_grouped <- cells_temp %>% group_by(nbhd_q)
  if(dest_lvl==2) cells_temp_grouped <- cells_temp %>% group_by(nbhd_id)
  if(dest_lvl==3) cells_temp_grouped <- cells_temp 
  
  feasible_area <- cells_temp_grouped %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
              cells=n(),
              cells_with_dest=sum(wt)) %>% 
    filter(cells_to_occupy+cells_with_dest<0.35*cells) %>% # Making sure there is enough space
    filter(area_catchment_potential>0) %>% 
    arrange(desc(area_catchment_potential)) 
  
  if(nrow(feasible_area)==0){
    echo("increasing destination space to 50%")
    feasible_area <- cells_temp_grouped %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
                cells=n(),
                cells_with_dest=sum(wt)) %>% 
      filter(cells_to_occupy+cells_with_dest<0.5*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }

  if(nrow(feasible_area)==0){
    echo("increasing destination space to 70%")
    feasible_area <- cells_temp_grouped %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
                cells=n(),
                cells_with_dest=sum(wt)) %>% 
      filter(cells_to_occupy+cells_with_dest<0.7*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }
  
  has_space <- T
  if (nrow(feasible_area)==0){
    discardRunFlag <<- T 
    has_space <- F
    echo("Not enough Space - skipping")
  }  
  return(has_space)
}

# Feasible Location Finder ------------------------------------------------
findDestinationCells <- function(cells_temp,dest_list,cells_to_occupy,
                                 dest_code,pxl_a,dest_lvl,dest_r){
  
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  # cells_temp <-pxlsInitial
  # cells_temp <- cells_initial
  
  # cells_temp=cells_mutation
  # dest_list
  # cells_to_occupy=cells_to_move
  # dest_code=dest_to_update_type
  # pxl_a
  # dest_lvl=dest_to_update_lvl
  # dest_r= dest_to_update_radius
  
  if(dest_lvl==1) cells_temp_grouped <- cells_temp %>% group_by(nbhd_q)
  if(dest_lvl==2) cells_temp_grouped <- cells_temp %>% group_by(nbhd_id)
  if(dest_lvl==3) cells_temp_grouped <- cells_temp 
  
  feasible_area <- cells_temp_grouped %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
              cells=n(),
              cells_with_dest=sum(wt)) %>% 
    filter(cells_to_occupy+cells_with_dest<0.35*cells) %>% # Making sure there is enough space
    filter(area_catchment_potential>0) %>% 
    arrange(desc(area_catchment_potential)) 
  
  if(nrow(feasible_area)==0){
    echo("increasing destination space to 50%")
    feasible_area <- cells_temp_grouped %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
                cells=n(),
                cells_with_dest=sum(wt)) %>% 
      filter(cells_to_occupy+cells_with_dest<0.5*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }

  if(nrow(feasible_area)==0){
    echo("increasing destination space to 70%")
    feasible_area <- cells_temp_grouped %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]]),
                cells=n(),
                cells_with_dest=sum(wt)) %>% 
      filter(cells_to_occupy+cells_with_dest<0.7*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }
  
  dest_position_pref <- dest_list[which(dest_list$dest_code==dest_code),"position"] %>% unlist()
  
  if(dest_lvl==1) {
    # Finding the best area based on the level 
    areas_with_prefered_dest<- cells_temp_grouped %>% 
      filter(type==dest_position_pref) %>% 
      summarise() %>% 
      unlist()
    
    feasible_area <- feasible_area %>% filter(nbhd_q%in%areas_with_prefered_dest)
    
    # Selecting the best area based on the level
    new_dest_area <- feasible_area %>%
      filter(area_catchment_potential == max(area_catchment_potential)) %>%
      dplyr::select(nbhd_q) %>% 
      sample_n(size = 1) %>% 
      as.character()
    cells_in_area <- cells_temp %>% filter(nbhd_q==new_dest_area)
    }
  if(dest_lvl==2){
    # Finding the best area based on the level 
    areas_with_prefered_dest<- cells_temp_grouped %>% 
      filter(type==dest_position_pref) %>% 
      summarise() %>% 
      unlist()
    
    feasible_area <- feasible_area %>% filter(nbhd_id%in%areas_with_prefered_dest)
    # Selecting the best area based on the level     
    new_dest_area <- feasible_area %>%
      filter(area_catchment_potential == max(area_catchment_potential)) %>%
      dplyr::select(nbhd_id) %>% 
      sample_n(size = 1) %>% 
      as.character()
    cells_in_area <- cells_temp %>% 
      filter(nbhd_id==new_dest_area)
    }
  if(dest_lvl==3){ cells_in_area <- cells_temp }
  
  # Select 10 cell centre matching the co-location preference
  if(dest_position_pref=="resid"){
  sample_cells <- cells_in_area %>% 
    filter(type=="resid") %>% 
    sample_n(10) 
  }else{ 
    sample_cells <- cells_in_area %>% 
      filter(type==dest_position_pref) %>% 
      sample_n(min(10,n())) 
  }
  
  # Selecting the best place for the dest within the area from a sample of 10
  bestCatchment <- 0
  #i <- 1
  for(i in 1:nrow(sample_cells)){
    x <- sample_cells[i,"pxl_x"]
    y <- sample_cells[i,"pxl_y"]
    # echo(paste(x,y))
    
    cells_within_bb <- cells_temp %>% 
      # Selecting cells within the bounding box
    filter(pxl_x <= x+(0.8+dest_r) & pxl_x >= x-(0.8+dest_r) & 
             pxl_y >= y-(0.8+dest_r) & pxl_y <= y+(0.8+dest_r)
    )  
    
    # Select the closest ones for            
    cells_within_bb <- cells_within_bb %>%            
      mutate(dist = sqrt((pxl_x-x)^2 + (pxl_y-y)^2)) %>% 
      arrange(dist)
    
    catchment20min <- cells_within_bb %>% 
      filter(dist>=dest_r & dist<=(0.8+dest_r) ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", dest_code)]])) %>% 
      unlist()

    if(catchment20min > bestCatchment){
      cells_selected <- cells_within_bb %>% 
        filter(type==dest_position_pref) %>% 
        slice_head(n=cells_to_occupy)
      
      bestCatchment <- catchment20min
    }
  }
  return(cells_selected$id)
}
