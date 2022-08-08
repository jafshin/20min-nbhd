move_homeless <- function(cells_df){
  
  # Find those to move
  non_resid_cells <- which(cells_df$type != "resid" & cells_df$pxl_pop > 0)
  
  homeless_pop <- cells_df[non_resid_cells, ] %>%
    summarise(homeless_pop = sum(pxl_pop)) %>% 
    unlist()
  
  # Find places still residential
  resid_cells <- cells_df %>% 
    filter(type == "resid") %>% 
    filter(pxl_pop > 0) 
 
  # Taking a sample of size homeless population of residential cells
  host_cells <- resid_cells %>% 
    slice_sample(n = homeless_pop, replace = T)
  lookup_table <- host_cells %>% count(id) 
  
  # Getting column indexes for "not_served_by_XX" 
  pop_serving_cols <- grep("not_", colnames(cells_df))
  
  # Setting population and unserved population of the new destinaion cells to zero
  cells_df[non_resid_cells, "pxl_pop"] <- 0
  cells_df[non_resid_cells, pop_serving_cols] <- 0
  
  # Adding as many time as the cell i was selected in the sample to its population and unserved population for all dests
  # Otherwise, not adding any population
  for (i in 1:nrow(cells_df)) {
    if(cells_df[i, "id"]%in%lookup_table$id){
      cells_df[i, "incoming_pop"] <- lookup_table[(lookup_table$id == cells_df[i, "id"]), "n"]
      cells_df[i, "pxl_pop"] <- cells_df[i, "pxl_pop"] + 
        cells_df[i, "incoming_pop"]
      cells_df[i, pop_serving_cols] <- cells_df[i, pop_serving_cols] + 
        cells_df[i, "incoming_pop"]
    }
    else{
      cells_df[i, "incoming_pop"] = 0
    }
  }
  echo("finished redistributing the population")
  return(cells_df)
}

update_service_all <- function(cells_df, dest_list){

  # Update the service -----------------------
  dests_to_update <- cells_df %>% 
    filter(type != "resid") %>% 
    group_by(type) %>% 
    summarise(area_cells=n()) %>% 
    ungroup() %>% 
    as.data.frame()
  
  for (i in 1:nrow(dests_to_update)){
    dest_to_update_type <- dests_to_update[i, "type"]
    
    # Setting col names
    total_pop_col <- paste0("pop_total_", dest_to_update_type)
    reminder_pop_col <- paste0("pop_remaining_", dest_to_update_type)      
    unserved_pop_col <- paste0("not_served_by_", dest_to_update_type)
    num_dest_col <- paste0("num_dest_", dest_to_update_type)
    dest_list_row <- which(dest_list$dest_code == dest_to_update_type)
    
    # Un-serving all those for dest_to_update_type
    resid_row_ids <- which(cells_df$type == "resid")
    cells_df[resid_row_ids, unserved_pop_col] <- cells_df[resid_row_ids, 
                                                          "pxl_pop"]
    # Returning all dests of this type to full cap
    dest_capacity <- dest_list[dest_list_row, "capacity"]
    dest_type_cell_rows <- which(cells_df$type == dest_to_update_type)
    cells_df[dest_type_cell_rows, total_pop_col] <- dest_capacity
    cells_df[dest_type_cell_rows, reminder_pop_col] <- dest_capacity
    
    # Iterating over all destinations of type dest_to_update_type
    dest_type_ids <- cells_df %>% 
      filter(type==dest_to_update_type) %>% 
      distinct(dest_id) 
    # dest <- dest_type_ids$dest_id[1]
    echo(paste0("Revisiting service provided by: ", dest_to_update_type, 
                " family"))
    
    for(dest in dest_type_ids$dest_id){
      dest_cells_row <- which(cells_df$dest_id == dest)
      dest_cells_id <- cells_df[dest_cells_row, "id"]
      
      # Start serving people around
      dest_geom <- cells_df %>% 
        filter(id%in%dest_cells_id) %>% 
        st_as_sf(coords=c("pxl_x", "pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cells_within_dist_row <- dest_geom %>% 
        st_buffer(0.8) %>% 
        st_intersects(st_as_sf(cells_df, coords=c("pxl_x", "pxl_y"), remove=F)) 
      
      cells_within_dist_id <- cells_df[unlist(cells_within_dist_row), ] %>% 
        st_as_sf(coords=c("pxl_x", "pxl_y"), remove = F) %>% 
        dplyr::mutate(dist_to_dest = st_distance(., dest_geom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist_to_dest) %>%
        dplyr::select(id)
      
      if(!nrow(cells_within_dist_id) > 0) echo("**Something is not right**")
      # Serving cells 
      system.time({
        remaining_capacity <- cells_df[dest_cells_row, reminder_pop_col][1]
        close_cells <- cells_df[which(cells_df$id %in% cells_within_dist_id$id),
                                ]
        # cell <- 1
        for(cell in 1:nrow(close_cells)) {
          # cellRow <- which(cells_df$id==id)
          cell_unserved_pop <-  close_cells[cell, unserved_pop_col]
          if (remaining_capacity > 0 & cell_unserved_pop > 0){
            pop_to_cover <- min(remaining_capacity, cell_unserved_pop) # finding population to cover
            # Updating the capapcity
            remaining_capacity <- remaining_capacity - pop_to_cover
            # Updating unserved pop
            close_cells[cell, unserved_pop_col] <- cell_unserved_pop - 
              pop_to_cover
          }
        }
        cells_df[which(
          cells_df$id %in% cells_within_dist_id$id), ] <- close_cells
        cells_df[dest_cells_row, reminder_pop_col] <- remaining_capacity
        
      })
    }
  }
  return(cells_df)
}
  
move_dest_single <- function(cells_mutation, 
                             origin_cells, 
                             dest_cells_row, 
                             dest_list, 
                             dest_to_update_type, 
                             dest_to_update_position, 
                             dest_to_update_id){

  # Setting col names
  total_pop_col <- paste0("pop_total_", dest_to_update_type)
  reminder_pop_col <- paste0("pop_remaining_", dest_to_update_type)      
  unserved_pop_col <- paste0("not_served_by_", dest_to_update_type)
  num_dest_col <- paste0("num_dest_", dest_to_update_type)
  dest_list_row <- which(dest_list$dest_code == dest_to_update_type)
  
  # Get population to move
  pop_to_move <- cells_mutation[dest_cells_row, ] %>% 
    summarise(total = sum(pxl_pop)) %>% 
    as.numeric()
  
  cells_mutation[origin_cells, "type"] <- dest_to_update_position # Setting orig cells to their parent type
  
  # Get the grant parent's ID
  parent_dest_id <- cells_mutation[origin_cells,"parent_dest_id"][1]
  grand_parent_dest_id <- cells_mutation[which(cells_mutation$dest_id==parent_dest_id),"parent_dest_id"][1]
  
  
  # Assigning origin cells parent id, to the origin cells current id
  cells_mutation[origin_cells, "dest_id"] <- parent_dest_id 
  cells_mutation[origin_cells, "parent_dest_id"] <- grand_parent_dest_id
  
  # Adding destinations to the previously resids
  cells_mutation[dest_cells_row, "type"] <- dest_to_update_type
  
  # Setting parent ID to be whatever was there before
  cells_mutation[dest_cells_row, "parent_dest_id"] <- cells_mutation[dest_cells_row,"dest_id"] 
  
  cells_mutation[dest_cells_row, "dest_id"] <- dest_to_update_id
  
  cells_mutation[dest_cells_row, "pxl_pop"] <- 0 # not resid anymore
  capacity_to_add <- dest_list[dest_list_row, "capacity"]
  cells_mutation[dest_cells_row, total_pop_col] <- capacity_to_add
  cells_mutation[dest_cells_row, reminder_pop_col] <- capacity_to_add
  cells_mutation[dest_cells_row, num_dest_col] <- 1
  
  
  # Adding population to the previously dests
  
  cells_mutation[origin_cells, "pxl_pop"] <- floor(pop_to_move / length(origin_cells)) # using floor to not to add extra
  
  extra_pop <- pop_to_move%%length(origin_cells) # get the reminder from the flow
  
  cells_mutation[origin_cells[1:extra_pop], "pxl_pop"] <- cells_mutation[
    origin_cells[1:extra_pop],"pxl_pop"] + 1  
  cells_mutation[origin_cells, total_pop_col] <- 0
  cells_mutation[origin_cells, reminder_pop_col] <- 0
  cells_mutation[dest_cells_row, num_dest_col] <- 0
  
  return(cells_mutation)
}
  
update_service_single <- function(cells_df, dest_list, dest_to_update_type){
    
    unserved_pop_col <- paste0("not_served_by_", dest_to_update_type)
    total_pop_col <- paste0("pop_total_", dest_to_update_type)
    reminder_pop_col <- paste0("pop_remaining_", dest_to_update_type)      
    dest_list_row <- which(dest_list$dest_code == dest_to_update_type)
    
    # Un-serving all those for dest_to_update_type
    resid_row_ids <- which(cells_df$type == "resid")
    cells_df[resid_row_ids, unserved_pop_col] <- cells_df[resid_row_ids, 
                                                          "pxl_pop"]
    
    # Returning all dests of this type to full cap
    dest_capacity <- dest_list[dest_list_row, "capacity"]
    dest_type_cell_rows <- which(cells_df$type == dest_to_update_type)
    cells_df[dest_type_cell_rows, total_pop_col] <- dest_capacity
    cells_df[dest_type_cell_rows, reminder_pop_col] <- dest_capacity
    
    echo(paste0("Revisiting service provided by: ", 
                dest_to_update_type, " family"))
    
    dest_type_ids <- cells_df %>% 
      filter(type == dest_to_update_type) %>%
      distinct(dest_id) 
    
    # Iterating over all destinations of type dest_to_update_type
    # dest <- dest_type_ids$dest_id[1]
    for(dest in dest_type_ids$dest_id){
      dest_cells_row <- which(cells_df$dest_id == dest)
      dest_cells_id <- cells_df[which(cells_df$dest_id == dest), "id"]
      
      # Start serving people around
      dest_geom <- cells_df %>% 
        filter(id %in% dest_cells_id) %>% 
        st_as_sf(coords = c("pxl_x", "pxl_y"), remove= F ) %>% 
        st_union() %>% 
        st_convex_hull() 

      # Getting those within 20 min access    
      cells_within_dist_row <- dest_geom %>% 
        st_buffer(0.8) %>% 
        st_intersects(st_as_sf(cells_df, coords = c("pxl_x", "pxl_y"), 
                               remove = F)) 

      cells_within_dist_id <- cells_df[unlist(cells_within_dist_row), ] %>% 
        st_as_sf(coords = c("pxl_x", "pxl_y"), remove = F) %>% 
        dplyr::mutate(dist_to_dest = st_distance(., dest_geom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist_to_dest) %>%
        dplyr::select(id)
      
      if(!nrow(cells_within_dist_id) > 0) echo("**Something is not right**")
      # Serving cells 
      system.time({
        remaining_capacity <- cells_df[dest_cells_row, reminder_pop_col][1]
        close_cells <- cells_df[which(cells_df$id %in% cells_within_dist_id$id),
                                ]
        # cell <- 1
        for(cell in 1:nrow(close_cells)) {
          # cellRow <- which(cells_df$id==id)
          cell_unserved_pop <-  close_cells[cell, unserved_pop_col]
          if (remaining_capacity > 0 & cell_unserved_pop > 0){
            pop_to_cover <- min(remaining_capacity, cell_unserved_pop) # finding population to cover
            # Updating the capapcity
            remaining_capacity <- remaining_capacity - pop_to_cover
            # Updating unserved pop
            close_cells[cell, unserved_pop_col] <- cell_unserved_pop - 
              pop_to_cover
          }
        }
        cells_df[which(
          cells_df$id %in% cells_within_dist_id$id), ] <- close_cells
        cells_df[dest_cells_row, reminder_pop_col] <- remaining_capacity
        
      })
    }
  return(cells_df)
  }

