#### NEIGHBOURHOOD FACILTIY LOCATION OPTIMISATION #### 
# This is a code that optimizes a neighborhood destination locations to achieve 20 Min
# neighbourhood using an evolutionary algorithm
#
# @author AJ

# Packages ----------------------------------------------------------------
library(tidyverse)
library(rdist)
library(sf)

# Functions ---------------------------------------------------------------
source("./models/functions/land_updating.R")
source("./models/functions/update_service.R")
source("./models/functions/scoring.R")
source("./models/functions/make-layout.R")

echo<- function(msg) {
  cat(paste0(as.character(Sys.time()), ' | ', msg,"\n"))  
} 

# dph <- 35
optimise_nbhds <- function(dph) {
  # Step 0: Setting up inputs and structure ---------------------------------
  echo(paste0("******************* DWELLING DENSITY: ", dph))
  output_sub_dir <-  paste0(output_dir, "Density_", dph, "/") # one sub-dir for each density
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE) # create if not exists
  log_file <- paste0(output_sub_dir, "output_log_D", dph, ".txt")  # log file for keeping the record
  sink(log_file, append = FALSE, split = TRUE) # sink to both console and log file
  output_file <- paste0(output_sub_dir, "output_decision_D", dph, ".csv") # decision file indicating where final destination will be located
  echo(paste0("Total Population, ", total_population))
  echo(paste0("Dweling Density, ", dph))
  echo(paste0("***********,", "***********"))
  
  # Step 1 Creating decision grid -------------------------------------------
  
  echo("Step 1 starts - Creating decision grid")
  
  cells <- suppressWarnings(make_layout(nbhd_d, share_land_for_resid, 
                                        total_population, dph, pphh, pxl_d))
  pxl_a <- pxl_d*pxl_d
  # Adding area in terms of pixel to list
  dests <- dests %>% 
    rowwise() %>% 
    mutate(area_in_cells = ifelse(test = dest_code == "cc",
                                  yes  = 2, 
                                  no   = max(1, round(land_req / pxl_a)))) %>%
    ungroup()
  
  # How many of each destination needed
  dests <- dests %>% 
    mutate(num_dests = ceiling(total_population / pop_req))
  
  # Sorting init dest based on pop_req*land_req
  # meaning starting from those big and high pop destinations
  dests <- dests %>%
    arrange(order, desc(land_req), desc(pop_req)) 
  
  for(dest in dests$dest_code){
    # Adding destinations to the pixels
    cells <- cells %>% 
      mutate(!!paste0("not_served_by_", dest) := pxl_pop) %>% 
      mutate(!!paste0("num_dest_",      dest) := 0) %>% 
      mutate(!!paste0("pop_total_",     dest) := 0) %>% 
      mutate(!!paste0("pop_remaining_", dest) := 0)
  }
  
  echo("Finished Creating decision grid (Step 1)")
  
  # Step 2: Initial layout  -------------------------------------------------
  
  echo("Starting Creating initial layout (Step 2)")
  dest_list <- dests # assigning iter specific variable destinations
  cells_initial <- cells # assigning iter specific variable pixels
  No_Answer_flag <- FALSE # a flag for when no answer will be found
  clstsId <- 1
  
  # Loop over all destinations
  echo("Looping over all destinations and allocating initial locations")
  # dest_row=1
  for(dest_row in 1:nrow(dest_list)){
    dest_code <- dest_list$dest_code[dest_row] # getting the dest type
    dest_lvl <- dest_list$lvl[dest_row] # getting the dest level
    cells_to_occupy <- dest_list$area_in_cells[dest_row]
    dest_r <- max((pxl_d / 2) + 0.001, sqrt(dest_list$land_req[dest_row] / pi))
    iter_dest_position <- dest_list$position[dest_row] # getting the dest type
    
    # FIRST destination OF TYPE dest_row is also going through the evolutionary process
    echo(paste("destination:",dest_code,"; dwelling denisty:",dph,sep=" "))
    
    error_counter <- 0
    remaining_num_dests <- dest_list$num_dests[dest_row]
    # Repeating the process until all neighborhoods are served
    while((remaining_num_dests > 0) & 
          (get_unsrvd_pop(cells_initial,dest_code) > total_population *
           (1 - dest_list$coverage[dest_row]))){ # loop until we have destinations to use
      # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS
      
      system.time(
        dest_cells_id <- findDestinationCells(cells_initial, dest_list, 
                                              cells_to_occupy, 
                                              dest_code, pxl_a, 
                                              dest_lvl,dest_r))
      
      dest_cells_row <- which(cells_initial$id %in% dest_cells_id)
      
      if(length(dest_cells_id) == 0){ 
        echo("No Answer")
        No_Answer_flag <- TRUE
        break();
      }
      
      # Add destination to the the cell
      
      cells_initial$type[dest_cells_row] <- dest_code
      remaining_num_dests <- remaining_num_dests - 1
      # Setting parent ID to be whatever was there before
      cells_initial[dest_cells_row, "parent_dest_id"] <- cells_initial$dest_id[dest_cells_row] 
      # Setting the current dest ID
      cells_initial$dest_id[dest_cells_row] <- paste0(dest_code,"_",
                                                      dest_list$num_dests[dest_row] - remaining_num_dests)
      
      # Adding new capacity
      reminderPopCol <- paste0("pop_remaining_",dest_code)
      totalPopCol <- paste0("pop_total_",dest_code)
      numDestCol <- paste0("num_dest_",dest_code)
      unserved_pop_col <- paste0("not_served_by_",dest_code)
      
      cells_initial[dest_cells_row,totalPopCol] <- dest_list[dest_row,"capacity"]
      cells_initial[dest_cells_row,reminderPopCol] <- dest_list[dest_row,"capacity"]
      cells_initial[dest_cells_row,numDestCol] <- 1
      # Start serving people around
      dest_geom <- cells_initial %>% 
        filter(id %in% dest_cells_id) %>% 
        st_as_sf(coords=c("pxl_x", "pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- dest_geom %>% 
        st_buffer(0.8) %>% 
        st_intersects(cells_initial %>% st_as_sf(coords=c("pxl_x", "pxl_y"), 
                                                 remove=F)) 
      
      cells_within_dist_id <- cells_initial[unlist(cellsWithinDistRow), ] %>% 
        st_as_sf(coords = c("pxl_x", "pxl_y"), remove=F) %>% 
        dplyr::mutate(dist_to_dest = st_distance(., dest_geom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist_to_dest) %>%
        dplyr::select(id)
      
      if(!nrow(cells_within_dist_id) > 0) message("**Something is not right**")
      if(!nrow(cells_within_dist_id) > 0) stop()
      # Serving cells 
      system.time({
        remaining_capacity <- cells_initial[dest_cells_row, reminderPopCol][1]
        close_cells <- cells_initial[which(
          cells_initial$id %in% cells_within_dist_id$id), ]
        cell <- 1
        for(cell in 1:nrow(close_cells)) {
          # cellRow <- which(cells_initial$id==id)
          cell_unsrved_pop <-  close_cells[cell, unserved_pop_col]
          if (remaining_capacity > 0 & cell_unsrved_pop > 0){
            pop2cover <- min(remaining_capacity, cell_unsrved_pop) # finding population to cover
            # Updating the capapcity
            remaining_capacity <- remaining_capacity-pop2cover
            # Updating unserved pop
            close_cells[cell,unserved_pop_col] <- cell_unsrved_pop - pop2cover
          }
        }
        cells_initial[which(
          cells_initial$id %in% cells_within_dist_id$id), ] <- close_cells
        cells_initial[dest_cells_row, reminderPopCol] <- remaining_capacity
      })
    }
  }
  cells_initial %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(output_sqlite, layer = paste0(dph,"_step2"), delete_layer=T)
  
  # In this step, population from pixels occupied with dests will be removed
  # And will be uniformly distributed in other locations
  # We will use random samples in order to redistribute
  echo("Finished Creating initial layout (Step 2)")
  
  # Step 3: Updating demand distribution ------------------------------------
  
  echo("Starting Updating demand distribution (step3)")
  
  cells_initial_relocated <-move_homeless(cells_initial) 
  
  system.time({
    cells_initial_updated <- update_service_all(cells_initial_relocated, 
                                                dest_list)
  })
  
  cells_initial_updated %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(output_sqlite, layer = paste0(dph,"_step3"), delete_layer=T)
  
  # cells_initial_updated  <- st_read(output_sqlite, layer = paste0(dph,"_step3"))
  
  echo("Finished Updating demand distribution (step3)")
  
  # Step 4: Scoring ---------------------------------------------------------
  
  echo("Starting Scoring (step4)")
  
  if(No_Answer_flag){  
    score_temp <- nrow(dests) * 1000
  }else{
    score_temp <- get_score(cells_initial_updated, dest_list, total_population) 
    cells_best <- cells_initial_updated
    score_best <- score_temp
  }
  echo(paste0("Initial layout score: ", score_best))
  
  echo("Finished Scoring (step4)")
  
  # Step5: Mutating Supply --------------------------------------------------
  echo("Starting Mutating Supply (step 5)")
  
  ### Here is where the While loop should start
  cells_temp <- cells_initial_updated
  iter <- 1
  convergence_counter <- 0
  while(iter < iters_max + 1){
    echo(paste0("Starting iteration: ", iter))
    
    # Select some dests to mutate:
    dests_to_update <- cells_temp %>% 
      filter(dest_id != "NA") %>%  
      distinct(dest_id, type, parent_dest_id) %>% 
      ungroup() %>% 
      slice_sample(prop = mutation_p) %>% 
      as.data.frame()
    
    if(nrow(dests_to_update) == 0) echo("Mutation skipped")
    if(nrow(dests_to_update) != 0){
      # Changing the locations with free cells
      cells_mutation <- cells_temp
      # i=12
      for (i in 1:nrow(dests_to_update)){
        dest_to_update_id <- dests_to_update[i,"dest_id"]
        echo(paste0("mutating the location of destination ", dest_to_update_id))
        dest_to_update_type <- dests_to_update[i,"type"]
        cells_to_move <- unlist(dest_list[which(
          dest_list[,"dest_code"] == dest_to_update_type), "area_in_cells"])
        dest_to_update_lvl <- unlist(dest_list[which(
          dest_list[,"dest_code"] == dest_to_update_type),"lvl"])
        dest_to_update_position <- unlist(dest_list[which(
          dest_list[,"dest_code"] == dest_to_update_type),"position"])
        dest_to_update_radius <- max(( pxl_d / 2 ) + 0.001 , unlist(sqrt(
          dest_list[which(dest_list[, "dest_code"] == dest_to_update_type), 
                    "land_req"] / pi)))
        
        origin_cells <- which(cells_mutation$dest_id == dest_to_update_id)
        
        # Find where to move
        dest_cells_id <- findDestinationCells(cells_mutation, 
                                              dest_list,
                                              cells_to_move,
                                              dest_to_update_type,
                                              pxl_a,
                                              dest_to_update_lvl,
                                              dest_to_update_radius)
        
        if(length(dest_cells_id) == 0){
          echo("No Answer")
          No_Answer_flag <- TRUE
          break();}
        
        dest_cells_row <- which(cells_mutation$id %in% dest_cells_id)
        
        # Moving destination from origin_cells to dest_cells_row
        cells_mutation <- move_dest_single(cells_mutation, 
                                           origin_cells, 
                                           dest_cells_row,
                                           dest_list,
                                           dest_to_update_type,
                                           dest_to_update_position,
                                           dest_to_update_id)
        
        # Check if any other destinations had this destination as the parent
        orphaned_cells <- cells_mutation %>% 
          filter(parent_dest_id == dest_to_update_id)
        
        # if yes, then all child destinations will be added to the mutation list
        if(nrow(orphaned_cells) > 0){
          orphaned_cells_formatted <- orphaned_cells %>% 
            distinct(dest_id, type, parent_dest_id) %>% 
            ungroup() %>% 
            as.data.frame()
          
          dests_to_update <- rbind(dests_to_update, orphaned_cells_formatted)
          
          echo(paste0("Found an orphaned cell, adding destination, ", 
                      orphaned_cells_formatted$dest_id, " to the mutation list"))
        } 
        
        # Updating the serving part ---------------------------------------
        
        # Updating service for all destinations of the same type 
        cells_mutation <- update_service_single(cells_mutation,
                                                dest_list,
                                                dest_to_update_type)
        
        # Evaluating the single destination mutation
        score_post_mutation <- get_score(cells_mutation, dest_list, total_population) 
        if (score_post_mutation < score_temp) {
          #   # Keeping the mutation result
          echo(paste0("Found a good mutation, destintation id= ", 
                      dest_to_update_id,
                      ", delta= ", (score_post_mutation - score_temp)))
          
          cells_temp <- cells_mutation
          score_temp <- score_post_mutation
          
        }else{
          echo(paste0("Not a good mutation, destintation id= ", 
                      dest_to_update_id,
                      ", delta= ",
                      (score_post_mutation - score_temp)))
          
          cells_mutation <- cells_temp
        }
      }
      # Updating the serving part ---------------------------------------
      
      # Updating service for all destinations 
      cells_mutation <- update_service_all(cells_mutation, dest_list)
      
      # Step7 Evaluation --------------------------------------------------------
      # Check the Score, If better keep, if not discard
      if(No_Answer_flag){  
        score_temp <- nrow(dests) * 1000
      }else{
        score_temp <- get_score(cells_temp, dest_list, total_population) 
      }
      if(score_temp < score_best){
        echo(paste0("Old best score: ", score_best))
        echo(paste0("New best score: ", score_temp))
        cells_best <- cells_mutation
        score_best <- score_temp
      }else{
        echo("best score not changed")
        convergence_counter <- convergence_counter + 1
        echo(paste0("convergence counter: ", convergence_counter))
        echo(paste("Iteration_result,", "NORMAL", sep = ","))
      }
      echo("******, ******")
      if(convergence_counter > convergence_iterations){
        echo(paste0("Skipping the rest, model seems to be converged at iter ", 
                    iter))
        break
      } 
      echo(paste0("Iteration: ", iter, "Finished, DPH: ", dph))
      iter <- iter + 1
      
    } # end of if 
  } # End of While loop
  
  # writing the final outputs 
  echo(paste0("Final best score: ", score_best))
  # decision writing
  cells_temp %>% 
    mutate(density = dph) %>% 
    st_as_sf(coords = c("pxl_x","pxl_y"), remove = F) %>% 
    st_write(output_sqlite, remove = F, layer=paste0(dph, "_final"), 
             delete_layer=T)
  sink()
}

# Setting initial parameters ----------------------------------------------
pphh <- 2.6 # person per household
total_population <- 60000 # total population
mutation_p <- 0.10 # mutate rate for optimization
iters_max <- 50 # max number of iterations
convergence_iterations <- 5
share_land_for_dest <- 0.15 # share of land for dest
share_land_for_resid <- 0.85 # share of land for residential
pxl_d <- 0.025 # pixel diameter
nbhd_d <- 1.6 # neighbourhood diameter
densities <- seq(from = 15, to = 45, by = 5) # dwelling per hectare
runs <- 10
experiment_time <- format(Sys.time(),"%d%b%y_%H%M")
test_run <- F # set true if you want a small experimental run 

# Destinations
dests <- read.csv("./inputs/destinations_v8.csv")

# Setting up folders ------------------------------------------------------

if(test_run){
  densities <- seq(from = 30, to = 35, by = 5)
  runs <- 1
  iters_max <- 1
}

#dph <- 15
for (dph in densities){
  #run <- 1
  for(run in 1:runs ){
    dir.create("./outputs/", showWarnings = FALSE)
    
    echo(paste0("Starting dph ", dph, " run ", run))
    
    output_dir <- paste0("./outputs/Exp3_",experiment_time)
    ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)
    
    output_dir <- paste0(output_dir,"/Dph",dph,"_Run",run,"/")
    ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)
    
    output_deci_dir <- paste0(output_dir,"decisions") # CHANGE THIS FOR DIFFERENT RUNS
    ifelse(!dir.exists(output_deci_dir), dir.create(output_deci_dir), FALSE)
    
    total_scores_file <- paste0(output_dir, "score_summary_Run", run, ".csv")
    total_score_df <- data.frame(density = densities)
    
    output_sqlite <- paste0(output_deci_dir, "/cells_dph", dph, ".sqlite")
    
    optimise_nbhds(dph)
  }
}
