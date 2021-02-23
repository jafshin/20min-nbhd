# TODO plot change in score over time too

#### NEIGHBOURHOOD FACILTIY LOCATION OPTIMISATION #### 
# 
# This is a code that optimizes a neighborhood destination locations to achieve 20 Min
# neighbourhood using an evolutionary algorithm
#
# @author AJ

# Packages ----------------------------------------------------------------
library(rdist)
library(dplyr)
library(ggplot2)
library(sf)
library(purrr)
library(readr)

# Functions ---------------------------------------------------------------
source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/distribute_population.R")
source("./functions/make_nbhds.R")
source("./functions/make_locations.R")


echo<- function(msg) {
  cat(paste0(as.character(Sys.time()), ' | ', msg,"\n"))  
}

# dph <- 15
optimise_nbhds <- function(dph) {
  echo(paste0("******************* DWELLING DENSITY: ", dph))
  output_sub_dir <-  paste0(output_dir, "Density_", dph, "/") # one sub-dir for each density
  print(output_sub_dir)
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE) # create if not exists
  log_file <- paste0(output_sub_dir, "output_log_D", dph, ".txt")  # log file for keeping the record
  sink(log_file, append=FALSE, split=TRUE) # sink to both console and log file
  output_file <- paste0(output_sub_dir, "output_decision_D", dph, ".csv") # decision file indicating where final destination will be located
  
  echo(paste0("Total Population, ", pop))
  echo(paste0("Dweling Density, ", dph))
  echo(paste0("***********,", "***********"))
  
  #  Destinations -----------------------------------------------------------
  init_dest <- read.csv("../inputs/destinations_v6.csv") #%>%  # list of destinations - from VPA
  #mutate(dist_in_20_min=dist_in_20_min/coverage)
  # How many of each destination needed
  init_dest <- init_dest %>% 
    mutate(num_dests=ceiling(pop/init_dest$pop_req))
  
  # Creating the neighbourhoods ---------------------------------------------
  # nbhds are considered as squares 
  nbhd_dev_area <- nbhd_d^2 * share_land_for_resid # land for development per nbhd
  nbhd_n <- ceiling(pop * 0.01 / (dph * pphh * nbhd_dev_area)) # number of nbhds
  
  nbhds <- make_nbhds(nbhd_d, nbhd_n) # nbhds with geometries
  
  study_area_d <- nbhd_d * (ceiling(sqrt(nbhd_n))+1) # Dimensions of the study area
  nbhds <- nbhds %>% # Adding Land for destinations as a var to nbhds
    mutate(land_for_dest = nbhd_d^2 * share_land_for_dest) %>% 
    mutate(remaining_land_for_dest = land_for_dest)
  
  # Creating pixels ---------------------------------------------------------
  pxl_a <- pxl_d*pxl_d
  pxl_dev_a <- pxl_a * share_land_for_resid # area in each pixel for population (this is used for applying density)
  pxl_n <- ceiling(pop * 0.01 / (dph * pphh * pxl_dev_a))

  pixls <- make_pixels_df(pxl_d, share_land_for_dest, pop, dph, 
                               pphh, study_area_d, nbhds) # creating pixles
  
  # Replace nbhd coordinates with its centroid X and Y
  nbhds_geom <- nbhds[,"NBHD_ID"]
  nbhds <- nbhds %>%
    mutate(nbhd_x=as.numeric(sf::st_coordinates(st_centroid(.))[,1]),
           nbhd_y=as.numeric(sf::st_coordinates(st_centroid(.))[,2]))%>% 
    st_drop_geometry()  
  
  # Replace cell coordinates with its centroid X and Y
  pixls_geom <- pixls[,"ID"]
  pixls <- pixls %>% 
    mutate(pxl_x=as.numeric(sf::st_coordinates(st_centroid(.))[,1]),
           pxl_y=as.numeric(sf::st_coordinates(st_centroid(.))[,2]))%>% 
    st_drop_geometry()
  
  # Add distance to nbhd centre to cells
  pixls <- pixls %>% 
    left_join(nbhds[,c("NBHD_ID","nbhd_x","nbhd_y")], by = c("NBHD_ID")) %>% 
    mutate(dist2ltc = sqrt((pxl_x-nbhd_x)^2 + (pxl_y-nbhd_y)^2 )) %>% 
    mutate(nbhdQ=paste0(NBHD_ID,"_1")) %>% 
    mutate(nbhdQ=ifelse(test = pxl_x>nbhd_x & pxl_y<nbhd_y , yes = paste0(NBHD_ID,"_2"), no=nbhdQ)) %>% 
    mutate(nbhdQ=ifelse(test = pxl_x<nbhd_x & pxl_y<nbhd_y , yes = paste0(NBHD_ID,"_3"), no=nbhdQ)) %>% 
    mutate(nbhdQ=ifelse(test = pxl_x<nbhd_x & pxl_y>nbhd_y , yes = paste0(NBHD_ID,"_4"), no=nbhdQ))  
  
  # Write cells to spatialite file
  pixls %>% 
    left_join(pixls_geom, by = "ID") %>%
    st_write("init_pixls225.sqlite", delete_layer = T)
  
  if(popDiversity){
    # high or medium density within walkable catchment of ltc
    hdc_pct <- 0.5 # 50% more density in high density pixels
    avg_nbhd_pop <- ceiling(pop/nbhd_n)
    remaining_population <- pop 
    for (nb in nbhds$NBHD_ID) {
      nbhd_pop <- min(avg_nbhd_pop, remaining_population)
      remaining_population <- remaining_population - nbhd_pop
      # Now we want to spread this nbhd_pop
      my_pixls <- pixls %>% 
        filter(NBHD_ID == nb) %>% 
        mutate(position=ifelse(dist2ltc<0.4, "close","away")) %>% 
        
      
      pxl_positions <- my_pixls$position %>% table()  
      pxl_pop_ldc <- nbhd_pop / (pxl_positions["close"]*(1+hdc_pct)+pxl_positions["away"])
      pxl_pop_hdc <- pxl_pop_ldc*(1+hdc_pct)
      nbhd_rem_pop <- nbhd_pop
      
      for (i in 1:nrow(my_pixls)){
        px <- my_pixls$ID[i]
        if(my_pixls$position[i]=="close"){
          #print("close")
          pixls[px, "pop"] <- min(pxl_pop_hdc, nbhd_rem_pop)
          nbhd_rem_pop <- nbhd_rem_pop - pixls$pop[px]
        }else{
          #print("away")
          pixls[px, "pop"] <- min(pxl_pop_ldc, nbhd_rem_pop)
          nbhd_rem_pop <- nbhd_rem_pop - pixls$pop[px]
        }
      }
    }
  }else{
    avg_px_pop <- ceiling (pop / pxl_n) # Assuming homogeneous population distribution
    remaining_population <- pop 
    for (nb in nbhds$NBHD_ID) { # populating the pixles
      my_pixls <- which(pixls$NBHD_ID == nb)
      for (px in my_pixls){
        pixls[px, "pop"] <- min(avg_px_pop, remaining_population)
        remaining_population <- remaining_population - pixls$pop[px]

      }
    }
  }
  
  ## 
  pixls <- pixls %>% filter(pop > 0) %>% # Just keeping the pixels with pop
    mutate(type="resid")

  if (consider_categories) {
    # finding near the local town centres (400m)
    pixls <- pixls %>%  
      mutate(position=ifelse(dist2ltc<0.4, yes = "nltc", no = "etc")) %>% 
      mutate(position=ifelse(dist2ltc<0.01, yes = "ltc", no = position)) 
  }else{
    pixls <- pixls %>%  
      mutate(position='all')  
  }
  
  # Joining nbhds and pixels ------------------------------------------------
  nbhds <- pixls %>%
    group_by(NBHD_ID) %>%
    summarise(pop = sum(pop)) %>%
    left_join(nbhds, by="NBHD_ID")  %>% 
    filter(pop>0)
  #plot(nbhds)
  
  # Creating the locations -------------------------------------------------
  ## EACH PXL WILL BE A DECISION POINT SO COMMENTING THIS OUT
  # init_loc <- make_locations(nbhds, study_area_d)
  
  # Sorting init dest based on pop_req*land_req
  # meaning starting from those big and high pop destinations
  init_dest <- init_dest %>%
    arrange(order,desc(land_req),desc(pop_req)) 
  
  for(dest in init_dest$dest_code){
    # Adding destinations to the pixels
    pixls <- pixls %>% 
      mutate(!!paste0("not_served_by_",dest):=pop) %>% 
      mutate(!!paste0("num_dest_", dest):=0) %>% 
      mutate(!!paste0("pop_total_", dest):=0) %>% 
      mutate(!!paste0("pop_remaining_", dest):=0)
  }
  
  # FIRST ROUND
  iter_nbhds <- nbhds # assigning iter specific variable nbhds
  #iter_loc <- init_loc %>%  # assigning iter specific variable locations
  #  st_drop_geometry()
  iter_dest <- init_dest # assigning iter specific variable destinations
  iter_pixls <- pixls # assigning iter specific variable pixels

# Initial dest distribution -----------------------------------------------

  land_flag <- FALSE # a flag for whether there are land avail or not
  No_Answer_flag <- FALSE # a flag for when no answer will be found
  
  # Loop over all destinations
  iter_dest_row <- 1
  for(iter_dest_row in 1:nrow(iter_dest)){
    unavail_decisions <- 0 # TODO check what is this
    iter_dest_code <- iter_dest$dest_code[iter_dest_row] # getting the dest type
    iter_dest_position <- iter_dest$position[iter_dest_row] # getting the dest type
    
    echo(paste("destination:",iter_dest_code,"; dwelling denisty:",dph,sep=" "))
    
    
    # FIRST destination OF TYPE iter_dest_row is also going through the evolutionary process
    # Repeating the process until all neighborhoods are served
    error_counter <- 0
    remaining_num_dests <- iter_dest$num_dests[iter_dest_row]
    while((get_unsrvd_pop(iter_pixls, iter_dest_code) > pop*(1-iter_dest$coverage[iter_dest_row])) & (remaining_num_dests > 0)){ # loop until all are served AND we have destinations to use
      # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS

      system.time(destCellsID <- findDestinationCells(iter_pixls,iter_dest,iter_dest_row,
                                                    iter_nbhds,iter_dest_code,pxl_a)
      )
      destCells <- which(iter_pixls$ID%in%destCellsID)
      
      if(length(destCellsID)==0){ # ???
        echo("No Answer")
        
        No_Answer_flag <- TRUE
        #unavail_decisions <- c(unavail_decisions, new_dest_id)
        break();
      }
      
      # Add destination to the the cell
      iter_pixls$type[destCells] <- iter_dest_code
      # Adding new capacity
      pop_remainder_col <- paste0("pop_remaining_",iter_dest_code)
      pop_total_col <- paste0("pop_total_",iter_dest_code)
      iter_pixls[destCells,pop_total_col] <- iter_dest[iter_dest_row,"capacity"]
      iter_pixls[destCells,pop_remainder_col] <- iter_dest[iter_dest_row,"capacity"]
      
      remaining_num_dests <- remaining_num_dests - 1
      
      # Start serving people around
      destGeom <- iter_pixls %>% 
        filter(ID%in%destCells) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # destGeom%>% 
      #    st_write("selectedPxls_CH.sqlite", delete_layer=T)
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- destGeom %>% 
        st_buffer(0.8) %>% 
        st_intersects(iter_pixls %>% st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)) 
        
      cellsWithinDistID <- iter_pixls[unlist(cellsWithinDistRow),] %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist2Dest) %>%
        dplyr::select(ID)
        # plot()
        # st_sf() %>% 
        # st_write("cellsWithin20Min222.sqlite", delete_layer=T)
  
      if(!nrow(cellsWithinDistID) > 0) message("**Something is not right**")
      # Serving cells 
      
      # id = cellsWithinDistID$ID[1]
      for (id in cellsWithinDistID$ID) {
        # print(id)
        cellRow <-  which(iter_pixls$ID==id)
        cell_unsrved_pop <-  get_unsrvd_pop(iter_pixls[cellRow,],
                                            iter_dest_code)

        if (iter_pixls[destCells,pop_remainder_col][1] > 0 & cell_unsrved_pop > 0){
          # print("here")
          pop2cover <- min(iter_pixls[destCells,pop_remainder_col][1] , 
                           cell_unsrved_pop) # finding population to cover
          # Updating the capapcity
          iter_pixls[destCells,pop_remainder_col] <- iter_pixls[destCells,pop_remainder_col][1]-pop2cover
          # Updating unserved pop
          iter_pixls[cellRow, paste("not_served_by_",
                                    iter_dest_code, sep = "")] <- cell_unsrved_pop - pop2cover
          
        }
      } 

      
      # Add a check here for if there are unserved pop in the close pxls
      # for (i in 1:nrow(close_pxls)) {
      #   temp_pixl_id <- st_drop_geometry(close_pxls[i,"ID"]) %>% 
      #     as.numeric()
      #   temp_pixl_row <- which(iter_pixls$ID==temp_pixl_id)
      #   
      #   # TODO there seems to be an issue here, check if it is getting the unsrved pop correctly
      #   pxls_unsrved_pop <-  get_unsrvd_pop(iter_pixls[temp_pixl_row,],
      #                                       iter_dest_code)
      #   pop_remainder_col <- paste0("pop_remaining_",iter_dest_code)
      #   if (iter_loc[new_dest_loc_id,pop_remainder_col] > 0 & pxls_unsrved_pop > 0){
      #     # If distance is less than 20 min given the mode speed and nb has unserved pop
          # pop2cover <- min(iter_loc[new_dest_loc_id,pop_remainder_col], 
          #                  pxls_unsrved_pop) # finding population to cover
          # 
          # updating not served pop in deci and pixles
          # iter_loc[new_dest_loc_id,pop_remainder_col] <- iter_loc[new_dest_loc_id,pop_remainder_col] - pop2cover
          # iter_pixls[temp_pixl_row, paste("not_served_by_", 
          #                                 iter_dest_code, sep = "")] <- pxls_unsrved_pop - pop2cover
          # adding the destinations to iter deci df
    }
    iter_pixls %>% 
      st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
      st_write("iter_pixls_Feb23_1230.sqlite", layer = dph, delete_layer=T)
    
  }
    
  # TODO We keep running out of space, check what is happenning with nbhds!? 
    # TODO Redistribute the population
  iter_pixls %>% 
    mutate(isResid=ifelse(type=="resid", yes=1,no=0)) %>% 
    group_by(NBHD_ID) %>% summarise(count=n(),
                                    residCount=sum(isResid),
                                    Pct=100*(count-residCount)/count) 
    
    # 
    
    
    
  # Evolutionary optimisation -----------------------------------------------
  score <- 200 # Assuming the worst score when no one is served (100%) + no capacity used (100% free capacity)
  #decision <- init_loc 
  iter <- 1
  convergenceCounter <- 0
  while(iter < iters_max + 1){
    echo(paste("############## iteration number",iter, sep = ":"))
    
    # A set of temp DFs for iterations
    iter_nbhds <- nbhds # assigning iter specific variable nbhds
    iter_loc <- init_loc %>%  # assigning iter specific variable locations
      st_drop_geometry()
    iter_dest <- init_dest # assigning iter specific variable destinations
    iter_pixls <- init_pixls # assigning iter specific variable pixels
    
    land_flag <- FALSE # a flag for whether there are land avail or not
    No_Answer_flag <- FALSE # a flag for when no answer will be found
    
    # Loop over all destinations
    iter_dest_row <- 1
    for(iter_dest_row in 1:nrow(iter_dest)){
      unavail_decisions <- 0 # TODO check what is this
      iter_dest_code <- iter_dest$dest_code[iter_dest_row] # getting the dest type
      iter_dest_position <- iter_dest$position[iter_dest_row] # getting the dest type
      
      echo(paste("destination:",iter_dest_code,"; iteration:",iter, 
                 "; dwelling denisty:",dph,sep = " "))
      
      
      # FIRST destination OF TYPE iter_dest_row is also going through the evolutionary process
      # Repeating the process until all neighborhoods are served
      error_counter <- 0
      remaining_num_dests <- iter_dest$num_dests[iter_dest_row]
      while((get_unsrvd_pop(iter_pixls, iter_dest_code) > pop*(1-iter_dest$coverage[iter_dest_row])) & (remaining_num_dests > 0)){ # loop until all are served AND we have destinations to use
        # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS
        feasible_locs <- find_feasible_locs(iter_loc, iter_pixls,
                                            iter_dest, iter_dest_row,
                                            iter_dest_position, 
                                            consider_categories,
                                            iter_dest_code)
        feasible_locs <- feasible_locs %>%
          filter(!(loc_id %in% unavail_decisions)) ## REMEBMER TO CHANGE THIS
        
        if(nrow(feasible_locs)==0){ # ???
          echo("No Answer")
          
          No_Answer_flag <- TRUE
          #unavail_decisions <- c(unavail_decisions, new_dest_id)
          break();
        }
        
        # selecting one from max catchments by random
        new_dest_loc_id <- feasible_locs %>%
          filter(catchment_potential == max(catchment_potential)) %>%
          select(loc_id) %>% 
          sample_n(size = 1) %>% 
          as.integer()
        
        # mutation ------------------------------------------------------------
        some_rnd <- runif(n =1,0,1)
        if(some_rnd < mutation_p & length(feasible_locs$loc_id)>1) new_dest_loc_id <- sample(feasible_locs$loc_id, 1)
        echo(paste0("new_dest_id: ", new_dest_loc_id))
        
        # Opening a new destination -------------------------------------------
        
        # Finding neighborhoods adjacent to the location for land
        loc_nbhds<- find_land_contributors(iter_loc, new_dest_loc_id,
                                           iter_nbhds)
        
        # land to occupy
        land_to_occupy <- iter_dest$land_req[which(iter_dest$dest_code == iter_dest_code)]
        
        if(!check_total_land(land_to_occupy,loc_nbhds,iter_nbhds)){
          error_counter <- error_counter + 1
          unavail_decisions <- c(unavail_decisions, new_dest_loc_id)
          if(error_counter > 100){
            echo("not enough space, SERIOUSLY! V2")
            
            land_flag <- TRUE
            stop();
          }
        }else{
          # Adding the destination to decision and location DFs
          iter_loc <- add_destination_to_location(iter_loc, new_dest_loc_id, 
                                                  iter_dest_code, iter_dest)
          # Occupying the land
          iter_nbhds <- occupy_land(loc_nbhds, iter_nbhds, land_to_occupy)
          # Updating number of remaining destinations
          remaining_num_dests <- remaining_num_dests - 1
          
          # SERVING THE NEIGHBOURS  -------------------------------------------
          # finding those closest to this
          close_pxls_id <- iter_loc[new_dest_loc_id,] %>% 
            st_as_sf(coords=c("x","y")) %>% 
            st_buffer(iter_dest[iter_dest_row,"dist_in_20_min"]) %>% 
            st_intersects(st_centroid(iter_pixls),sparse = F)
          
          close_pxls <- iter_pixls[close_pxls_id,] %>% 
            st_centroid() %>% 
            mutate(dist_to_dest = st_distance(.,
                                              iter_loc[new_dest_loc_id,] %>% 
                                                st_as_sf(coords=c("x","y")))) %>% 
            arrange(dist_to_dest)
          
          
          #iter_loc[new_dest_loc_id,] %>%  st_as_sf(coords=c("x","y")) %>% 
          #  st_write("iter_loc_test.sqlite")
          
          #st_write(iter_pixls[close_pxls_id,] ,"close_pxls_sq_Test.sqlite")
          # Find all those in range and not fully served
          if(!nrow(close_pxls) > 0) message("**Something is not right**")
          
          # Add a check here for if there are unserved pop in the close pxls
          for (i in 1:nrow(close_pxls)) {
            temp_pixl_id <- st_drop_geometry(close_pxls[i,"ID"]) %>% 
              as.numeric()
            temp_pixl_row <- which(iter_pixls$ID==temp_pixl_id)
            
            # TODO there seems to be an issue here, check if it is getting the unsrved pop correctly
            pxls_unsrved_pop <-  get_unsrvd_pop(iter_pixls[temp_pixl_row,],
                                                iter_dest_code)
            pop_remainder_col <- paste0("pop_remaining_",iter_dest_code)
            if (iter_loc[new_dest_loc_id,pop_remainder_col] > 0 & pxls_unsrved_pop > 0){
              # If distance is less than 20 min given the mode speed and nb has unserved pop
              pop2cover <- min(iter_loc[new_dest_loc_id,pop_remainder_col], 
                               pxls_unsrved_pop) # finding population to cover
              
              # updating not served pop in deci and pixles
              iter_loc[new_dest_loc_id,pop_remainder_col] <- iter_loc[new_dest_loc_id,pop_remainder_col] - pop2cover
              iter_pixls[temp_pixl_row, paste("not_served_by_", 
                                              iter_dest_code, sep = "")] <- pxls_unsrved_pop - pop2cover
              # adding the destinations to iter deci df
            }
          }
        }
      }
      if(No_Answer_flag){
        break;
      }
      iter_dest[iter_dest_row, "pop_unsrvd"] <- get_unsrvd_pop(iter_pixls, iter_dest_code)
    }
    
    # Scoring and best solution selection -------------------------------------
    if(land_flag){
      echo("I AM HERE")
      iter_score <- nrow(init_dest) * (200)
    }else if(No_Answer_flag){
      echo("I AM HERE 2")
      iter_score <- nrow(init_dest) * (200)
    }
    else{
      tp <- iter_loc %>% 
        select(starts_with("pop_total")) %>% 
        sum()
      
      tpr <- iter_loc %>% 
        select(starts_with("pop_remaining")) %>% 
        sum()
      
      deci_score <- ifelse(tp>0,100*tpr/tp,0)
      
      iter_score <- 100*(sum(iter_dest$pop_unsrvd, 
                             na.rm = T)/(pop*nrow(iter_dest[!is.na(iter_dest$pop_unsrvd),]))) +
        deci_score
      if(is.na(iter_score)) iter_score=200
    }
    
    echo(paste("Iteration,",iter, sep = ","))
    echo(paste("Iteration_Score,",iter_score, sep = ","))
    echo(paste("Iteration_result,",iter_score, sep = ","))
    
    # Checking if it is a new best result
    if(iter_score < score){
      convergenceCounter <- 0
      score <- iter_score
      decision <- iter_loc %>% 
        st_as_sf(coords=c("x","y"))
      neighbourhood_output <- iter_nbhds 
      pixels_output <- iter_pixls
      echo(paste("Iteration_result,","NEW_BEST_SCORE", sep = ","))
      echo(paste("new_Best_Score,",score, sep = ","))
      #echo(paste0("new_Best_Score, ",score))
    }else if(land_flag){
      echo(paste("Iteration_result,","NO_SPACE", sep = ","))
    }else if(No_Answer_flag){
      echo(paste("Iteration_result,","NO_SPACE", sep = ","))
    }else{
      echo("iteration score not changed")
      
      convergenceCounter <- convergenceCounter + 1
      
      echo(paste0("convergence counter: ", convergenceCounter))
      echo(paste("Iteration_result,","NORMAL", sep = ","))
    }
    echo("******, ******")
    if(convergenceCounter>convergenceIterations){
      echo(paste0("Skipping the rest, model seems to be converged at iter ", iter))
      
      break
    } 
    iter <- iter + 1
  }
  # writing outputs 
  echo(paste("Final_best_score", score, sep = ","))
  total_score_df$score[which(total_score_df$density == dph)] <- score
  
  # decision writing
  decision %>% 
    mutate(density = dph) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="decision",delete_layer=T)
  
  pixels_output %>% 
    mutate(density = dph) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="pixels",delete_layer=T)
  
  neighbourhood_output %>% 
    mutate(density = dph) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="nbhd",delete_layer=T)
  
  sink()
  
}

# Setting initial parameters ----------------------------------------------
pphh <- 2.6 # person per household
pop <- 60000 # total population
mutation_p <- 0.20 # mutate rate for optimization
iters_max <- 50 # max number of iterations
convergenceIterations <- 5
share_land_for_dest <- 0.35 # share of land for dest
share_land_for_resid <- 0.7 # share of land for residential
pxl_d <- 0.025 # pixel diameter
nbhd_d <- 1.6 # neighbourhood diameter
consider_categories <- F 
popDiversity <- T
densities <- seq(from = 35, to = 45, by = 10) # dwelling per hectare
# Setting up folders ------------------------------------------------------
output_dir <- "../outputs/Exp3_withDiversity_Feb1_1734/" # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

output_deci_dir <- paste0(output_dir,"decisions") # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_deci_dir), dir.create(output_deci_dir), FALSE)

total_scores_file <- paste(output_dir, "score_summary.csv", sep = "")
total_score_df <- data.frame(density = densities)



# iterating over densities ------------------------------------------------
#dph <- densities[1] # dwelling per hectare
#for (dph in densities){
#  optimise_nbhds(dph)
#  }

# lapply(densities, optimise_nbhds)  

#library(doParallel)  
#no_cores <- min(( detectCores() - 1 ),  length(densities))
#cl <- makeCluster(no_cores, type="FORK")  
#registerDoParallel(cl)  
#foreach(i=densities) %dopar% optimise_nbhds(i)
#parLapply(cl, densities, optimise_nbhds)  
#stopCluster(cl)  
