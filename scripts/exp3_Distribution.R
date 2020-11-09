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
library(readr)

# Functions ---------------------------------------------------------------
source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/distribute_population.R")
source("./functions/make_nbhds.R")
source("./functions/make_locations.R")

# Setting initial parameters ----------------------------------------------
pphh <- 2.6 # person per household
pop <- 60000 # total population
mutation_p <- 0.20 # mutate rate for optimization
iters_max <- 25 # max number of iterations
convergenceIterations <- 5
share_land_for_dest <- 0.35 # share of land for dest
share_land_for_resid <- 0.7 # share of land for residential
pxl_d <- 0.2 # pixel diameter
nbhd_d <- 1.6 # neighbourhood diameter
consider_categories <- FALSE 
densities <- seq(from = 15, to = 45, by = 10) # dwelling per hectare
# Setting up folders ------------------------------------------------------
output_dir <- "../outputs/Exp3_Nov9_1732/" # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

output_deci_dir <- paste0(output_dir,"decisions") # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_deci_dir), dir.create(output_deci_dir), FALSE)

total_scores_file <- paste(output_dir, "score_summary.csv", sep = "")
total_score_df <- data.frame(density = densities)

# iterating over densities ------------------------------------------------
dph <- densities[1] # dwelling per hectare
for (dph in densities){ # iterating over densities
  
  print(paste0("******************* DWELLING DENSITY: ", dph))
  output_sub_dir <-  paste0(output_dir, "Density_", dph, "/") # one sub-dir for each density
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE) # create if not exists
  log_file <- paste0(output_sub_dir, "output_log_D", dph, ".txt")  # log file for keeping the record
  output_file <- paste0(output_sub_dir, "output_decision_D", dph, ".csv") # decision file indicating where final destination will be located
  cat("", file = log_file, append = FALSE)
  
  cat(paste0("Total Population, ", pop),  file = log_file, append = , sep="\n")
  cat(paste0("Dweling Density, ", dph),  file = log_file, append = TRUE, sep="\n")
  cat(paste0("***********,", "***********"),  file = log_file, append = TRUE, sep="\n")
  
  #  Destinations -----------------------------------------------------------
  init_dest <- read.csv("../inputs/destinations_v4.csv") # list of destinations - from VPA
  # init_dest <- init_dest[1:4,]
  # How many of each destination needed
  init_dest <- init_dest %>% 
    mutate(num_dests=ceiling(pop/init_dest$pop_req))
  
  # Creating the neighbourhoods ---------------------------------------------
  # nbhds are considered as squares 
  nbhd_dev_area <- nbhd_d^2 * share_land_for_resid # land for development per nbhd
  nbhd_n <- ceiling(pop * 0.01 / (dph * pphh * nbhd_dev_area)) # number of nbhds
  #nbhd_p <- round(pop / nbhd_n) # population per nbhds
  nbhd_sq <- make_nbhds(nbhd_d, nbhd_n) # nbhds with geometries
  #plot(nbhd_sq)
  study_area_d <- nbhd_d * (ceiling(sqrt(nbhd_n))+1) # Dimensions of the study area
  nbhd_sq <- nbhd_sq %>% # Adding Land for destinations as a var to nbhds
    mutate(land_for_dest = nbhd_d^2 * share_land_for_dest) %>% 
    mutate(remaining_land_for_dest = land_for_dest)
  
  # Creating pixels ---------------------------------------------------------
  pxl_a <- 0.2*0.2
  pxl_dev_a <- pxl_a * share_land_for_resid # area in each pixel for population (this is used for applying density)
  pxl_n <- ceiling(pop * 0.01 / (dph * pphh * pxl_dev_a))
  init_pixls <- make_pixels_df(pxl_d, share_land_for_dest, pop, dph, 
                               pphh, study_area_d, nbhd_sq) # creating pixles
  #init_pixls <- distribute_population(avg_px_pop, nbhd_sq, init_pixls, pop, init_dest)
  avg_px_pop <- ceiling (pop / pxl_n) # Assuming homogeneous population distribution
  remaining_population <- pop 
  for (nb in nbhd_sq$NBHD_ID) { # populating the pixles
    my_pixls <- which(init_pixls$NBHD_ID == nb)
    for (px in my_pixls){
      init_pixls[px, "pop"] <- min(avg_px_pop, remaining_population)
      remaining_population <- remaining_population - init_pixls$pop[px]
    }
  }
  init_pixls <- init_pixls %>% filter(pop > 0) # Just keeping the pixels with pop
  #plot(init_pixls)
  #plot(init_pixls)
  #st_write(init_pixls, "pxl.sqlite", delete_layer =  T)
  #st_write(nbhd_sq, "nbhd.sqlite", delete_layer =  T)
  
  # Joining nbhds and pixels ------------------------------------------------
  nbhd_sq <- init_pixls %>%
    st_drop_geometry() %>%
    group_by(NBHD_ID) %>%
    summarise(pop = sum(pop)) %>%
    left_join(nbhd_sq, by="NBHD_ID") %>% 
    st_as_sf()
  #plot(nbhd_sq)
  
  # Creating the locations -------------------------------------------------
  init_loc <- make_locations(nbhd_sq, study_area_d)
  #plot(init_loc )
  
  # Creating the decision dataframe -----------------------------------------
  init_deci <- make_decsion_df(init_loc,init_dest) %>% 
    st_as_sf(coords=c("x","y"), remove=F) 

  # Calculating distances between selection points --------------------------
  for (j in 1:nrow(init_pixls)){
    new_col_name <- paste0("distance_to_",as.character(init_pixls$ID[j]))
    init_deci <- init_deci %>% 
      mutate(!!new_col_name:=as.numeric(st_distance(.,init_pixls[j,])))
  }  
  
  # each decision point will have n variables indicating how many
  # destination of type i (i in 1:n) are placed in that location
  # TODO enhance the speed here
  init_deci <- add_decision_vars(init_deci, init_pixls) 
  
  # Sorting init dest based on pop_req*land_req
  # meaning starting from those big and high pop destinations
  init_dest <- init_dest %>%
    mutate(pop_land_weigh = (pop_req*land_req)/sum(pop_req*land_req)) %>%
    arrange(desc(pop_land_weigh)) 
  
  for(dest in init_dest$dest_type_id){
    # Adding destinations to the pixels
    new_col_name <- paste0("not_served_by_",dest)
    init_pixls <- init_pixls %>% 
      mutate(!!new_col_name:=pop)
    # Adding destinations to the locations
    new_col_name <- paste0("num_dest_",dest)
    init_loc <- init_loc %>% 
      mutate(!!new_col_name:=0)
  }
  
  # Evolutionary optimisation -----------------------------------------------
  score <- 100 # Assuming the worst score when no one is served (100%)
  #scoring_denom <- init_dest %>% select(pop_req) %>% colSums(na.rm = TRUE) %>% as.integer()
  init_deci <- init_deci %>%
    left_join(init_dest[,c("dest_type_id","pop_land_weigh")], 
              by = "dest_type_id") %>% 
    rename(dest_weight=pop_land_weigh)
  
  decision <- init_deci 
  iter <- 1
  convergenceCounter <- 0
  while(iter < iters_max + 1){
    print(paste("############## iteration number",iter, sep = ":"))
    
    # A set of temp DFs for iterations
    iter_deci <- init_deci # assigning iter specific variable decision
    iter_nbhds <- nbhd_sq # assigning iter specific variable nbhds
    #iter_nbhds_filtered <- init_nbhd_filtered
    iter_loc <- init_loc # assigning iter specific variable locations
    iter_dest <- init_dest # assigning iter specific variable destinations
    iter_pixls <- init_pixls # assigning iter specific variable pixels
    
    land_flag <- FALSE # a flag for whether there are land avail or not
    No_Answer_flag <- FALSE # a flag for when no answer will be found
    
    # Loop over all destinations
    iter_dest_row <- 1
    for(iter_dest_row in 1:nrow(iter_dest)){
      unavail_decisions <- 0 # TODO check what is this
      iter_dest_type <- iter_dest$dest_type_id[iter_dest_row] # getting the dest type
      iter_dest_position <- iter_dest$position[iter_dest_row] # getting the dest type
      print(paste("destination:",iter_dest_type,"; iteration:",iter,
                  "; dwelling denisty:",dph,sep = " "))
      
      # FIRST destination OF TYPE iter_dest_row is also going through the evolutionary process
      # Repeating the process until all neighborhoods are served
      error_counter <- 0
      remaining_num_dests <- iter_dest$num_dests[iter_dest_row]
      while((get_unsrvd_pop(iter_pixls, iter_dest_type) > pop*(1-iter_dest$coverage[iter_dest_row])) & (remaining_num_dests > 0)){ # loop until all are served AND we have destinations to use
        # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS
        feasible_locs <- find_feasible_locs(iter_deci, iter_pixls,
                                            iter_dest, iter_dest_row,
                                            iter_dest_position, consider_categories)
        
        #st_write(iter_deci, "iter_deci.sqlite", delete_layer = T)
        #st_write(iter_pixls, "iter_pixls.sqlite", delete_layer = T)
        
        feasible_locs <- feasible_locs %>%
          filter(!(dest_id %in% unavail_decisions))
        
        #st_write(feasible_locs, "feasibleLocs.sqlite", delete_layer = T)
        
        if(nrow(feasible_locs)==0){
          print("No Answer")
          No_Answer_flag <- TRUE
          unavail_decisions <- c(unavail_decisions, new_dest_id)
          break();
        }
        
        # selecting one from max catchments by random
        new_dest_id <- feasible_locs %>%
          st_drop_geometry() %>% 
          filter(catchment_potential == max(catchment_potential)) %>%
          select(dest_id) %>% 
          sample_n(size = 1)
        
        # mutation ------------------------------------------------------------
        some_rnd <- runif(n =1,0,1)
        if(some_rnd < mutation_p) new_dest_id <- sample(feasible_locs$dest_id, 1)
        
        print(paste0("new_dest_id: ", new_dest_id))
        
        # Opening a new destination -------------------------------------------
        new_deci_row <- which(iter_deci$dest_id == as.character(new_dest_id))
        
        # Finding neighborhoods adjacent to the location for land
        loc_nbhds<- find_land_contributors(iter_loc, iter_deci, 
                                           iter_nbhds, new_deci_row)
        
        # land to occupy
        land_to_occupy <- iter_dest$land_req[which(iter_dest$dest_type_id == iter_deci$dest_type_id[new_deci_row])]
        
        if(!check_total_land(land_to_occupy,loc_nbhds,iter_nbhds)){
          #print("SPACE")
          error_counter <- error_counter + 1
          unavail_decisions <- c(unavail_decisions, new_dest_id)
          if(error_counter > 100){
            print("not enough space, SERIOUSLY! V2")
            land_flag <- TRUE
            stop();
          }
        }else{
          # Adding the destination to decision and location DFs
          iter_deci <- add_destination_to_decision(iter_deci,new_deci_row,
                                                   iter_dest_type)
          iter_loc <- add_destination_to_location(iter_loc, iter_deci, 
                                                  new_deci_row, iter_dest_type)
          # Occupying the land
          iter_nbhds <- occupy_land(loc_nbhds, iter_nbhds, land_to_occupy)
          # Updating number of remaining destinations
          remaining_num_dests <- remaining_num_dests - 1
          print(paste0("Number of remaining destinations to open: ", remaining_num_dests))
          
          # SERVING THE NEIGHBOURS  -------------------------------------------
          # finding those closest to this
          close_pxls_id <- iter_deci[new_deci_row,] %>% 
            st_buffer(as.numeric(st_drop_geometry(iter_deci[new_deci_row,"dist_in_20_min"]))) %>% 
            st_intersects(st_centroid(iter_pixls),sparse = F)
          
          close_pxls <- iter_pixls[close_pxls_id,] %>% 
            st_centroid() %>% 
            mutate(dist_to_dest = st_distance(.,iter_deci[new_deci_row,])) %>% 
            arrange(dist_to_dest)
          
          # Find all those in range and not fully served
          if(!nrow(close_pxls) > 0) message("**Something is not right**")
          
          # Add a check here for if there are unserved pop in the close pxls
          for (i in 1:nrow(close_pxls)) {
            #temp_dist <- as.double(dists_ord[i])
            temp_pixl_id <- st_drop_geometry(close_pxls[i,"ID"]) %>% 
              as.numeric()
            temp_pixl_row <- which(iter_pixls$ID==temp_pixl_id)
            
            # TODO there seems to be an issue here, check if it is getting the unsrved pop correctly
            pxls_unsrved_pop <-  get_unsrvd_pop(iter_pixls[temp_pixl_row,],
                                                iter_dest_type)
            if (iter_deci$pop_remainder[new_deci_row] > 0 & pxls_unsrved_pop > 0){
              # If distance is less than 20 min given the mode speed and nb has unserved pop
              pop2cover <- min(iter_deci$pop_remainder[new_deci_row], 
                               pxls_unsrved_pop) # finding population to cover
              
              # updating not served pop in deci and pixles
              iter_deci$pop_remainder[new_deci_row] <- iter_deci$pop_remainder[new_deci_row] - pop2cover
              iter_pixls[temp_pixl_row, paste("not_served_by_", 
                                              iter_dest_type, sep = "")] <- pxls_unsrved_pop - pop2cover
              # adding the destinations to iter deci df
              deci_col <- paste0("is_serving_", iter_pixls$ID[temp_pixl_row])
              iter_deci[new_deci_row, deci_col] <- 1
            }
          }
        }
      }
      if(No_Answer_flag){
        break;
      }
      iter_dest[iter_dest_row, "pop_unsrvd"] <- get_unsrvd_pop(iter_pixls, iter_dest_type)
      
      }
    
    # Scoring and best solution selection -------------------------------------
    
    if(land_flag){
      print("I AM HERE")
      iter_score <- nrow(init_deci) * (-100)
    }else if(No_Answer_flag){
      print("I AM HERE 2")
      iter_score <- nrow(init_deci) * (-100)
    }
    else{
      iter_score <- 100*(sum(iter_dest$pop_unsrvd)/(pop*nrow(iter_dest))) 
      if(is.na(iter_score)) iter_score=100
    }
    
    cat(paste("Iteration,",iter, sep = ","),file=log_file,append=TRUE, sep="\n")
    cat(paste("Iteration_Score,",iter_score, sep = ","),file=log_file,append=TRUE, sep="\n")
    cat(paste("Iteration_result,",iter_score, sep = ","),file=log_file,append=TRUE, sep="\n")
    
    # Checking if it is a new best result
    if(iter_score < score){
      convergenceCounter <- 0
      score <- iter_score
      decision <- iter_deci
      neighbourhood_output <- iter_nbhds 
      pixels_output <- iter_pixls
      cat(paste("Iteration_result,","NEW_BEST_SCORE", sep = ","),file=log_file,append=TRUE, sep="\n")
      #cat(paste("new_Best_Score,",score, sep = ","),file=log_file,append=TRUE, sep="\n")
      print(paste0("new_Best_Score, ",score))
    }else if(land_flag){
      cat(paste("Iteration_result,","NO_SPACE", sep = ","),file=log_file,append=TRUE, sep="\n")
    }else if(No_Answer_flag){
      cat(paste("Iteration_result,","NO_SPACE", sep = ","),file=log_file,append=TRUE, sep="\n")
    }else{
      print("iteration score not changed")
      convergenceCounter <- convergenceCounter + 1
      print(paste0("convergence counter: ", convergenceCounter))
      cat(paste("Iteration_result,","NORMAL", sep = ","),file=log_file,append=TRUE, sep="\n")
    }
    cat("******, ******",file=log_file,append=TRUE, sep="\n")
    if(convergenceCounter>convergenceIterations){
      print(paste0("Skipping the rest, model seems to be converged at iter ", iter))
      break
    } 
    iter <- iter + 1
  }
  # Writing some of the inputs
  #write.csv(neighbourhood_output, file = paste(output_sub_dir, "nbhd_", "D", dph, ".csv", sep = ""))
  #write.csv(pixels_output, file = paste(output_sub_dir, "pixls", "D", dph, ".csv", sep = ""))
  # writing outputs 
  cat(paste("Final_best_score", score, sep = ","),file=log_file,append=TRUE, sep="\n")
  total_score_df$score[which(total_score_df$density == dph)] <- score
  #write.csv(decision, file = output_file)

  # decision writing
  decision %>% 
    mutate(density = dph) %>% 
    dplyr::select(-starts_with("distance")) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="decision",delete_layer=T)
  
  pixels_output %>% 
    mutate(density = dph) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="pixels",delete_layer=T)
  
  neighbourhood_output %>% 
    mutate(density = dph) %>% 
    st_write(paste0(output_deci_dir,"/D",dph,".sqlite"),layer="nbhd",delete_layer=T)
}

#write.csv(total_score_df,file = total_scores_file)
#summerise_destinations(densities, output_dir)
#plot_scores(total_score_df)
#plot_destinations()




