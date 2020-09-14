#### NEIGHBOURHOOD FACILTIY LOCATION OPTIMISATION #### 
# 
# This is a code that optimises a neighbourhood destination locations to achieve 20 Min
# neighbourhood using an evolutionary alogrithm
#
# @author AJ

# TODOs
# 1. Making the thresholds less strict
# 2. Only major infrastructure
# 3. 


# Packages
library(rdist)
library(dplyr)
library(ggplot2)
library(sf)
library(readr)


source("./functions/initation_functions.R")
source("./functions/land_updating.R")
source("./functions/visualisation.R")
source("./functions/make_pixels_df.R")
source("./functions/distribute_population.R")
source("./functions/make_nbhds.R")

# Control variables
pphh <- 2.6
pop <- 30000
mutation_p <- 0.20
total_iters <- 10

share_land_for_dest <- 0.25
share_land_for_resid <- 0.7
pxl_d <- 0.2
nbhd_d <- 1.6
dph <- 15

catchment_treshold <- 0.8 # TODO make this dependent on the destination type
consider_categories <- FALSE # TODO make this to work
densities <- seq(from = 15, to = 55, by = 2.5)

# Setting up folders ------------------------------------------------------

output_dir <- "../outputs/RunNewSeptember/" # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

total_scores_file <- paste(output_dir, "score_summary.csv", sep = "")
total_score_df <- data.frame(density = densities)

for (dph in densities){
  print(paste("******************* DWELLING DENSITY:", dph, sep = " "))
  # REading inputs
  init_nbhd <- read.csv("../inputs/neighbourhoods.csv")
  init_loc <- read.csv("../inputs/locations.csv")
  init_dest <- read.csv("../inputs/destinations_v3.csv")
  
  # Setting up folders for the denisty
  output_sub_dir <-  paste(output_dir, "Density_", dph, "/", sep = "")
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE)
  log_file <- paste(output_sub_dir, "output_log_D", dph, ".txt", sep = "") 
  output_file <- paste(output_sub_dir, "output_decision_D", dph, ".csv", sep = "")
  cat("", file = log_file, append = FALSE)

  # CALCUALTING NUMBER OF NEIGHBOURHOODS
  
  pxl_a <- 0.2*0.2
  pxl_dev_a <- pxl_a * share_land_for_resid
  pxl_n <- ceiling(pop * 0.01 / (dph * pphh * pxl_dev_a))
  
  # How many pixels?
  nbhd_sq <- make_nbhds(nbhd_d)
  nbhd_a <- nbhd_d * nbhd_d # As squares (sq.km)
  nbhd_dev_a <- nbhd_a * share_land_for_resid # As squares (sq.km)
  
  #nbhd_a <- 0.4*0.4*pi # As circles (sq.km)
  nbhd_n <- ceiling(pop * 0.01 / (dph * pphh * nbhd_dev_a))
  nbhd_p <- round(pop / nbhd_n)
  
  # Creating the neighbourhoods ---------------------------------------------
  study_area_d <- nbhd_d * (nbhd_n + 2)
  
  init_pixls <- make_pixels_df(pxl_d, share_land_for_dest, pop, dph, pphh, study_area_d, nbhd_sq)

  avg_px_pop <- ceiling (pop / pxl_n)
  
  #init_pixls <- distribute_population(avg_px_pop, init_nbhd, init_pixls, pop, init_dest)

  remaining_population <- pop
  for (nb in nbhd_sq$NBHD_ID) {
    my_pixls <- which(init_pixls$NBHD_ID == nb)
    for (px in my_pixls){
      init_pixls[px, "pop"] <- min(avg_px_pop, remaining_population)
      remaining_population <- remaining_population - init_pixls$pop[px]
    }
  }
  
  # Selecting only occupied pixels
  init_pixls <- init_pixls %>% filter(pop > 0)
  # Adding Land
  for (i in 1:nbhd_n){
    init_nbhd$remaining_land_for_dest[i] <- init_nbhd$land_for_dest[i] <- nbhd_a * share_land_for_dest
  }

  
  for (nb in init_nbhd$ID) {
    init_nbhd$pop[nb] <- init_pixls %>% 
      filter(NBHD_ID == nb) %>% 
      summarise(pop = sum(pop)) %>%
      select(pop)
  }
  
  init_nbhd$pop <- init_nbhd$pop %>% unlist()
  
  cat(paste("Total Population", pop, sep = ","),  file = log_file, append = , sep="\n")
  cat(paste("Dweling Density", dph, sep = ","),  file = log_file, append = TRUE, sep="\n")
  cat(paste("***********", "***********", sep = ","),  file = log_file, append = TRUE, sep="\n")

  # Creating the locations ---------------------------------------------
  
  init_loc <- make_location_df_methdod_2(init_loc, init_dest, init_nbhd[which(init_nbhd$pop>0),] )
  #init_loc <- make_location_df_methdod_2(init_loc, init_dest, init_pixls[which(init_pixls$pop>0),] )
  
  # Creating the decision dataframe --------------------------
  init_deci <- make_decsion_df(init_loc,init_dest)
  
  init_deci_sf <- init_deci %>% 
    st_as_sf(coords=c("x","y"), remove=F) 
  
  # Calculating distances between selection points --------------------------
  for (j in 1:nrow(init_pixls)){
    new_col_name <- paste0("distance_to_",as.character(init_pixls$ID[j]))
    init_deci_sf <- init_deci_sf %>% 
      mutate(!!new_col_name:=as.numeric(st_distance(.,init_pixls[j,])))
  }  
  
  init_deci <- add_decision_vars(init_deci, init_pixls)
  # Soring init dest based on pop_req*land_req_sqkm
  
  init_dest <- init_dest %>%
    mutate(pop_land_score = pop_req*land_req_sqkm) %>%
    mutate(pop_land_weigh = pop_land_score/sum(pop_land_score)) %>%
    arrange(desc(pop_land_weigh)) %>%
    select(-pop_land_score)
  
  # Evolutionary optimisation -----------------------------------------------
  
  iter <- 1
  # Creating worst case score
  score <- nrow(init_deci) * (-100)
  #scoring_denom <- init_dest %>% select(pop_req) %>% colSums(na.rm = TRUE) %>% as.integer()
  for(i in 1:nrow(init_deci)){
    init_deci$dest_weight[i] <- init_dest$pop_land_weigh[which(init_dest$dest_type_id == init_deci$dest_type_id[i])]
  }
  
  decision <- init_deci
  while(iter < total_iters + 1){
    print(paste("############## iteration number",iter, sep = ":"))
    # Resetting the variables
    iter_deci <- init_deci
    iter_nbhds <- init_nbhd
    #iter_nbhds_filtered <- init_nbhd_filtered
    iter_loc <- init_loc
    iter_dest <- init_dest
    iter_pixls <- init_pixls
    
    land_flag <- FALSE
    No_Answer_flag <- FALSE
    
    iter_dest_row <- 1
    # Loop over all destinations
    for(iter_dest_row in 1:nrow(iter_dest)){
      unavail_decisions <- 0
      iter_dest_type <- iter_dest$dest_type_id[iter_dest_row]
      print(paste("destination:",iter_dest_type, "; iteration:", iter, "; dwelling denisty:", dph, sep = " "))
      this_dest_deci_rows <- which(iter_deci$dest_type_id == iter_dest_type)
      # FIRST destination OF TYPE iter_dest_row is also going trought the evolutionary process
      # Repeating the process until all neighbourhoods are served
      error_counter <- 0
      while(sum(iter_pixls[,paste("pop_not_served_by_dest_", iter_dest_type, sep = "")], na.rm = TRUE) > pop*(1-catchment_treshold)){
        # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS
        feasible_locs <- find_feasible_locs(iter_deci, iter_pixls, iter_dest, this_dest_deci_rows, iter_dest_row)
        
        feasible_locs <- feasible_locs %>%
          filter(!(dest_id %in% unavail_decisions))
        
        if(nrow(feasible_locs)==0){
          print("No Answer")
          No_Answer_flag <- TRUE
          unavail_decisions <- c(unavail_decisions, new_dest_id)
          break();
        }
        
        # selecting one from max catchments by random
        new_dest_id <- feasible_locs %>%
          filter(catchment_potential == max(catchment_potential)) %>%
          select(dest_id) %>%
          sample_n(size = 1)
        #print(paste("new_dest_id:", new_dest_id))
        
        # mutation
        if(runif(n = 1, 0, 1) < mutation_p ){ # add number of iters with mutations to here too
          #print("mutation")
          new_dest_id <- sample(feasible_locs$dest_id, 1)
        }
        
        # Opening a new destination
        new_deci_row <- which(iter_deci$dest_id == as.character(new_dest_id))
        
        temp_loc_nbhds<- find_land_contributors(iter_loc, iter_deci, new_deci_row)
      
        # land to occupy
        land_to_occupy <- iter_dest$land_req_sqkm[which(iter_dest$dest_type_id == iter_deci$dest_type_id[new_deci_row])]
        land_to_occupy_per_nbhd <- land_to_occupy/length(temp_loc_nbhds)
      
        is_total_land_enough <- check_total_land(land_to_occupy,temp_loc_nbhds,iter_nbhds)
        # Total remaining land
        
        if(!is_total_land_enough){
          #print("SPACE")
          error_counter <- error_counter + 1
          unavail_decisions <- c(unavail_decisions, new_dest_id)
          if(error_counter > 100){
            print("not enough space, SERIOUSLY! V2")
            land_flag <- TRUE
            stop();
            
          }
        }else{
          
          iter_deci <- add_destination_to_decision(iter_deci,new_deci_row,iter_dest_type)
          iter_loc <- add_destination_to_location(iter_loc, iter_deci, new_deci_row, iter_dest_type)
          iter_nbhds <- occupy_land(temp_loc_nbhds, iter_nbhds, land_to_occupy, land_to_occupy_per_nbhd)
          
          # SERVING THE NEIGHBOURS 
          # finding those closest to this
          dists <- iter_deci[new_deci_row, grepl("^[0-9]", x = colnames(iter_deci))]
          dists_ord <- sort(dists)
          
          # Find all those in range and not fully served
          if(nrow(dists_ord > 0)){
            for (i in 1:length(dists_ord)) {
              temp_dist <- as.double(dists_ord[i])
              temp_pixl_row <- which(iter_pixls$ID == colnames(dists_ord)[i]) 
              # If distance is less than 20 min given the mode speed and nb has unserved pop
              if (iter_deci$remaining_pop_cap[new_deci_row] > 0 & temp_dist <= iter_deci$dist_in_20_min[new_deci_row] &  iter_pixls[temp_pixl_row, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] > 0){
                # serve
                new_covered_pop <- min(iter_deci$remaining_pop_cap[new_deci_row], iter_pixls[temp_pixl_row, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")])
                iter_deci$remaining_pop_cap[new_deci_row] <- iter_deci$remaining_pop_cap[new_deci_row] - new_covered_pop
                iter_pixls[temp_pixl_row, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] <- iter_pixls[temp_pixl_row, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] - new_covered_pop
                iter_deci[new_deci_row, which(colnames(iter_deci) == paste("is_serving", iter_pixls$ID[temp_pixl_row], sep = "_"))] <- 1
              }
            }
          }
        }
      }
      if(No_Answer_flag){
        break;
      }
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
      iter_score <- iter_deci %>%
        filter(total_pop_cap > 0) %>%
        mutate(dest_score = ((remaining_pop_cap/total_pop_cap)+1) * num_open * (-1)*dest_weight) %>%
        summarise(sum(dest_score)) %>%
        as.double()
    }
    
    cat(paste("Iteration,",iter, sep = ","),file=log_file,append=TRUE, sep="\n")
    cat(paste("Iteration_Score,",iter_score, sep = ","),file=log_file,append=TRUE, sep="\n")
    cat(paste("Iteration_result,",iter_score, sep = ","),file=log_file,append=TRUE, sep="\n")
    # Checking if it is a new best result
    
    if(iter_score > score){
      score <- iter_score
      decision <- iter_deci
      neighbourhood_output <- iter_nbhds 
      pixels_output <- iter_pixls
      cat(paste("Iteration_result,","NEW_BEST_SCORE", sep = ","),file=log_file,append=TRUE, sep="\n")
      #cat(paste("new_Best_Score,",score, sep = ","),file=log_file,append=TRUE, sep="\n")
      print(paste("new_Best_Score,",score, sep = ","))
    }else if(land_flag){
      cat(paste("Iteration_result,","NO_SPACE", sep = ","),file=log_file,append=TRUE, sep="\n")
    }else if(No_Answer_flag){
      cat(paste("Iteration_result,","NO_SPACE", sep = ","),file=log_file,append=TRUE, sep="\n")
    }else{
      cat(paste("Iteration_result,","NORMAL", sep = ","),file=log_file,append=TRUE, sep="\n")
    }
    cat("******, ******",file=log_file,append=TRUE, sep="\n")
    iter <- iter + 1

  }
  # Writing some of the inputs
  write.csv(neighbourhood_output, file = paste(output_sub_dir, "nbhd_", "D", dph, ".csv", sep = ""))
  write.csv(pixels_output, file = paste(output_sub_dir, "pixls", "D", dph, ".csv", sep = ""))
  # writing outputs 
  cat(paste("Final_best_score", score, sep = ","),file=log_file,append=TRUE, sep="\n")
  total_score_df$score[which(total_score_df$density == dph)] <- score
  write.csv(decision, file = output_file)
  
}

write.csv(total_score_df,file = total_scores_file)
summerise_destinations(densities, output_dir)
plot_scores(total_score_df)
plot_destinations()
# Generating plots and output files ------------------------------------------------------

#toPlot <- decision %>%
#  select(x, y, num_open) %>%
#  mutate(status = ifelse(num_open >= 1, "Serving", "NotServing")) %>%
#  mutate(type = "destination") %>%
#  select(x, y, status, type)

#toPlot2 <- init_nbhd %>%
#  select(x,y) %>%
#  mutate(status = "FullyServed") %>%
#  mutate(type = "neighbourhood")

# toPlot <- rbind(toPlot, toPlot2)

#library(ggplot2)
#ggplot(data = toPlot, aes(x, y, fill = status, color = status)) + geom_point(aes(shape = type), size=4)

# TODO saving the graph