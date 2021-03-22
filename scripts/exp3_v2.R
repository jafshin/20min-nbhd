
#### NEIGHBOURHOOD FACILTIY LOCATION OPTIMISATION #### 
# TODO Make the scoring to happen after each category mutation and not just after all mutations combined - we want co-evolutionalry alogrithm.
# TODO and potentially choose from best alternatives! rather than fully randomly
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
library(stringr)

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

# dph <- 35
optimise_nbhds <- function(dph) {

# Step 0: Setting up inputs and structure ---------------------------------
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
  
  #  Destinations
  dests <- read.csv("../inputs/destinations_v6.csv") 
  # How many of each destination needed
  dests <- dests %>% 
    mutate(num_dests=ceiling(pop/dests$pop_req))

# Step 1 Creating decision grid -------------------------------------------
  # Creating the neighbourhoods 
  nbhd_dev_area <- nbhd_d^2 * share_land_for_resid # land for development per nbhd
  nbhd_n <- ceiling(pop * 0.01 / (dph * pphh * nbhd_dev_area)) # number of nbhds
  nbhds <- make_nbhds(nbhd_d, nbhd_n) # nbhds with geometries
  study_area_d <- nbhd_d * (ceiling(sqrt(nbhd_n))+1) # Dimensions of the study area
  nbhds <- nbhds %>% # Adding Land for destinations as a var to nbhds
    mutate(land_for_dest = nbhd_d^2 * share_land_for_dest) %>% 
    mutate(remaining_land_for_dest = land_for_dest)
  
  # Creating pixels
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
    mutate(nbhdQ=ifelse(test = pxl_x>nbhd_x & pxl_y<nbhd_y, 
                        yes = paste0(NBHD_ID,"_2"), no=nbhdQ)) %>% 
    mutate(nbhdQ=ifelse(test = pxl_x<nbhd_x & pxl_y<nbhd_y, 
                        yes = paste0(NBHD_ID,"_3"), no=nbhdQ)) %>% 
    mutate(nbhdQ=ifelse(test = pxl_x<nbhd_x & pxl_y>nbhd_y,
                        yes = paste0(NBHD_ID,"_4"), no=nbhdQ))  
  
  avg_px_pop <- ceiling (pop / pxl_n) # Assuming homogeneous population distribution
  remaining_population <- pop 
  for (nb in nbhds$NBHD_ID) { # populating the pixles
    my_pixls <- which(pixls$NBHD_ID == nb)
    for (px in my_pixls){
      pixls[px, "pxl_pop"] <- min(avg_px_pop, remaining_population)
      remaining_population <- remaining_population - pixls$pxl_pop[px]
    }
  }
  pixls <- pixls %>% filter(pxl_pop > 0) %>% # Just keeping the pixels with pop
    mutate(type="resid", destID="NA")
  
  if (consider_categories) {
    # finding near the local town centres (400m)
    pixls <- pixls %>%  
      mutate(position=ifelse(dist2ltc<0.4, yes = "nltc", no = "etc")) %>% 
      mutate(position=ifelse(dist2ltc<0.01, yes = "ltc", no = position)) 
  }else{
    pixls <- pixls %>%  
      mutate(position='all')  
  }
  
  # Joining nbhds and pixels
  nbhds <- pixls %>%
    group_by(NBHD_ID) %>%
    summarise(pxls_pop = sum(pxl_pop)) %>%
    left_join(nbhds, by="NBHD_ID")  %>% 
    filter(pxls_pop>0)
  
  # Sorting init dest based on pop_req*land_req
  # meaning starting from those big and high pop destinations
  dests <- dests %>%
    arrange(order,desc(land_req),desc(pop_req)) 
  
  for(dest in dests$destCode){
    # Adding destinations to the pixels
    pixls <- pixls %>% 
      mutate(!!paste0("not_served_by_",dest):=pxl_pop) %>% 
      mutate(!!paste0("num_dest_", dest):=0) %>% 
      mutate(!!paste0("pop_total_", dest):=0) %>% 
      mutate(!!paste0("pop_remaining_", dest):=0)
  }

  nbhdsTemp <- nbhds # assigning iter specific variable nbhds
  destList <- dests # assigning iter specific variable destinations
  pxlsTemp <- pixls # assigning iter specific variable pixels
  No_Answer_flag <- FALSE # a flag for when no answer will be found

# Step 2: Initial layout  -------------------------------------------------
  # Loop over all destinations
  # destRow=1
  for(destRow in 1:nrow(destList)){
    destCode <- destList$destCode[destRow] # getting the dest type
    iter_dest_position <- destList$position[destRow] # getting the dest type
    echo(paste("destination:",destCode,"; dwelling denisty:",dph,sep=" "))
    
    # FIRST destination OF TYPE destRow is also going through the evolutionary process
    # Repeating the process until all neighborhoods are served
    error_counter <- 0
    remaining_num_dests <- destList$num_dests[destRow]
    while((get_unsrvd_pop(pxlsTemp, 
                          destCode) > pop*(1-destList$coverage[destRow])) & 
          (remaining_num_dests > 0)){ # loop until all are served AND we have destinations to use
      # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS
      cellsToOccupy <- max(1,round(destList$land_req[destRow]/pxl_a))
      system.time(
        destCellsID <- findDestinationCells(pxlsTemp,destList,cellsToOccupy,
                                                      destCode,pxl_a))
      destCellsRow <- which(pxlsTemp$ID%in%destCellsID)
      if(length(destCellsID)==0){ 
        echo("No Answer")
        No_Answer_flag <- TRUE
        break();
      }
      
      # Add destination to the the cell
      pxlsTemp$type[destCellsRow] <- destCode
      remaining_num_dests <- remaining_num_dests - 1
      pxlsTemp$destID[destCellsRow] <- paste0(destCode,"_",
                                             destList$num_dests[destRow]-remaining_num_dests)
      # Adding new capacity
      pop_remainder_col <- paste0("pop_remaining_",destCode)
      pop_total_col <- paste0("pop_total_",destCode)
      num_dest_col <- paste0("num_dest_",destCode)
      
      pxlsTemp[destCellsRow,pop_total_col] <- destList[destRow,"capacity"]
      pxlsTemp[destCellsRow,pop_remainder_col] <- destList[destRow,"capacity"]
      pxlsTemp[destCellsRow,num_dest_col] <- 1
      
      # Start serving people around
      destGeom <- pxlsTemp %>% 
        filter(ID%in%destCellsID) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- destGeom %>% 
        st_buffer(0.8) %>% 
        st_intersects(pxlsTemp %>% st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)) 
      
      cellsWithinDistID <- pxlsTemp[unlist(cellsWithinDistRow),] %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist2Dest) %>%
        dplyr::select(ID)
      
      if(!nrow(cellsWithinDistID) > 0) message("**Something is not right**")
      if(!nrow(cellsWithinDistID) > 0) stop()
      # Serving cells 
      # id = cellsWithinDistID$ID[1]
      for (id in cellsWithinDistID$ID) {
        # print(id)
        cellRow <-  which(pxlsTemp$ID==id)
        cell_unsrved_pop <-  get_unsrvd_pop(pxlsTemp[cellRow,],
                                            destCode)
        
        if (pxlsTemp[destCellsRow,pop_remainder_col][1] > 0 & cell_unsrved_pop > 0){
          pop2cover <- min(pxlsTemp[destCellsRow,pop_remainder_col][1] , 
                           cell_unsrved_pop) # finding population to cover
          # Updating the capapcity
          pxlsTemp[destCellsRow,
                     pop_remainder_col] <- pxlsTemp[destCellsRow,
                                                      pop_remainder_col][1]-pop2cover
          # Updating unserved pop
          pxlsTemp[cellRow,paste0("not_served_by_",
                                    destCode)]<-cell_unsrved_pop-pop2cover
        }
      } 
    }
  }
  pxlsTemp %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(outputSqlite, layer = paste0(dph,"_step2"), delete_layer=T)
  # In this step, population from pixels occupied with dests will be removed
  # And will be uniformly distributed in other locations
  # We will use random samples in order to redistribute

# Step 3: Updating demand distribution ------------------------------------
  homelessPop <- pxlsTemp %>%
    filter(type!="resid") %>% 
    summarise(homelessPop = sum(pxl_pop)) %>% 
    unlist()
  
  hostPxls <- pxlsTemp %>% 
    filter(type=="resid") %>% 
    filter(pxl_pop>0) %>% 
    dplyr::select(ID) %>%
    slice_sample(n=homelessPop,replace=T)
  
  popServCols <- grep("not_",colnames(pxlsTemp))
  nonResids <- which(pxlsTemp$type!="resid" & pxlsTemp$pxl_pop>0)
  pxlsTemp[nonResids,"pxl_pop"] <- 0
  pxlsTemp[nonResids,popServCols] <- 0
  #i=3963
  for (i in 1:nrow(pxlsTemp)) {
    if(pxlsTemp[i,"ID"]%in%hostPxls$ID){
      pxlsTemp[i,"incomingPop"]=length(hostPxls[hostPxls==pxlsTemp[i,"ID"]])
      pxlsTemp[i,"pxl_pop"]=pxlsTemp[i,"pxl_pop"]+pxlsTemp[i,"incomingPop"]
      pxlsTemp[i,popServCols]=pxlsTemp[i,popServCols]+pxlsTemp[i,"incomingPop"]
    }
    else{
      pxlsTemp[i,"incomingPop"]=0
    }
  }
  pxlsTemp %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(outputSqlite, layer = paste0(dph,"_step3"), delete_layer=T)
# Step 4: Scoring ---------------------------------------------------------
  
  if(No_Answer_flag){  
    scoreTemp <- nrow(dests)*(200)
  }else{
    source("./functions/scoring.R")
    scoreTemp <- getScore(pxlsTemp, destList) 
    pxlsBest <- pxlsTemp
    scoreBest <- scoreTemp
  }

### Here is where the While loop should start
  iter <- 1
  convergenceCounter <- 0
  while(iter < iters_max + 1){
    echo(paste("############## iteration number",iter, sep = ":"))
    
# Step5: Mutating Supply --------------------------------------------------
    # Select some dests to mutate:
    destsForMutation <- pxlsTemp %>% 
      filter(destID!="NA") %>%  
      group_by(destID,type) %>% 
      summarise(areaPxls=n()) %>% 
      ungroup() %>% 
      slice_sample(prop=mutation_p) %>% 
      as.data.frame()
    
    # Changing the locations with free cells
    # i=12
    for (i in 1:nrow(destsForMutation)){
      destToMutateID <- destsForMutation[i,"destID"]
      destToMutateType <- destsForMutation[i,"type"]
      pxlsToMove <- destsForMutation[i,"areaPxls"]
      origCells <- which(pxlsTemp$destID==destToMutateID)
      # Find where to move
      system.time(destCellsID <- findDestinationCells(pxlsTemp,destList,pxlsToMove,
                                                      destToMutateType,pxl_a))
      if(length(destCellsID)==0){
        echo("No Answer")
        No_Answer_flag <- TRUE
        break();}
      destCellsRow <- which(pxlsTemp$ID%in%destCellsID)
      # Get population to move
      popToMove <- pxlsTemp %>% 
        filter(ID%in%destCellsRow) %>% 
        summarise(total=sum(pxl_pop)) %>% 
        as.numeric()
      # Adding destinations to the previously resids
      pxlsTemp[destCellsRow,"type"] <- destToMutateType
      pxlsTemp[destCellsRow,"destID"] <- destToMutateID
      pxlsTemp[destCellsRow, "pxl_pop"] <- 0 # not resid anymore
      capacityToAdd <- destList[which(destList$destCode==destToMutateType),
                                 "capacity"]
      pxlsTemp[destCellsRow,
                 paste0("pop_total_",destToMutateType)] <- capacityToAdd
      pxlsTemp[destCellsRow,
                 paste0("pop_remaining_",destToMutateType)] <- capacityToAdd
      pxlsTemp[destCellsRow,
                 paste0("num_dest_",destToMutateType)] <- 1
      # Adding population to the previously dests
      pxlsTemp[origCells,"type"] <- "resid"
      pxlsTemp[origCells,"destID"] <- "NA"
      pxlsTemp[origCells, "pxl_pop"] <- floor(popToMove/length(origCells)) # using floor to not to add extra
      extraPop <- popToMove%%length(origCells) # get the reminder from the flow
      pxlsTemp[origCells[1:extraPop],"pxl_pop"]<-pxlsTemp[origCells[1:extraPop],
                                                              "pxl_pop"] +1  
      pxlsTemp[origCells, paste0("pop_total_",destToMutateType)] <- 0
      pxlsTemp[origCells,paste0("pop_remaining_",destToMutateType)] <- 0
      pxlsTemp[destCellsRow, paste0("num_dest_",destToMutateType)] <- 0
    }
  
# Step6: Updating the service ---------------------------------------------
    # Setting everyone to unserved
    popServCols <- grep("not_",colnames(pxlsTemp))
    pxlsTemp[which(pxlsTemp$type=="resid"),
                 popServCols] <- pxlsTemp[which(pxlsTemp$type=="resid"),
                                            "pxl_pop"]
    # Setting all capacities to full cap
    for (dest in destList$destCode) {
      pxlsTemp[which(pxlsTemp$type==dest),
                 paste0("pop_total_",dest)] <- pxlsTemp[which(pxlsTemp$type==dest),
                                                          paste0("pop_remaining_",dest)]
    }
    destTypeIDs <- pxlsTemp %>% filter(destID!="NA") %>%  distinct(destID) 
    
    # Serving everyone
    # dest <- destTypeIDs[2,]
    for(dest in destTypeIDs$destID){
      destType <- str_extract(dest, "[^_]+")
      destRow <- which(destList$destCode==destType)
      echo(paste("destination:",destType,"; dwelling denisty:",dph,sep=" "))
      destCellsRow <- which(pxlsTemp$destID==dest)
      destCellsID <- pxlsTemp[which(pxlsTemp$destID==dest),"ID"]
        
      # Start serving people around
      destGeom <- pxlsTemp %>% 
        filter(ID%in%destCellsID) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- destGeom %>% 
        st_buffer(0.8) %>% 
        st_intersects(pxlsTemp %>% st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)) 
      
      cellsWithinDistID <- pxlsTemp[unlist(cellsWithinDistRow),] %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist2Dest) %>%
        dplyr::select(ID)
      
      if(!nrow(cellsWithinDistID) > 0) message("**Something is not right**")
      # Serving cells 
      # id = cellsWithinDistID$ID[1]
      for (id in cellsWithinDistID$ID) {
        cellRow <-  which(pxlsTemp$ID==id)
        cell_unsrved_pop <-  get_unsrvd_pop(pxlsTemp[cellRow,],
                                            destCode)
        
        if (pxlsTemp[destCellsRow,pop_remainder_col][1] > 0 & cell_unsrved_pop > 0){
          # print("here")
          pop2cover <- min(pxlsTemp[destCellsRow,pop_remainder_col][1] , 
                           cell_unsrved_pop) # finding population to cover
          # Updating the capapcity
          pxlsTemp[destCellsRow,pop_remainder_col] <- pxlsTemp[destCellsRow,pop_remainder_col][1]-pop2cover
          # Updating unserved pop
          pxlsTemp[cellRow, paste("not_served_by_",
                                    destCode, sep = "")] <- cell_unsrved_pop - pop2cover
        }
      }
    }
    
# Step7 Evaluation --------------------------------------------------------
    # Check the Score, If better keep, if not discard
    if(No_Answer_flag){  
      scoreTemp <- nrow(dests)*(200)
    }else{
      scoreTemp <- getScore(pxlsTemp, destList) 
    }
    if(scoreTemp<scoreBest){
      pxlsBest <- pxlsTemp
      scoreBest <- scoreTemp
    }else{
      echo("best score not changed")
      convergenceCounter <- convergenceCounter + 1
      echo(paste0("convergence counter: ", convergenceCounter))
      echo(paste("Iteration_result,","NORMAL", sep = ","))
    }
    echo("******, ******")
    if(convergenceCounter>convergenceIterations){
      echo(paste0("Skipping the rest, model seems to be converged at iter ", iter))
      break
    } 
    echo(iter)
    iter <- iter + 1
    echo(iter)
    
  }
  # writing outputs 
  echo(paste("Final_best_score", scoreBest, sep = ","))
  # decision writing
  pxlsTemp %>% 
    mutate(density = dph) %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(outputSqlite, remove=F,layer=paste0(dph,"_final"),delete_layer=T)
  
  sink()
}

# Setting initial parameters ----------------------------------------------
pphh <- 2.6 # person per household
pop <- 60000 # total population
mutation_p <- 0.10 # mutate rate for optimization
iters_max <- 5 # max number of iterations
convergenceIterations <- 5
share_land_for_dest <- 0.35 # share of land for dest
share_land_for_resid <- 0.7 # share of land for residential
pxl_d <- 0.025 # pixel diameter
nbhd_d <- 1.6 # neighbourhood diameter
consider_categories <- T 
densities <- seq(from = 35, to = 45, by = 10) # dwelling per hectare
# Setting up folders ------------------------------------------------------
output_dir <- "../outputs/Exp3_withDiversity_Mar21/" # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)

output_deci_dir <- paste0(output_dir,"decisions") # CHANGE THIS FOR DIFFERENT RUNS
ifelse(!dir.exists(output_deci_dir), dir.create(output_deci_dir), FALSE)

total_scores_file <- paste(output_dir, "score_summary.csv", sep = "")
total_score_df <- data.frame(density = densities)

outputSqlite <- "pxls_march21_2143.sqlite"

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
