
#### NEIGHBOURHOOD FACILTIY LOCATION OPTIMISATION #### 
# This is a code that optimizes a neighborhood destination locations to achieve 20 Min
# neighbourhood using an evolutionary algorithm
#
# @author AJ

# Packages ----------------------------------------------------------------
library(rdist)
library(dplyr)
options(dplyr.summarise.inform = FALSE)
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
# dph <- 15
optimise_nbhds <- function(dph) {
# Step 0: Setting up inputs and structure ---------------------------------
  echo(paste0("******************* DWELLING DENSITY: ", dph))
  output_sub_dir <-  paste0(output_dir, "Density_", dph, "/") # one sub-dir for each density
  #print(output_sub_dir)
  ifelse(!dir.exists(output_sub_dir), dir.create(output_sub_dir), FALSE) # create if not exists
  log_file <- paste0(output_sub_dir, "output_log_D", dph, ".txt")  # log file for keeping the record
  sink(log_file, append=FALSE, split=TRUE) # sink to both console and log file
  output_file <- paste0(output_sub_dir, "output_decision_D", dph, ".csv") # decision file indicating where final destination will be located
  echo(paste0("Total Population, ", pop))
  echo(paste0("Dweling Density, ", dph))
  echo(paste0("***********,", "***********"))
  #  Destinations
  dests <- read.csv("../inputs/destinations_v8.csv") 
  # How many of each destination needed
  dests <- dests %>% 
    mutate(num_dests=ceiling(pop/dests$pop_req))
# Step 1 Creating decision grid -------------------------------------------\
  echo("Step 1 starts - Creating decision grid")
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
  pixls <- make_pixels_df(pxl_d, pop, dph, 
                          pphh, study_area_d, nbhds) # creating pixles
  # Replace nbhd coordinates with its centroid X and Y
 
  #     mutate(position='all')  
  # } nbhds_geom <- nbhds[,"NBHD_ID"]
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
  
  # Calculating probability for cell population
  px_pop_avg <- pop / pxl_n
  px_pop_h <- ceiling(pop / pxl_n) 
  px_pop_l <- floor(pop / pxl_n) 
  prob_h <- (px_pop_avg-px_pop_l)/(px_pop_h-px_pop_l) # probability of population being the ceiling value 
  prob_l <- (1-prob_h) # probability of population being the floor value
  
  remaining_population <- pop 
  for (nb in nbhds$NBHD_ID) { # populating the pixles
    my_pixls <- which(pixls$NBHD_ID == nb)
    for (px in my_pixls){
      rnd_number <- runif(1, 0, 1)
      if(rnd_number<prob_h){
        pixls[px, "pxl_pop"] <- min(prob_h, remaining_population)
      }else{
        pixls[px, "pxl_pop"] <- min(px_pop_l, remaining_population)
        }
        remaining_population <- remaining_population - pixls$pxl_pop[px]
    }
  }
  pixls <- pixls %>% filter(pxl_pop > 0) %>% # Just keeping the pixels with pop
    mutate(type="resid", destID="NA")

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
  echo("Finished Creating decision grid (Step 1)")
  # Step 2: Initial layout  -------------------------------------------------
  echo("Starting Creating initial layout (Step 2)")
  nbhdsTemp <- nbhds # assigning iter specific variable nbhds
  destList <- dests # assigning iter specific variable destinations
  pxlsInitial <- pixls # assigning iter specific variable pixels
  No_Answer_flag <- FALSE # a flag for when no answer will be found
  clstsId <- 1
  
  # Loop over all destinations
  # destRow=1
  echo("Looping over all destinations and allocating initial locations")
  
  for(destRow in 1:nrow(destList)){
    destCode <- destList$destCode[destRow] # getting the dest type
    destLvl <- destList$lvl[destRow] # getting the dest level
    cellsToOccupy <- max(1,round(destList$land_req[destRow]/pxl_a))
    if (destCode=="cc") cellsToOccupy <- 2 
    destRadius <- max((pxl_d/2)+0.001,sqrt(destList$land_req[destRow]/pi))
    iter_dest_position <- destList$position[destRow] # getting the dest type
    
    # FIRST destination OF TYPE destRow is also going through the evolutionary process
    echo(paste("destination:",destCode,"; dwelling denisty:",dph,sep=" "))
    
    error_counter <- 0
    remaining_num_dests <- destList$num_dests[destRow]
    # Repeating the process until all neighborhoods are served
    while((remaining_num_dests > 0) & (get_unsrvd_pop(pxlsInitial,destCode) > pop*(1-destList$coverage[destRow]))){ # loop until we have destinations to use
      # CREATING A LIST OF DIFFERENT LOCATIONS AND THEIR POTENTIAL CATCHMENTS

      system.time(
        destCellsID <- findDestinationCells(pxlsInitial,destList,cellsToOccupy,
                                            destCode,pxl_a,destLvl,destRadius))
      destCellsRow <- which(pxlsInitial$ID%in%destCellsID)
      if(length(destCellsID)==0){ 
        echo("No Answer")
        No_Answer_flag <- TRUE
        break();
      }
      # Add destination to the the cell
      pxlsInitial$type[destCellsRow] <- destCode
      remaining_num_dests <- remaining_num_dests - 1
      pxlsInitial$destID[destCellsRow] <- paste0(destCode,"_",
                                             destList$num_dests[destRow]-remaining_num_dests)
      # Adding new capacity
      reminderPopCol <- paste0("pop_remaining_",destCode)
      totalPopCol <- paste0("pop_total_",destCode)
      numDestCol <- paste0("num_dest_",destCode)
      unsrvdPopCol <- paste0("not_served_by_",destCode)
      
      pxlsInitial[destCellsRow,totalPopCol] <- destList[destRow,"capacity"]
      pxlsInitial[destCellsRow,reminderPopCol] <- destList[destRow,"capacity"]
      pxlsInitial[destCellsRow,numDestCol] <- 1
      # Start serving people around
      destGeom <- pxlsInitial %>% 
        filter(ID%in%destCellsID) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- destGeom %>% 
        st_buffer(0.8) %>% 
        st_intersects(pxlsInitial %>% st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)) 
      
      cellsWithinDistID <- pxlsInitial[unlist(cellsWithinDistRow),] %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist2Dest) %>%
        dplyr::select(ID)
   
      if(!nrow(cellsWithinDistID) > 0) message("**Something is not right**")
      if(!nrow(cellsWithinDistID) > 0) stop()
      # Serving cells 
      system.time({
        remainingCap <- pxlsInitial[destCellsRow,reminderPopCol][1]
        closeCells <- pxlsInitial[which(pxlsInitial$ID %in% cellsWithinDistID$ID),]
        cell <- 1
        for(cell in 1:nrow(closeCells)) {
          # cellRow <- which(pxlsInitial$ID==id)
          cellUnsrvedPop <-  closeCells[cell,unsrvdPopCol]
          if (remainingCap > 0 & cellUnsrvedPop > 0){
            pop2cover <- min(remainingCap,cellUnsrvedPop) # finding population to cover
            # Updating the capapcity
            remainingCap <- remainingCap-pop2cover
            # Updating unserved pop
            closeCells[cell,unsrvdPopCol] <- cellUnsrvedPop - pop2cover
          }
        }
        pxlsInitial[which(pxlsInitial$ID %in% cellsWithinDistID$ID),] <- closeCells
        pxlsInitial[destCellsRow,reminderPopCol] <- remainingCap
      })
    }
  }
  pxlsInitial %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(outputSqlite, layer = paste0(dph,"_step2"), delete_layer=T)
  # In this step, population from pixels occupied with dests will be removed
  # And will be uniformly distributed in other locations
  # We will use random samples in order to redistribute
  echo("Finished Creating initial layout (Step 2)")
  
# Step 3: Updating demand distribution ------------------------------------
  echo("Starting Updating demand distribution (step3)")
  
  homelessPop <- pxlsInitial %>%
    filter(type!="resid") %>% 
    summarise(homelessPop = sum(pxl_pop)) %>% 
    unlist()
  # echo("here1-1")
  hostPxls <- pxlsInitial %>% 
    filter(type=="resid") %>% 
    filter(pxl_pop>0) %>% 
    dplyr::select(ID) %>%
    slice_sample(n=homelessPop,replace=T)
  # echo("here1-2")
  popServCols <- grep("not_",colnames(pxlsInitial))
  nonResids <- which(pxlsInitial$type!="resid" & pxlsInitial$pxl_pop>0)
  pxlsInitial[nonResids,"pxl_pop"] <- 0
  pxlsInitial[nonResids,popServCols] <- 0
  #i=3963
  for (i in 1:nrow(pxlsInitial)) {
    if(pxlsInitial[i,"ID"]%in%hostPxls$ID){
      pxlsInitial[i,"incomingPop"]=length(hostPxls[hostPxls==pxlsInitial[i,"ID"]])
      pxlsInitial[i,"pxl_pop"]=pxlsInitial[i,"pxl_pop"]+pxlsInitial[i,"incomingPop"]
      pxlsInitial[i,popServCols]=pxlsInitial[i,popServCols]+pxlsInitial[i,"incomingPop"]
    }
    else{
      pxlsInitial[i,"incomingPop"]=0
    }
  }
  pxlsInitial %>% 
    st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    st_write(outputSqlite, layer = paste0(dph,"_step3"), delete_layer=T)
  
  # Update the service -----------------------
  destsToUpdate <- pxlsInitial %>% 
    filter(type!="resid") %>% 
    group_by(type) %>% 
    summarise(areaPxls=n()) %>% 
    ungroup() %>% 
    as.data.frame()
  
  for (i in 1:nrow(destsToUpdate)){
    destToUpdateType <- destsToUpdate[i,"type"]
  
    # Setting col names
    totalPopCol <- paste0("pop_total_",destToUpdateType)
    reminderPopCol <- paste0("pop_remaining_",destToUpdateType)      
    unsrvdPopCol <- paste0("not_served_by_",destToUpdateType)
    numDestCol <- paste0("num_dest_",destToUpdateType)
    destListRow <- which(destList$destCode==destToUpdateType)
    
    # Un-serving all those for destToUpdateType
    residRowIds <- which(pxlsInitial$type=="resid")
    pxlsInitial[residRowIds,unsrvdPopCol] <- pxlsInitial[residRowIds,"pxl_pop"]
    
    # Returning all dests of this type to full cap
    destCapacity <- destList[destListRow,"capacity"]
    destTypeCellRows <- which(pxlsInitial$type==destToUpdateType)
    pxlsInitial[destTypeCellRows,totalPopCol] <- destCapacity
    pxlsInitial[destTypeCellRows,reminderPopCol] <- destCapacity
    
    # Iterating over all destinations of type destToUpdateType
    destTypeIDs <- pxlsInitial%>%filter(type==destToUpdateType)%>%distinct(destID) 
    # dest <- destTypeIDs$destID[1]
    echo(paste0("Revisiting service provided by: ",destToUpdateType," family"))
    for(dest in destTypeIDs$destID){
      destCellsRow <- which(pxlsInitial$destID==dest)
      destCellsID <- pxlsInitial[which(pxlsInitial$destID==dest),"ID"]
      
      # Start serving people around
      destGeom <- pxlsInitial %>% 
        filter(ID%in%destCellsID) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        st_union() %>% 
        st_convex_hull() 
      
      # Getting those within 20 min access    
      cellsWithinDistRow <- destGeom %>% 
        st_buffer(0.8) %>% 
        st_intersects(st_as_sf(pxlsInitial,coords=c("pxl_x","pxl_y"), remove=F)) 
      
      cellsWithinDistID <- pxlsInitial[unlist(cellsWithinDistRow),] %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
        dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
        st_drop_geometry() %>% 
        arrange(dist2Dest) %>%
        dplyr::select(ID)
      
      if(!nrow(cellsWithinDistID) > 0) echo("**Something is not right**")
      # Serving cells 
      system.time({
        remainingCap <- pxlsInitial[destCellsRow,reminderPopCol][1]
        closeCells <- pxlsInitial[which(pxlsInitial$ID %in% cellsWithinDistID$ID),]
        # cell <- 1
        for(cell in 1:nrow(closeCells)) {
          # cellRow <- which(pxlsInitial$ID==id)
          cellUnsrvedPop <-  closeCells[cell,unsrvdPopCol]
          if (remainingCap > 0 & cellUnsrvedPop > 0){
            pop2cover <- min(remainingCap,cellUnsrvedPop) # finding population to cover
            # Updating the capapcity
            remainingCap <- remainingCap-pop2cover
            # Updating unserved pop
            closeCells[cell,unsrvdPopCol] <- cellUnsrvedPop - pop2cover
          }
        }
        pxlsInitial[which(pxlsInitial$ID %in% cellsWithinDistID$ID),] <- closeCells
        pxlsInitial[destCellsRow,reminderPopCol] <- remainingCap
        
      })
    }
  }
  echo("Finished Updating demand distribution (step3)")
  # Step 4: Scoring ---------------------------------------------------------
  echo("Starting Scoring (step4)")
  
  if(No_Answer_flag){  
    scoreTemp <- nrow(dests)*(200)
  }else{
    source("./functions/scoring.R")
    scoreTemp <- getScore(pxlsInitial, destList) 
    pxlsBest <- pxlsInitial
    scoreBest <- scoreTemp
  }
  echo("Finished Scoring (step4)")
  
  echo(paste0("Initial layout score: ", scoreBest))
  
# Step5: Mutating Supply --------------------------------------------------
  echo("Starting Mutating Supply (step 5)")
  
### Here is where the While loop should start
  pxlsTemp <- pxlsInitial
  iter <- 1
  convergenceCounter <- 0
  while(iter < iters_max + 1){
    echo(paste0("Starting iteration: ", iter))
    
    # Select some dests to mutate:
    destsToUpdate <- pxlsTemp %>% 
      filter(destID!="NA") %>%  
      group_by(destID,type) %>% 
      summarise(areaPxls=n()) %>% 
      ungroup() %>% 
      slice_sample(prop=mutation_p) %>% 
      as.data.frame()
    if(nrow(destsToUpdate)==0) echo("Mutation skipped")
    if(nrow(destsToUpdate)!=0){
    # Changing the locations with free cells
    pxlsMutation <- pxlsTemp
    # i=12
    for (i in 1:nrow(destsToUpdate)){
      
      destToUpdateID <- destsToUpdate[i,"destID"]
      destToUpdateType <- destsToUpdate[i,"type"]
      pxlsToMove <- destsToUpdate[i,"areaPxls"]
      destToUpdateLvl <- destList[which(destList[,"destCode"]==destToUpdateType),"lvl"]
      destToUpdateRadius <- max((pxl_d/2)+0.001,sqrt(destList[which(destList[,"destCode"]==destToUpdateType),"land_req"]/pi))

      origCells <- which(pxlsMutation$destID==destToUpdateID)
      # Find where to move
      system.time(destCellsID <- findDestinationCells(pxlsMutation,destList,pxlsToMove,
                                                      destToUpdateType,pxl_a,
                                                      destToUpdateLvl,
                                                      destToUpdateRadius))
      if(length(destCellsID)==0){
        echo("No Answer")
        No_Answer_flag <- TRUE
        break();}
      destCellsRow <- which(pxlsMutation$ID%in%destCellsID)
      # Get population to move
      popToMove <- pxlsMutation %>% 
        filter(ID%in%destCellsRow) %>% 
        summarise(total=sum(pxl_pop)) %>% 
        as.numeric()
      
      # Setting col names
      totalPopCol <- paste0("pop_total_",destToUpdateType)
      reminderPopCol <- paste0("pop_remaining_",destToUpdateType)      
      unsrvdPopCol <- paste0("not_served_by_",destToUpdateType)
      numDestCol <- paste0("num_dest_",destToUpdateType)
      destListRow <- which(destList$destCode==destToUpdateType)

      # Adding destinations to the previously resids
      pxlsMutation[destCellsRow,"type"] <- destToUpdateType
      pxlsMutation[destCellsRow,"destID"] <- destToUpdateID
      pxlsMutation[destCellsRow, "pxl_pop"] <- 0 # not resid anymore
      capacityToAdd <- destList[destListRow,"capacity"]
      pxlsMutation[destCellsRow,totalPopCol] <- capacityToAdd
      pxlsMutation[destCellsRow,reminderPopCol] <- capacityToAdd
      pxlsMutation[destCellsRow,numDestCol] <- 1
      # Adding population to the previously dests
      pxlsMutation[origCells,"type"] <- "resid"
      pxlsMutation[origCells,"destID"] <- "NA"
      pxlsMutation[origCells, "pxl_pop"] <- floor(popToMove/length(origCells)) # using floor to not to add extra
      extraPop <- popToMove%%length(origCells) # get the reminder from the flow
      pxlsMutation[origCells[1:extraPop],"pxl_pop"]<-pxlsMutation[origCells[1:extraPop],
                                                              "pxl_pop"] +1  
      pxlsMutation[origCells, totalPopCol] <- 0
      pxlsMutation[origCells,reminderPopCol] <- 0
      pxlsMutation[destCellsRow, numDestCol] <- 0
      
      # Updating the serving part ---------------------------------------
      # Un-serving all those for destToUpdateType
      residRowIds <- which(pxlsMutation$type=="resid")
      pxlsMutation[residRowIds,unsrvdPopCol] <- pxlsMutation[residRowIds,"pxl_pop"]
      
      # Returning all dests of this type to full cap
      destCapacity <- destList[destListRow,"capacity"]
      destTypeCellRows <- which(pxlsMutation$type==destToUpdateType)
      pxlsMutation[destTypeCellRows,totalPopCol] <- destCapacity
      pxlsMutation[destTypeCellRows,reminderPopCol] <- destCapacity
      
      # Iterating over all destinations of type destToUpdateType
      destTypeIDs <- pxlsMutation%>%filter(type==destToUpdateType)%>%distinct(destID) 
      # dest <- destTypeIDs$destID[1]
      echo(paste0("Revisiting service provided by: ",destToUpdateType," family"))
      for(dest in destTypeIDs$destID){
        destCellsRow <- which(pxlsMutation$destID==dest)
        destCellsID <- pxlsMutation[which(pxlsMutation$destID==dest),"ID"]
        
        # Start serving people around
        destGeom <- pxlsMutation %>% 
          filter(ID%in%destCellsID) %>% 
          st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
          st_union() %>% 
          st_convex_hull() 
        
        # Getting those within 20 min access    
        cellsWithinDistRow <- destGeom %>% 
          st_buffer(0.8) %>% 
          st_intersects(st_as_sf(pxlsMutation,coords=c("pxl_x","pxl_y"), remove=F)) 
        
        cellsWithinDistID <- pxlsMutation[unlist(cellsWithinDistRow),] %>% 
          st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
          dplyr::mutate(dist2Dest=st_distance(.,destGeom)) %>% 
          st_drop_geometry() %>% 
          arrange(dist2Dest) %>%
          dplyr::select(ID)

        if(!nrow(cellsWithinDistID) > 0) echo("**Something is not right**")
        # Serving cells 
        system.time({
        remainingCap <- pxlsMutation[destCellsRow,reminderPopCol][1]
        closeCells <- pxlsMutation[which(pxlsMutation$ID %in% cellsWithinDistID$ID),]
        # cell <- 1
        for(cell in 1:nrow(closeCells)) {
          # cellRow <- which(pxlsMutation$ID==id)
          cellUnsrvedPop <-  closeCells[cell,unsrvdPopCol]
          if (remainingCap > 0 & cellUnsrvedPop > 0){
            pop2cover <- min(remainingCap,cellUnsrvedPop) # finding population to cover
            # Updating the capapcity
            remainingCap <- remainingCap-pop2cover
            # Updating unserved pop
            closeCells[cell,unsrvdPopCol] <- cellUnsrvedPop - pop2cover
          }
        }
        pxlsMutation[which(pxlsMutation$ID %in% cellsWithinDistID$ID),] <- closeCells
        pxlsMutation[destCellsRow,reminderPopCol] <- remainingCap
        
        })
      }
      
      # Evaluating the mutation
      scorePostMutation <- getScore(pxlsMutation, destList) 
      if (scorePostMutation < scoreTemp) {
        # Keeping the mutation result
        echo(paste0("Found a good mutation, destintation ID= ",destToUpdateID,
                    ", delta= ",(scorePostMutation-scoreTemp)))
        pxlsTemp <- pxlsMutation
        scoreTemp <- scorePostMutation
      }else{
        echo(paste0("Not a good mutation, destintation ID= ",destToUpdateID,
                    ", delta= ",(scorePostMutation-scoreTemp)))
        pxlsMutation <- pxlsTemp
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
      echo(paste0("Old best score: ",scoreBest))
      echo(paste0("New best score: ",scoreTemp))
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
    echo(paste0("Iteration: ", iter, "Finished, DPH: ",dph))
    iter <- iter + 1
    # pxlsTemp %>% 
    #   mutate(density = dph) %>% 
    #   st_as_sf(coords=c("pxl_x","pxl_y"), remove=F) %>% 
    #   st_write(outputSqlite, remove=F,layer=paste0(dph,"dph_iter_",iter),delete_layer=T)
    
    } # end of if 
  } # End of While loop
  
  # writing the final outputs 
  echo(paste0("Final best score: ", scoreBest))
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
iters_max <- 50 # max number of iterations
convergenceIterations <- 5
share_land_for_dest <- 0.15 # share of land for dest
share_land_for_resid <- 0.85 # share of land for residential
pxl_d <- 0.025 # pixel diameter
nbhd_d <- 1.6 # neighbourhood diameter
consider_categories <- T  
densities <- seq(from = 15, to = 45, by = 5) # dwelling per hectare
# Setting up folders ------------------------------------------------------
#dph <- 15
runs <- 10
expTime <- format(Sys.time(),"%d%b%y_%H%M")
for (dph in densities){
  #run <- 1
  for(run in 1:runs ){
    dir.create("../outputs/", showWarnings = FALSE)
    
    echo(paste0("Starting dph ", dph, " run ", run))

    output_dir <- paste0("../outputs/Exp3_",expTime)
    ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)
    
    output_dir <- paste0(output_dir,"/Dph",dph,"_Run",run,"/")
    ifelse(!dir.exists(output_dir), dir.create(output_dir), FALSE)
    
    output_deci_dir <- paste0(output_dir,"decisions") # CHANGE THIS FOR DIFFERENT RUNS
    ifelse(!dir.exists(output_deci_dir), dir.create(output_deci_dir), FALSE)
    
    
    total_scores_file <- paste0(output_dir, "score_summary_Run",run,".csv")
    total_score_df <- data.frame(density = densities)
    
    outputSqlite <- paste0(output_deci_dir,"/pxls_Run",run,".sqlite")
    
    optimise_nbhds(dph)
  }
  
  
}



#library(doParallel)  
#no_cores <- min(( detectCores() - 1 ),  length(densities))
#cl <- makeCluster(no_cores, type="FORK")  
#registerDoParallel(cl)  
#foreach(i=densities) %dopar% optimise_nbhds(i)
#parLapply(cl, densities, optimise_nbhds)  
#stopCluster(cl)  
