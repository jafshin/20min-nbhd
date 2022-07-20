
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(pxlsTemp, destCode){
  unsrvd <- pxlsTemp %>% 
    dplyr::select(paste("not_served_by_", destCode, sep = "")) %>% 
    sum()
  return(unsrvd)
} 

findSpace <- function(pxlsTemp,destCode, cellsToOccupy,destLvl){
  
  if(destLvl==1) pxlsTempGrpd <- pxlsTemp %>% group_by(nbhdQ)
  if(destLvl==2) pxlsTempGrpd <- pxlsTemp %>% group_by(NBHD_ID)
  if(destLvl==3) pxlsTempGrpd <- pxlsTemp 
  
  feasibleArea <- pxlsTempGrpd %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(area_catchment_potential>0) %>% 
    arrange(desc(area_catchment_potential)) 
  
  if(nrow(feasibleArea)==0){
    echo("increasing destination space to 50%")
    feasibleArea <- pxlsTempGrpd %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.5*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }
  
  # if(nrow(feasibleNbhdsQuarters)==0){
  #   echo("increasing destination space to 75%")
  #   
  #   feasibleNbhdsQuarters <- pxlsTemp %>% 
  #     mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
  #     group_by(nbhdQ) %>% 
  #     summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
  #               cells=n(),
  #               cellsWithDest=sum(wt)) %>% 
  #     filter(cellsToOccupy+cellsWithDest<0.75*cells) %>% # Making sure there is enough space
  #     filter(catchment_potential>0) 
  # }
  
  hasSpace <- T
  if (nrow(feasibleArea)==0){
    discardRunFlag <<- T 
    hasSpace <- F
    echo("Not enough Space - skipping")
  }  
  return(hasSpace)
}

# Feasible Location Finder ------------------------------------------------
findDestinationCells <- function(pxlsTemp,destList,cellsToOccupy,
                                 destCode,pxl_a,destLvl,destRadius){
  
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  #pxlsTemp <-pxlsInitial
  
  if(destLvl==1) pxlsTempGrpd <- pxlsTemp %>% group_by(nbhdQ)
  if(destLvl==2) pxlsTempGrpd <- pxlsTemp %>% group_by(NBHD_ID)
  if(destLvl==3) pxlsTempGrpd <- pxlsTemp 
  
  feasibleArea <- pxlsTempGrpd %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(area_catchment_potential>0) %>% 
    arrange(desc(area_catchment_potential)) 
  
  if(nrow(feasibleArea)==0){
    echo("increasing destination space to 50%")
    feasibleArea <- pxlsTempGrpd %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.5*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) %>% 
      arrange(desc(area_catchment_potential)) 
  }
  
  destPositionPref <- destList[which(destList$destCode==destCode),"position"]
  
  # Finding the best area based on the level 
  
  areasWithPreferedDest<- pxlsTempGrpd %>% 
    filter(type==destPositionPref) %>% 
    summarise() %>% 
    unlist()
  
  if(destLvl==1) {
    feasibleArea <- feasibleArea %>% filter(nbhdQ%in%areasWithPreferedDest)
    # Selecting the best area based on the level
    newDestArea <- feasibleArea %>%
      filter(area_catchment_potential == max(area_catchment_potential)) %>%
      dplyr::select(nbhdQ) %>% 
      sample_n(size = 1) %>% 
      as.character()
    cellsInArea <- pxlsTemp %>% filter(nbhdQ==newDestArea)
    }
  if(destLvl==2){
    feasibleArea <- feasibleArea %>% filter(NBHD_ID%in%areasWithPreferedDest)
    # Selecting the best area based on the level     
    newDestArea <- feasibleArea %>%
      filter(area_catchment_potential == max(area_catchment_potential)) %>%
      dplyr::select(NBHD_ID) %>% 
      sample_n(size = 1) %>% 
      as.character()
    cellsInArea <- pxlsTemp %>% 
      filter(NBHD_ID==newDestArea)
    }
  if(destLvl==3){ cellsInArea <- pxlsTemp }
  
  # Select 10 cell centre matching the co-location preference
  if(destPositionPref=="resid"){
  sampleCells <- cellsInArea %>% 
    filter(type=="resid") %>% 
    sample_n(10) 
  }else{ 
    sampleCells <- cellsInArea %>% 
      filter(type==destPositionPref) %>% 
      sample_n(min(10,n())) 
  }
  
  # Selecting the best place for the dest within the area from a sample of 10
  bestCatchment <- 0
  #i <- 1
  for(i in 1:nrow(sampleCells)){
    x <- sampleCells[i,"pxl_x"]
    y <- sampleCells[i,"pxl_y"]
    # echo(paste(x,y))
    
    cellsWithinBB <- pxlsTemp %>% 
      # Selecting cells within the bounding box
    filter(pxl_x <= x+(0.8+destRadius) & pxl_x >= x-(0.8+destRadius) & 
             pxl_y >= y-(0.8+destRadius) & pxl_y <= y+(0.8+destRadius)
    )  
    
    # Select the closest ones for            
    cellsWithinBB <- cellsWithinBB %>%            
      mutate(dist = sqrt((pxl_x-x)^2 + (pxl_y-y)^2)) %>% 
      arrange(dist)
    
    catchment20min <- cellsWithinBB %>% 
      filter(dist>=destRadius & dist<=(0.8+destRadius) ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]])) %>% 
      unlist()

    if(catchment20min > bestCatchment){
      cellsSelected <- cellsWithinBB %>% 
        filter(type==destPositionPref) %>% 
        slice_head(n=cellsToOccupy)
      
      bestCatchment <- catchment20min
    }
  }
  return(cellsSelected$ID)
}
