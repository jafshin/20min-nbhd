
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(pxlsTemp, destCode){
  unsrvd <- pxlsTemp %>% 
    dplyr::select(paste("not_served_by_", destCode, sep = "")) %>% 
    sum()
  return(unsrvd)
} 

findSpace <- function(pxlsTemp,destCode, cellsToOccupy,lvl){
  

  
  
  
  
  feasibleNbhdsQuarters <- pxlsTemp %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    group_by(nbhdQ) %>% 
    summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(area_catchment_potential>0) 
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 50%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.5*cells) %>% # Making sure there is enough space
      filter(area_catchment_potential>0) 
  }
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 75%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(area_catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.75*cells) %>% # Making sure there is enough space
      filter(catchment_potential>0) 
  }
  
  hasSpace <- T
  if (nrow(feasibleNbhdsQuarters)==0){
    discardRunFlag <<- T 
    hasSpace <- F
    echo("Not enough Space - skipping")
  }  
  return(hasSpace)
}

# Feasible Location Finder ------------------------------------------------
findDestinationCells <- function(pxlsTemp,destList,cellsToOccupy,
                                 destCode,pxl_a,destLvl){
  
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  #pxlsTemp <-pxlsInitial
  
  if(destLvl==1){
    pxlsTempGrpd <- pxlsTemp %>% group_by(nbhdQ)
    
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

    areasWithPreferedDest<- pxlsTempGrpd %>% 
      filter(type==destPositionPref) %>% 
      summarise() %>% 
      unlist()
    
    feasibleArea <- feasibleArea %>% filter(nbhdQ%in%areasWithPreferedDest)
    
    newDestArea <- feasibleArea %>%
      filter(area_catchment_potential == max(area_catchment_potential)) %>%
      dplyr::select(nbhdQ) %>% 
      sample_n(size = 1) %>% 
      as.character()
    
    cellsInArea <- pxlsTemp %>% 
      filter(nbhdQ==newDestArea)  
    
    if(destPositionPref=="resid"){
    # Lets do 10 instead
    sampleCells <- cellsInArea %>% 
      filter(type=="resid") %>% 
      sample_n(10) 
    }else{
      sampleCells <- cellsInArea %>%
        filter(type==destPositionPref)
    }
    
    for(i in 1:nrow(sampleCells)){
      x <- sampleCells[i,"pxl_x"]
      y <- sampleCells[i,"pxl_y"]
      echo(paste(x,y))
      
      cellsInRadius <- sampleCells %>% 
        mutate(dist = )
      
    }
      
    
    
    
    
    
    
    centrePxl <- pxlsTemp %>% 
      filter(nbhdQ==newDestArea) %>% 
      filter(type=="resid") %>% 
      sample_n(1) %>% 
      st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)
      
    if(destPositionPref=="joined" | destPositionPref=="free"){
      # echo("here3_3")
      centrePxl <- pxlsTemp %>% 
        filter(nbhdQ==newDestNbhd) %>% 
        filter(type=="resid") %>% 
        sample_n(1) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)
      # echo("here3_4")
    }
    if(destPositionPref=="within"){
      # echo("here3_5")
      centrePxl <- pxlsTemp %>% 
        filter(nbhdQ==newDestNbhd) %>% 
        filter(type==destTypePref) %>% 
        sample_n(1) %>% 
        st_as_sf(coords=c("pxl_x","pxl_y"), remove=F)
      # echo("here3_6")
    }
    
    closePxls <- centrePxl %>% 
      st_buffer(0.4) %>% 
      st_intersection(pxlsTemp %>% 
                        filter(type=="resid") %>% 
                        st_as_sf(coords=c("pxl_x","pxl_y"))) %>% 
      mutate(dist2Pxl = st_distance(.,centrePxl)) %>% 
      arrange(dist2Pxl)
    
    plxs4Dest <- closePxls %>%  slice_head(n=cellsToOccupy)
    
    
    
  } 
  if(destLvl==2) pxlsTempGrpd <- pxlsTemp %>% group_by(NBHD_ID)
  if(destLvl==3) pxlsTempGrpd <- pxlsTemp 

  
 
  

  
  # destPositionPref <- destList[which(destList$destCode==destCode),"positionType"]
  

  
  # Feasible area
  

  if(destLvl== 2 & destTypePref!="all"){
    feasibleArea <- feasibleArea %>% filter(NBHD_ID%in%areasWithPreferedDest)
  }  

  # Feasible cells
  if(destLvl== 1) {
    
    
    #feasibleArea <- feasibleArea %>% filter(nbhdQ)
    
    
    }
  
  
  
  
  if(destLvl== 2) 
  if(destLvl== 3)
  # echo("here3_1")
  # selecting one from max catchments by random
  # echo(length(feasibleNbhdsQuarters))

  # echo("here3_2")
  
  if(destTypePref=="all"){
    
    # select 10 resid cells
    pxlsTemp %>% 
      filter(nbhdQ==newDestNbhd) %>% 
      filter(type=="resid") %>% 
      sample_n(10)
    
  }
  
  

  
  return(plxs4Dest$ID.1)
}