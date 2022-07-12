
# Function to get number of unserved population per pixel/dest ------------
get_unsrvd_pop <- function(pxlsTemp, destCode){
  unsrvd <- pxlsTemp %>% 
    dplyr::select(paste("not_served_by_", destCode, sep = "")) %>% 
    sum()
  return(unsrvd)
} 

findSpace <- function(pxlsTemp,destCode, cellsToOccupy){
  feasibleNbhdsQuarters <- pxlsTemp %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    group_by(nbhdQ) %>% 
    summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(catchment_potential>0) 
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 50%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.5*cells) %>% # Making sure there is enough space
      filter(catchment_potential>0) 
  }
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 75%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
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
                                 destCode,pxl_a){
  # a function to find feasible decision locations, we need this to limit the search space
  # The idea here is to for each location, to find a potential catchment
  # so it will limit the search space for the program
  # potential catchment is considered as the 20 min access
  #pxlsTemp <-pxlsInitial
  feasibleNbhdsQuarters <- pxlsTemp %>% 
    mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
    group_by(nbhdQ) %>% 
    summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
              cells=n(),
              cellsWithDest=sum(wt)) %>% 
    filter(cellsToOccupy+cellsWithDest<0.35*cells) %>% # Making sure there is enough space
    filter(catchment_potential>0) %>% 
    arrange(desc(catchment_potential))  
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 50%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.50*cells) %>% # Making sure there is enough space
      filter(catchment_potential>0) %>% 
      arrange(desc(catchment_potential))  
    
  }
  
  if(nrow(feasibleNbhdsQuarters)==0){
    echo("increasing destination space to 75%")
    
    feasibleNbhdsQuarters <- pxlsTemp %>% 
      mutate(wt=ifelse(type=="resid",yes = 0,no=1)) %>% 
      group_by(nbhdQ) %>% 
      summarise(catchment_potential = sum(.data[[paste0("not_served_by_", destCode)]]),
                cells=n(),
                cellsWithDest=sum(wt)) %>% 
      filter(cellsToOccupy+cellsWithDest<0.75*cells) %>% # Making sure there is enough space
      filter(catchment_potential>0) %>% 
      arrange(desc(catchment_potential))  
    
  }
  
  destPositionPref <- destList[which(destList$destCode==destCode),"positionType"]
  destTypePref <- destList[which(destList$destCode==destCode),"position"]
  
  if(destPositionPref=="joined" | destPositionPref=="within"){
    nbhdsQuartersWithPreferedDest<- pxlsTemp %>% 
      filter(type==destTypePref) %>% 
      group_by(nbhdQ) %>% 
      summarise() %>% 
      unlist()
    if(any(nbhdsQuartersWithPreferedDest%in%feasibleNbhdsQuarters$nbhdQ)){
      feasibleNbhdsQuarters <- feasibleNbhdsQuarters %>% 
        filter(nbhdQ%in%nbhdsQuartersWithPreferedDest)
    } 
  }
  # echo("here3_1")
  # selecting one from max catchments by random
  # echo(length(feasibleNbhdsQuarters))
  newDestNbhd <- feasibleNbhdsQuarters %>%
    filter(catchment_potential == max(catchment_potential)) %>%
    dplyr::select(nbhdQ) %>% 
    sample_n(size = 1) %>% 
    as.character()
  # echo("here3_2")
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
  
  return(plxs4Dest$ID.1)
}