
getScore <- function(pxlsDf, destList){
  #pxlsDf <- pxlsInitial
  tp <- pxlsDf %>% 
    group_by(destID) %>% 
    filter(destID!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    dplyr::select(starts_with("pop_total")) %>% 
    sum()
  
  tpr <- pxlsDf %>% 
    group_by(destID) %>% 
    filter(destID!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    select(starts_with("pop_remaining")) %>% 
    sum()
  
  tpr <- ifelse(tpr==0,1,tpr)
  
  no <- pxlsDf %>% 
    filter(destID!="NA") %>% 
    distinct(destID) %>% 
    nrow()
  
  deci_score <- ifelse(tp>0,100*tpr/tp,0)
  
  totalUnsrvdPop <-0
  for (dest in destList$dest_code) {
    totalUnsrvdPop <- get_unsrvd_pop(pxlsDf, dest) + totalUnsrvdPop
  }
  scoreTemp <- 100*(totalUnsrvdPop/(pop*no)) +  deci_score
  if(is.na(scoreTemp)) scoreTemp=200
  return(scoreTemp)
  
}

getScore2 <- function(pxlsDf, destList){
  #pxlsDf <- pxlsInitial
  #dest <- destList$destCode[1]
  scoreTemp <- 0
  for (dest in destList$destCode) {
    tp <- pxlsDf %>% 
      filter(destID!="NA") %>% 
      filter(type==dest) %>% 
      distinct(destID, .keep_all = T) %>% 
      dplyr::select(paste0("pop_total_",dest)) %>% 
      sum()
    
    tpr <- pxlsDf %>% 
      filter(destID!="NA") %>% 
      filter(type==dest) %>% 
      distinct(destID, .keep_all = T) %>% 
      dplyr::select(paste0("pop_remaining_",dest)) %>% 
      sum()
    
    tpr <- ifelse(tpr==0,1,tpr)
    
    no_dw <- pxlsDf %>% 
      filter(destID!="NA") %>% 
      filter(type==dest) %>% 
      left_join(destList[,c("destCode","dest_weight")], by=c("type"="destCode")) %>% 
      distinct(destID, .keep_all = T) %>% 
      group_by(type) %>% 
      summarise(no=n(), dw=dest_weight)%>%
      distinct(type, .keep_all = T) %>% 
      transmute(no_dw=no*dw)
    
    scoreTemp <- scoreTemp + ((tp/tpr)+1)*no_dw$no_dw*(-1)
  }
  if(is.na(scoreTemp)) scoreTemp=nrow(pxlsDf)*-1
  return(scoreTemp)
}