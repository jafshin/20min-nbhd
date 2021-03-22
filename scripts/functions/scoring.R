
getScore <- function(pxlsTemp, destList){
  tp <- pxlsTemp %>% 
    group_by(destID) %>% 
    filter(destID!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    dplyr::select(starts_with("pop_total")) %>% 
    sum()
  
  tpr <- pxlsTemp %>% 
    group_by(destID) %>% 
    filter(destID!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    select(starts_with("pop_remaining")) %>% 
    sum()
  
  no <- pxlsTemp %>% 
    filter(destID!="NA") %>% 
    distinct(destID) %>% 
    nrow()
  
  deci_score <- ifelse(tp>0,100*tpr/tp,0)
  
  totalUnsrvdPop <-0
  for (dest in destList$dest_code) {
    totalUnsrvdPop <- get_unsrvd_pop(pxlsTemp, dest) + totalUnsrvdPop
  }
  
  scoreTemp <- 100*(totalUnsrvdPop/(pop*no)) +  deci_score
  if(is.na(scoreTemp)) scoreTemp=200
  
  echo(paste("Iteration_Score,",scoreTemp, sep = ","))
  echo(paste("Iteration_result,",scoreTemp, sep = ","))
  
  return(scoreTemp)
  
}