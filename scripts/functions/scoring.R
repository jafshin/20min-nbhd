
getScore <- function(cells_df, dest_list){
  #cells_df <- pxlsInitial
  tp <- cells_df %>% 
    group_by(dest_id) %>% 
    filter(dest_id!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    dplyr::select(starts_with("pop_total")) %>% 
    sum()
  
  tpr <- cells_df %>% 
    group_by(dest_id) %>% 
    filter(dest_id!="NA") %>% 
    slice_sample(n=1) %>% 
    ungroup() %>% 
    select(starts_with("pop_remaining")) %>% 
    sum()
  
  tpr <- ifelse(tpr==0,1,tpr)
  
  no <- cells_df %>% 
    filter(dest_id!="NA") %>% 
    distinct(dest_id) %>% 
    nrow()
  
  deci_score <- ifelse(tp>0,100*tpr/tp,0)
  
  total_unserved_pop <-0
  for (dest in dest_list$dest_code) {
    total_unserved_pop <- get_unsrvd_pop(cells_df, dest) + total_unserved_pop
  }
  score_temp <- 100*(total_unserved_pop/(pop*no)) +  deci_score
  if(is.na(score_temp)) score_temp=200
  return(score_temp)
  
}

getScore2 <- function(cells_df, dest_list){
  #cells_df <- pxlsInitial
  score_temp <- 0
  #dest <- dest_list$dest_code[1]
  # dest <- "cc"
  for (dest in dest_list$dest_code) {
    tp <- cells_df %>% 
      filter(dest_id!="NA") %>% 
      filter(type==dest) %>% 
      distinct(dest_id, .keep_all = T) %>% 
      dplyr::select(paste0("pop_total_",dest)) %>% 
      sum()
    
    tpr <- cells_df %>% 
      filter(dest_id!="NA") %>% 
      filter(type==dest) %>% 
      distinct(dest_id, .keep_all = T) %>% 
      dplyr::select(paste0("pop_remaining_",dest)) %>% 
      sum()
    
    tpr <- ifelse(tpr==0,1,tpr)
    
    no_dw <- cells_df %>% 
      filter(dest_id!="NA") %>% 
      filter(type==dest) %>% 
      left_join(dest_list[,c("dest_code","dest_weight")], by=c("type"="dest_code")) %>% 
      distinct(dest_id, .keep_all = T) %>% 
      group_by(type) %>% 
      summarise(no=n(), dw=dest_weight)%>%
      distinct(type, .keep_all = T) %>% 
      transmute(no_dw=no*dw)
    
    score_temp <- score_temp + ((tp/tpr)+1)*no_dw$no_dw*(-1)
  }
  if(is.na(score_temp)) score_temp=nrow(cells_df)*-1
  return(score_temp)
}