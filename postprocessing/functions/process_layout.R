# This is a code that reads model output sqlite files and generates the table below
# destination|level|20min_access_threshold|expected_no|no_15dph|uc_15dph|
# usp_15dph|cost_15dph|...|no_45dph|uc_45dph|usp_45dph|cost_45dph|
# no= number opened; uc= unused capacity (%); usp= un-serviced population (%)

process_layout <- function(output_dir,dph){
  # output_dir <- "./outputs/exp4/"
  # dph <- 35
  # cells <- read_sf(paste0(output_dir,"pxls_dph35_run1.sqlite"),
  #                  layer="35_final") %>%
  #   st_drop_geometry()
  # 
  cells <- read_sf(paste0(output_dir,"pxls_dph",dph,".sqlite"),
                   layer=paste0(dph,"_final")) %>% 
    st_drop_geometry()  
  
  cells_unique <- cells %>% 
    distinct(dest_id, .keep_all = T)
  
  destinations_df <- cells_unique %>% count(type) %>% 
    filter(type!="resid") %>% 
    rename(!!paste0("no_",dph,"dph"):=n)
  
  for (i in 1:nrow(destinations_df)) {
    
    dt <- destinations_df$type[i]
    
    num_dest_col <- paste0("num_dest_",dt)
    pop_remaining_col <- paste0("pop_remaining_",dt)
    pop_total_col <- paste0("pop_total_",dt)
    not_served_col <- paste0("not_served_by_",dt)
    
    # un-used capacity = remaining population capacity / total population capacity
    uc <- sum(cells_unique[,pop_remaining_col])/sum(cells_unique[,pop_total_col])
    
    # un-served population = sum unserved population / total population
    usp <- sum(cells[,not_served_col] )/sum(cells$pxl_pop)
    
    destinations_df[i,paste0("uc_",dph,"dph")] <- uc * 100 
    destinations_df[i,paste0("usp_",dph,"dph")] <- usp * 100 
  }
  
  return(destinations_df)
  
}








