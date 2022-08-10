
get_score <- function(cells_df, dest_list, total_population){
  #cells_df <- cells
  
  cells_distinct <- cells_df %>% 
    filter(type!="resid") %>%
    distinct(dest_id, .keep_all = T) 
  
  # Capacity ----------------------------------------------------------------
  
  # Get the total added capacity per destination type
  # tc = total capacity
  capacity_df <- cells_distinct %>% 
    dplyr::select(type, dest_id, starts_with("pop_total")) %>% 
    pivot_longer(!c(type, dest_id), names_to = "dest", values_to = "capacity") %>% 
    group_by(type) %>% 
    summarise(capacity = sum(capacity)) %>% 
    mutate(capacity = ifelse(test = capacity == 0, yes = 1, no = capacity))
  
  
  # Get the total remaining capacity per destination type
  capacity_remaining_df  <- cells_distinct %>% 
    dplyr::select(type, dest_id, starts_with("pop_remaining")) %>% 
    pivot_longer(!c(type, dest_id), names_to = "dest", values_to = "capacity") %>% 
    group_by(type) %>% 
    summarise(capacity_remaining = sum(capacity)) %>% 
    mutate(capacity_remaining = ifelse(test = capacity_remaining == 0,
                                       yes  = 1,
                                       no   = capacity_remaining))
  
  destinations <- cells_distinct %>% 
    distinct(type) %>% 
    left_join(capacity_df, by = "type") %>% 
    left_join(capacity_remaining_df, by = "type") %>% 
    mutate(unused_capacity_pct = 100 * capacity_remaining / capacity) %>% 
    dplyr::select(-c(capacity_remaining, capacity))
  
  # Service -----------------------------------------------------------------
  
  for (i in 1:nrow(destinations)) {
    dt <- destinations$type[i]
    destinations[i, "unserved_population"] <- cells_df %>% 
      dplyr::select(paste0("not_served_by_", dt)) %>% 
      sum()
  }
  
  destinations <- destinations %>% 
    mutate(unserved_population_pct = 
             100 * unserved_population / total_population) %>% 
    dplyr::select(-unserved_population)
  
  # Total Score -------------------------------------------------------------
  
  score_temp <- destinations %>% dplyr::select(-type) %>% sum()
  
  if(is.na(score_temp)) score_temp=200*nrow(destinations)
  
  return(score_temp)
}


get_score2 <- function(cells_df, dest_list, total_population){
  # cells_df <- cells_initial_updated
  score_temp <- 0
  #dest <- dest_list$dest_code[1]
  # dest <- "cc"
  # tp : total population capacity
  # tpr: total remaining population capacity, i.e., unused capacity
  
  cells_distinct <- cells_df %>% 
    filter(type!="resid") %>%
    distinct(dest_id, .keep_all = T) 
  
  # Capacity ----------------------------------------------------------------
  
  # Get the total added capacity per destination type
  # tc = total capacity
  capacity_df <- cells_distinct %>% 
    dplyr::select(type, dest_id, starts_with("pop_total")) %>% 
    pivot_longer(!c(type, dest_id), names_to = "dest", values_to = "capacity") %>% 
    group_by(type) %>% 
    summarise(capacity = sum(capacity)) %>% 
    mutate(capacity = ifelse(test = capacity == 0, yes = 1, no = capacity))
  
  # Get the total remaining capacity per destination type
  capacity_remaining_df  <- cells_distinct %>% 
    dplyr::select(type, dest_id, starts_with("pop_remaining")) %>% 
    pivot_longer(!c(type, dest_id), names_to = "dest", values_to = "capacity") %>% 
    group_by(type) %>% 
    summarise(capacity_remaining = sum(capacity)) %>% 
    mutate(capacity_remaining = ifelse(test = capacity_remaining == 0,
                                       yes  = 1,
                                       no   = capacity_remaining))
  
  destinations <- cells_distinct %>% 
    distinct(type) %>% 
    left_join(capacity_df, by = "type") %>% 
    left_join(capacity_remaining_df, by = "type") %>% 
    mutate(unused_capacity_pct = 100 * capacity_remaining / capacity) %>% 
    dplyr::select(-c(capacity_remaining, capacity))
  
  # Service -----------------------------------------------------------------
  
  for (i in 1:nrow(destinations)) {
    dt <- destinations$type[i]
    destinations[i, "unserved_population"] <- cells_df %>% 
      dplyr::select(paste0("not_served_by_", dt)) %>% 
      sum()
  }
  
  destinations <- destinations %>% 
    mutate(unserved_population_pct = 
             100 * unserved_population / total_population) %>% 
    dplyr::select(-unserved_population)
  
  # Open destinations -------------------------------------------------------
  
  dest_list_eno <- dest_list %>% 
    mutate(expected_number = ceiling(total_population / pop_req)) %>% 
    dplyr::select(type=dest_code, expected_number)
  
  dest_list_no <- cells_distinct %>% 
    group_by(type) %>% 
    summarise(number_open=n()) 
  
  destinations <- destinations %>% 
    left_join(dest_list_eno, by = "type") %>% 
    left_join(dest_list_no, by = "type") %>% 
    mutate(destination_open_pct = 
             100*(number_open - expected_number) / expected_number ) %>% 
    dplyr::select(-c(expected_number, number_open))
  
  # Total Score -------------------------------------------------------------
  
  score_temp <- destinations %>% dplyr::select(-type) %>% sum()
  
  if(is.na(score_temp)) score_temp=400*nrow(destinations)
  
  return(score_temp)
}