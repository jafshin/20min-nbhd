summerise_destinations <- function(densities, output_dir){
  dest_df <- read.csv("./inputs/destinations.csv")
  
  dest_df <- dest_df[,c(2:3)]
  
  dest_df_extended <- dest_df %>% slice(rep(1:n(), each = length(densities)))
  
  dest_df_extended$density <- rep(densities,nrow(dest_df))  
  
  for(den in densities){
    
    dest_density_rows <- which(dest_df_extended$density == den) 
    output_file <- paste(output_dir, "Density_", den, "/", "output_decision_D",den,".csv", sep = "")
    
    if(!file.exists(output_file)){
      warning(paste(output_file, " dose not exists!", sep = ""))
    }
    
    this_output <- read.csv(output_file, header = TRUE)
    
    dest_df_extended[dest_density_rows,c("num_open","unused_capacity")] <- this_output %>% 
      select(dest_type_id, num_open, remaining_pop_cap) %>%
      group_by(dest_type_id) %>%
      summarise(total_open = sum(num_open), unused_capacity = sum(remaining_pop_cap)) %>%
      select(total_open, unused_capacity)
  }
  write.csv(dest_df_extended,paste(output_dir, "decision_summary.csv", sep = ""))
}

plot_scores <- function(total_score_df){
  
  ggplot(data=total_score_df, aes(x=density, y=score)) +
    geom_line()+
    geom_point()
  
}

plot_destinations <- function(){
  dest_df<- read.csv(paste(output_dir,"decision_summary.csv", sep = ""))
  ggplot(data=dest_df, aes(x=as.character(dest_type_id), y=num_open, group = density)) +
    geom_line(aes(color=density))+
    geom_point(aes(color=density))
}
