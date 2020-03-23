
distribute_population <- function(px_pop, init_nbhd, init_pixls, total_pop, init_dest){
  # Adding total_pop to neighbourhoods
  
  remaining_population <- total_pop
  for (nb in 1:nrow(init_nbhd)) {
    my_pixls <- which(init_pixls$nbhd == nb)
    for (px in my_pixls){
      init_pixls[px, "pop"] <- min(px_pop, remaining_population)
      remaining_population <- remaining_population - init_pixls$pop[px]
      for(iter_dest_type in init_dest$dest_type_id){
        init_pixls[px, paste("pop_not_served_by_dest_", iter_dest_type, sep = "")] <- init_pixls$pop[px]
      }
    }
  }
  return(init_pixls)
}