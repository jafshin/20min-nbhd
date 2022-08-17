
library(sf)
library(tidyverse)

source("./postprocessing/functions/process_layout.R")


destinations <- read_csv("./inputs/destinations_v8.csv") %>% 
  select(id, type=dest_code, name, coverage)

# Experiment three
destinations_exp3 <- destinations
output_dir <- "./outputs/exp3/"
#dph <- 15

for (dph in seq(15, 45, 5)) {
  df <- process_layout(output_dir,dph)
  destinations_exp3 <- destinations_exp3 %>% 
    left_join(df, by = "type")
}

write_csv(destinations_exp3, "experiment3_summary.csv")

# Experiment four
destinations_exp4 <- destinations
output_dir <- "./outputs/exp4/"
#dph <- 15

for (dph in seq(15, 45, 5)) {
  df <- process_layout(output_dir,dph)
  destinations_exp4 <- destinations_exp4 %>% 
    left_join(df, by = "type")
}

write_csv(destinations_exp4, "experiment4_summary.csv")


