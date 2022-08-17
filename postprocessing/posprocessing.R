
library(sf)
library(tidyverse)

source("./postprocessing/functions/process_layout.R")


destinations <- read_csv("./inputs/destinations_v8.csv") %>% 
  select(id, type=dest_code, name, coverage)

# Experiment three
destinations_exp3 <- destinations
output_dir <- "./outputs/Aug2022/Exp3_01Aug22_1827/"
#dph <- 15

for (dph in seq(15, 45, 5)) {
  df <- process_layout(output_dir,dph)
  destinations_exp3 <- destinations_exp3 %>% 
    left_join(df, by = "type")
}

# Experiment four
destinations_exp4 <- destinations
output_dir <- "./outputs/Aug2022/Exp4_01Aug22_1827/"
#dph <- 15

for (dph in seq(15, 45, 5)) {
  df <- process_layout(output_dir,dph)
  destinations_exp4 <- destinations_exp4 %>% 
    left_join(df, by = "type")
}

source("./models/functions/land_updating.R")

stwo35 <- read_sf("./outputs/Aug2022/Exp3_01Aug22_1827/pxls_dph35.sqlite", layer="35_step2") %>% 
  st_drop_geometry()
get_unsrvd_pop(stwo35, dest_code = "occ")

60000*(1-destinations$coverage[13])

sthree35 <- read_sf("./outputs/Aug2022/Exp3_01Aug22_1827/pxls_dph35.sqlite", layer="35_step3") %>% 
  st_drop_geometry()

get_unsrvd_pop(sthree35, dest_code = "occ")

sfinal35 <- read_sf("./outputs/Aug2022/Exp3_01Aug22_1827/pxls_dph35.sqlite", layer="35_final") %>% 
  st_drop_geometry()
get_unsrvd_pop(sfinal35, dest_code = "occ")

sfinal35$not_served_by_occ %>% sum()

sfinal35$pxl_pop %>% sum()

