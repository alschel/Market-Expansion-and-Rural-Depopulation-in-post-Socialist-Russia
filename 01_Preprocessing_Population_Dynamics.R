# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Calculate population changes between time steps and label them

library(dplyr)
library(sf)

load("data/settlements.Rdata")

# First period
population_dynamics <- 
  rbind(settlements %>% 
          mutate(Period = "1990-2002") %>% 
          mutate(lon = st_coordinates(.)[,1],
                 lat = st_coordinates(.)[,2],
                 init_pop = Rosstat1990,
                 res_pop = Census2002) %>% 
          filter(init_pop != 0 | res_pop != 0) %>% 
          mutate(res_pop_to_init_pop = Census2002 / Rosstat1990 * 100,
                 label = case_when(res_pop_to_init_pop == 0 ~ "abolished",
                                   res_pop_to_init_pop < 75 ~ "< -25%",
                                   res_pop_to_init_pop <= 100 ~ "-25% - 0",
                                   res_pop_to_init_pop <= 125 ~ "0 - 25%",
                                   T ~ "> 25%") %>% 
                   factor(levels = c("> 25%", "0 - 25%", "-25% - 0", "< -25%", "abolished"),
                          labels = c("> 25%", "0 - 25%", "-25% - 0", "< -25%", "abandoned"))) %>% 
          dplyr::select(c("id", "lon", "lat", "ShortName", "MunicipalDistrict", 
                   "Period", "init_pop", "res_pop", "res_pop_to_init_pop","label")),
  # Second period
        settlements %>%
          mutate(Period = "2002-2010") %>% 
          mutate(lon = st_coordinates(.)[,1],
                 lat = st_coordinates(.)[,2],
                 init_pop = Census2002,
                 res_pop = Census2010) %>% 
          filter(init_pop != 0 | res_pop != 0) %>% 
          mutate(res_pop_to_init_pop = Census2010 / Census2002 * 100,
                 label = case_when(res_pop_to_init_pop == 0 ~ "abolished",
                                   res_pop_to_init_pop < 75 ~ "< -25%",
                                   res_pop_to_init_pop <= 100 ~ "-25% - 0",
                                   res_pop_to_init_pop <= 125 ~ "0 - 25%",
                                   T ~ "> 25%") %>% 
                   factor(levels = c("> 25%", "0 - 25%", "-25% - 0", "< -25%", "abolished"),
                          labels = c("> 25%", "0 - 25%", "-25% - 0", "< -25%", "abandoned"))) %>% 
          dplyr::select(c("id", "lon", "lat", "ShortName", "MunicipalDistrict", 
                          "Period", "init_pop", "res_pop", "res_pop_to_init_pop","label"))) %>% 
  mutate(Period = factor(Period, levels = c("1990-2002", "2002-2010")))

# Sae as Rdata file
save(population_dynamics, file = "data/population_dynamics.Rdata")

