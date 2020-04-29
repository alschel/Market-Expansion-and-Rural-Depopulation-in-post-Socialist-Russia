# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Helpers

library(dplyr)
library(sf)

# =====================================
# 1. Coordinate reference systems (crs) 
# =====================================

# epsg:28412
pulkovo1942.GK12 <- "+proj=tmerc +lat_0=0 +lon_0=69 +k=1 +x_0=12500000 +y_0=0 +ellps=krass +units=m +no_defs"
# epsg:4326
WGS84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
AlbersSiberia <- "+proj=aea +lat_1=52 +lat_2=64 +lat_0=0 +lon_0=105 +x_0=18500000 +y_0=0 +ellps=krass +units=m +towgs84=28,-130,-95,0,0,0,0 +no_defs"

# Save as Rdata file
save(pulkovo1942.GK12, WGS84, AlbersSiberia, file = "data/crs.Rdata")

# =================================================
# 2. Data frames to add labels for cities in ggplot
# =================================================

load("data/settlements.Rdata")

settlements <- settlements %>% 
  mutate(lon = st_coordinates(settlements)[,1],
         lat = st_coordinates(settlements)[,2])

# Create data frames for city labels
cities_labels <- tibble(x = rep(c(settlements %>% filter(ShortName == "г. Ишим") %>% pull(lon) + 120000,
                                  settlements %>% filter(ShortName == "г. Тобольск") %>% pull(lon) + 80000,
                                  settlements %>% filter(ShortName == "г. Тюмень") %>% pull(lon) - 5000),2),
                        y = rep(c(settlements %>% filter(ShortName == "г. Ишим") %>% pull(lat) + 5000,
                                  settlements %>% filter(ShortName == "г. Тобольск") %>% pull(lat) + 5000,
                                  settlements %>% filter(ShortName == "г. Тюмень") %>% pull(lat) + 100000),2),
                        label = rep(c('Ishim','Tobolsk','Tyumen'),2))

cities_curves <- 
  tibble(x=cities_labels$x,
         xend=rep(settlements %>% filter(ShortName %in% c("г. Тюмень", "г. Тобольск", "г. Ишим")) %>% pull(lon), 2),
         y=cities_labels$y - 5000, 
         yend=rep(settlements %>% filter(ShortName %in% c("г. Тюмень", "г. Тобольск", "г. Ишим")) %>% pull(lat), 2))

# Save as Rdata file
save(cities_labels, cities_curves, file = "data/cities_labels.Rdata")

