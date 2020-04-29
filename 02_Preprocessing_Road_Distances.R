# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Calculate road distances from each settlement to Tyumen and nearest rayon center

library(sp)
library(sf)
library(rgdal)
library(dplyr)
library(shp2graph)
library(igraph)

# =================================================
# Part 1. Distance matrices based on OSM road graph
# =================================================

# Create distance matrices between all the settlements
# Road data source: OpenStreetMap (loaded 11 August 2018), 
# highway = trunk, primary, secondary, tertiary, unclassified. 
# Tags' description: https://wiki.openstreetmap.org/wiki/Key:highway
# The topology was cleaned in GrassGIS.

# Algorithm:
# 1) split settlements layer into 2 objects according to periods
# 2) read OSM data
# 3) integrate SpatialPoints* object (settlements) and SpatialLines* object (roads), using 
# shp2graph functions. Create graphs for 1990 and 2002 years
# 4) calculate distance matrices for 1990 and 2002

# 1.1. Split settlements layers 
load("data/settlements.Rdata")

# Split data into two objects, convert to Spatial*. Keep the settlements with no zero population only
settlements %>% filter(Rosstat1990 != 0 | Census2002 != 0) %>% as('Spatial') -> settlements_1990
settlements %>% filter(Census2002 != 0 | Census2010 != 0) %>% as('Spatial') -> settlements_2002

# 1.2. Create graphs

# Read OSM data
roads <- readOGR("data/OSM_roads/roads_fixed.shp")
# Check the topology
nt.connect(roads) # 1 self-connected parts, which means the topology is OK

# Integrate settlements and roads geometries
# Method of integration is nearest point (2), so every settlement will be provided with 
# the link to the nearest point on road graph (creates new nodes)

# 1.2.1. 1990 год
# Integrate settlements to graph
roads_points_temp <- points2network(ntdata = roads,                            # roads layer
                                    pointsxy = coordinates(settlements_1990),  # points ccordinates
                                    approach = 2,                              # Mapping each point to the nearest point (add them as nodes if they are not) on the network
                                    ELComputed = T,                            # calculate and return the length of each edge
                                    longlat = FALSE,
                                    ea.prop = c(0,1,1,0,0,0,0))                # from the edge attributes keep only id and type      

# Convert to igraph object
res_graph_1990 <- nel2igraph(roads_points_temp[[1]],           # nodelist
                             roads_points_temp[[2]],           # edgelist
                             weight = roads_points_temp[[8]])  # the length of edges (from @ea.prop)

# Save node indexes, accotiated with settlements (we need them later)
settl_index_1990 <- roads_points_temp[[3]]

# 1.2.2. 2002 год
# Integrate settlements to graph
roads_points_temp <- points2network(ntdata = roads,                            # roads layer
                                    pointsxy = coordinates(settlements_2002),  # points ccordinates
                                    ELComputed = T,                            # calculate and return the length of each edge
                                    approach = 2,
                                    longlat = FALSE,
                                    ea.prop = c(0,1,1,0,0,0,0))                # keep edge attributes
# Convert to igraph object
res_graph_2002 <- nel2igraph(roads_points_temp[[1]],           # nodelist
                             roads_points_temp[[2]],           # edgelist
                             weight = roads_points_temp[[8]])  # the length of edges (from @ea.prop)

# Save node indexes, accotiated with settlements
settl_index_2002 <- roads_points_temp[[3]]

# 1.3. Calculate distance matrices

# 1990
dist_matrix_1990 <- 
  shortest.paths(res_graph_1990,                                        # igraph object
                 v = settl_index_1990,                                  # from
                 to = as.numeric(levels(as.factor(settl_index_1990))))  # to: use the same indexes, but ordered
# Bring the matrix to standart form (diagonal axis = 0)
dist_matrix_1990 <- dist_matrix_1990[, match(settl_index_1990, as.numeric(levels(as.factor(settl_index_1990))))]

# Check it
dist_matrix_1990[1:10, 1:10]

# 2002
dist_matrix_2002 <- 
  shortest.paths(res_graph_2002,                                        # igraph object
                 v = settl_index_2002,                                  # from
                 to = as.numeric(levels(as.factor(settl_index_2002))))  # to: use the same indexes, but ordered

# Bring the matrix to standart form (diagonal axis = 0)
dist_matrix_2002 <- dist_matrix_2002[, match(settl_index_2002, as.numeric(levels(as.factor(settl_index_2002))))]

# =======================================================
# Part 2. Calculate distances to Tyumen and rayon centres
# =======================================================

load("data/population_dynamics.Rdata")

# 2.1. Distance to cities (Tyumen and one of three nearest centres (Tyumen, Tobolsk or Ishim))

# Filter distance matrices by destination (cities)
dist_matrix_1990[, c(which(population_dynamics %>%
                             filter(Period == "1990-2002") %>%
                             pull(ShortName) == "г. Тюмень"),
                     which(population_dynamics %>%
                             filter(Period == "1990-2002") %>%
                             pull(ShortName) == "г. Тобольск"),
                     which(population_dynamics %>%
                             filter(Period == "1990-2002") %>%
                             pull(ShortName) == "г. Ишим"))] -> dist_matrix_1990_2cities

dist_matrix_2002[, c(which(population_dynamics %>%
                             filter(Period == "2002-2010") %>%
                             pull(ShortName) == "г. Тюмень"),
                     which(population_dynamics %>%
                             filter(Period == "2002-2010") %>%
                             pull(ShortName) == "г. Тобольск"),
                     which(population_dynamics %>%
                             filter(Period == "2002-2010") %>%
                             pull(ShortName) == "г. Ишим"))] -> dist_matrix_2002_2cities

# Add distance to Tyumen to data
population_dynamics %>%
  mutate(Dist2Tyumen = c(dist_matrix_1990_2cities[,1], dist_matrix_2002_2cities[,1])) -> 
  population_dynamics

# Which urban center is the nearest?
population_dynamics %>% 
  mutate(nearest_center = c(apply(dist_matrix_1990_2cities, 1, function(x) which(x == min(x))),
                            apply(dist_matrix_2002_2cities, 1, function(x) which(x == min(x))))) %>%
  mutate(nearest_center = recode(nearest_center,
                                 "1" = "Tyumen",
                                 "2" = "Tobolsk",
                                 "3" = "Ishim")) -> population_dynamics

# Distance to the nearest urban center
population_dynamics %>% 
  mutate(nearest_center_dist = c(apply(dist_matrix_1990_2cities, 1, min),
                                 apply(dist_matrix_2002_2cities, 1, min))) ->
  population_dynamics



# 2.2. Distance to rayon centres

# 2.2.1. Create a temp data frame with rayons and its centres' names
rayon_centres <- tibble(Rayon = unique(population_dynamics$MunicipalDistrict),
                        Centre = c("с. Абатское", "с. Армизонское", "с. Аромашево",
                                   "с. Бердюжье", "с. Вагай", "с. Викулово",
                                   "с. Голышманово", "г. Заводоуковск", "с. Исетское",
                                   "г. Ишим", "с. Казанское", "с. Нижняя Тавда", 
                                   "с. Омутинское", "с. Сладково", "с. Большое Сорокино",
                                   "г. Тобольск", "г. Тюмень", "с. Уват", 
                                   "с. Упорово", "с. Юргинское", "г. Ялуторовск",
                                   "с. Ярково"))

# 2.1.2. Calculate distance to the rayon center
population_dynamics$dist2rayon_center <- NA_real_

# 1990-2002
for(i in 1:nrow(rayon_centres)) {
  # Subset distance matrix by rayon
  settlements_idx <- population_dynamics[population_dynamics$Period == "1990-2002", ]$MunicipalDistrict == rayon_centres$Rayon[i]
  dist_matrix_1990[settlements_idx, settlements_idx] -> temp_matrix
  # Rayon center id
  ray_center_id <- population_dynamics %>% 
    filter(Period == "1990-2002", MunicipalDistrict == rayon_centres$Rayon[i]) %>% 
    pull(ShortName) == rayon_centres$Centre[i]
  # Assign distances to rayon center to variable
  temp_matrix[, ray_center_id] ->
    population_dynamics[population_dynamics$Period == "1990-2002" & population_dynamics$MunicipalDistrict == rayon_centres$Rayon[i], ]$dist2rayon_center
}

# 2002-2010
for(i in 1:nrow(rayon_centres)) {
  # Subset distance matrix by rayon
  settlements_idx <- population_dynamics[population_dynamics$Period == "2002-2010", ]$MunicipalDistrict == rayon_centres$Rayon[i]
  dist_matrix_2002[settlements_idx, settlements_idx] -> temp_matrix
  # Rayon center id
  ray_center_id <- population_dynamics %>% 
    filter(Period == "2002-2010", MunicipalDistrict == rayon_centres$Rayon[i]) %>% 
    pull(ShortName) == rayon_centres$Centre[i]
  # Assign distances to rayon center to variable
  temp_matrix[, ray_center_id] ->
    population_dynamics[population_dynamics$Period == "2002-2010" & population_dynamics$MunicipalDistrict == rayon_centres$Rayon[i], ]$dist2rayon_center
}

# Save the results into Rdata file
save(population_dynamics, file = "data/population_dynamics.Rdata")
