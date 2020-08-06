# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Figure 1. Ecozones (Olson et al. 2001), rivers, settlements, and major transport routes in the study area.

library(sp)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)

# =================================
# 1. Reading data and preprocessing
# =================================

load("data/crs.Rdata")
load("data/settlements.Rdata")
load("data/regional_border.Rdata")

# 1.1. Russian regional borders (based on OSM data)
russia <- st_read("data/Case region/Russia_regions/Russia_regions.shp") %>%
  st_simplify(preserveTopology = T, dTolerance = 1000) %>%  # Simplify polygons geometry
  st_transform(AlbersSiberia)                               # Transform from WGS84 to AlbersRussia

# 1.2. Ecoregions
# WWWF terrestrial ecoregions (Olson et al. 2001)
# Source: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
load("data/wwf_ecoregions.Rdata")

# Change labels of the ecoregions names
ecoregions %>%
  mutate(ECO_NAME = factor(ECO_NAME, levels = levels(ECO_NAME), 
                           labels = c("Taiga", "Hemiboreal forests", "Forest steppe"))) -> 
  ecoregions

# 1.3. Settlements
# Add two columns with coordinates
df <- settlements %>% 
  mutate(lon = st_coordinates(settlements)[,1],
         lat = st_coordinates(settlements)[,2])
  
# 1.4. Rivers, roads and railway
# Read and reproject
rivers <- st_read("data/Case region/Main_rivers/Main_rivers.shp") %>% 
  st_transform(crs = pulkovo1942.GK12)
roads <- st_read("data/Case region/Main_roads/Main_roads.shp") %>% 
  st_transform(crs = pulkovo1942.GK12)
railRoads <- st_read("data/Case region/RailWays_lines/RailWays_lines.shp") %>% 
  st_transform(crs = pulkovo1942.GK12) %>% 
  select(ogc_fid) %>% 
  st_simplify(preserveTopology = T, dTolerance = 1000)

# Remove the redundant attributes and merge roads and railways into single sf
transport_lines <- rbind(roads %>% mutate(type = "Major roads") %>% select(id, geometry, type),
      railRoads %>% mutate(id = ogc_fid, type = "Railway") %>% select(id, geometry, type))
transport_lines %>% 
  mutate(type = factor(type, levels = c("Major roads", "Railway"))) -> transport_lines

# ================
# 2. Separate maps
# ================

# 2.1. Context map (Tyumen oblast among Russian regions)

# Plot
russia_plot <- ggplot()+
  geom_sf(data = russia, fill = "white", col = "black", lwd = 0.3)+
  geom_sf(data = russia[which(russia$name == "Tyumen Oblast"),], 
          col = "black", fill = "red", lwd = 0.3)+
  scale_x_continuous(name = element_blank(), expand=c(0,0))+
  scale_y_continuous(name = element_blank(), expand=c(0,0))+
  coord_sf(crs = AlbersSiberia, datum = NA)+
  theme_void()

# 2.2. Main map

# Cities labels
load("data/cities_labels.Rdata")

rayon_centres <- tibble(Rayon = unique(df$MunicipalDistrict),
                        Centre = c("с. Абатское", "с. Армизонское", "с. Аромашево",
                                   "с. Бердюжье", "с. Вагай", "с. Викулово",
                                   "пгт. Голышманово", "г. Заводоуковск", "с. Исетское",
                                   "г. Ишим", "с. Казанское", "с. Нижняя Тавда", 
                                   "с. Омутинское", "с. Сладково", "с. Большое Сорокино",
                                   "г. Тобольск", "г. Тюмень", "с. Уват", 
                                   "с. Упорово", "с. Юргинское", "г. Ялуторовск",
                                   "с. Ярково"))

# Add two columns to provide ggplot with shape and size
df %>% 
  mutate(shape = case_when(ShortName %in% rayon_centres$Centre & Census2010 > 3000 ~ "cities and rayon centres", 
                           T ~ "settlements") %>% 
           factor(levels = c("cities and rayon centres", "settlements"),
                  labels = c("Cities and rayon centres", "Settlements")),
         Census2010 = Census2010/1000,
         size = case_when(Census2010 <= 1 ~ 1,
                          Census2010 <= 5 ~ 5,
                          Census2010 <= 20 ~ 20,
                          Census2010 <= 100 ~ 100,
                          T ~ 500)) -> df
# Plot
case_region_plot <-
  ggplot()+
  geom_sf(data = ecoregions, aes(fill = ECO_NAME), col = "black", lwd = 0, alpha = 1)+
  scale_fill_manual(values = c("#688B8A",  "#A0B084", "#FAEFD4"))+
  geom_sf(data = rivers, col = "steelblue1", lwd = 0.5, show.legend = F)+
  geom_sf(data = transport_lines %>% filter(!is.na(id)), aes(col = type), lwd = 0.6, show.legend = "line")+
  geom_point(data = df,
             aes(x = lon, y = lat, shape = shape, size = Census2010),
             col = "black", stroke = 0.4, show.legend = T, fill = "white")+
  geom_text(data = cities_labels,
            aes(x=x,y=y,label=label),
            color = "black", fontface = "italic",
            size=3.5, hjust=0.5, vjust=0, 
            show.legend = F)+
  geom_curve(data = cities_curves,
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm")),
             show.legend = F)+
  scale_color_manual(values = c("black", "#FE0000"))+
  scale_shape_manual(values = c(21, 19))+
  scale_size_continuous(name = "Population, 1000 (2010)",                        
                        breaks = c(1, 5, 20, 100, 500),
                        range = c(0.4, 10))+
  scale_x_continuous(expand = c(0.1, 0.1))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  ggsn::scalebar(region, dist = 50, transform = FALSE, dist_unit = "km",
                 location = "bottomleft",
                 height = 0.01, st.size = 3.5, border.size = 0.2)+
  theme_minimal(base_family = "Helvetica", base_size = 12)+
  theme(axis.title = element_blank(),
        legend.title = element_text(size = 10),
        legend.position = c(0.8, 0.32),
        legend.spacing = unit(x = 0, units = "cm"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(x = 0, units = "cm"),
        legend.key.height = unit(0.48, "cm"),
        legend.text = element_text(margin = margin(t = 0))
  )+
  guides(fill = guide_legend(title = element_blank(),
                             direction = "vertical",
                             label.position = "right",
                             frame.colour = "black",
                             default.unit="cm",
                             barwidth = 5,
                             barheight = 0.7,
                             override.aes = list(linetype = rep("blank", 3), shape = NA),
                             order = 1),
         shape = guide_legend(title = element_blank(),
                              order = 2, override.aes = list(linetype = rep("blank", 2))),
         size = guide_legend(override.aes = list(linetype = rep("blank", 5)), title.position = "top",
                             direction = "horizontal", label.position = "bottom", keyheight = 0,
                             order = 3),
         color = guide_legend(title = element_blank(),
                              override.aes = list(linetype = c("solid", "solid"), shape = NA),
                              nrow=2, byrow=TRUE, order = 4)
         )

# ===============
# 3. Combine maps
# ===============
par(mar=c(0,0,0,0))

Fig1 <- ggplot()+
  coord_equal(xlim = c(1, 10), ylim = c(0, 10))+
  annotation_custom(ggplotGrob(case_region_plot),
                    xmin = 0, xmax = 12, ymin = 0, ymax = 10)+
  annotation_custom(ggplotGrob(russia_plot),
                    xmin = 0, xmax = 3.5, ymin = 7.5, ymax = 9.5)+
  scale_x_continuous(name = element_blank(), expand=c(0,0))+
  scale_y_continuous(name = element_blank(), expand=c(0,0))+
  theme_void()+
  theme(axis.title=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"))

# Save the plot
ggsave(plot = Fig1, filename = "Figure 1.jpeg", path = "plots/",
       dpi = 300, width = 18, height = 15, units = "cm")

ggsave(plot = Fig1, filename = "Figure 1.eps", path = "plots/",
       width = 18, height = 15, units = "cm", device = "eps")
