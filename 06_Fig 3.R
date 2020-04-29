# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Figure 3. Patterns of population change in Tyumen province.

library(sp)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Load data
load("data/crs.Rdata")
load("data/settlements.Rdata")
load("data/population_dynamics.Rdata")
load("data/cities_labels.Rdata")
load("data/regional_border.Rdata")

# Plot margin
par(mar = c(0,0,0,0))

# Define color palettes
fill_palette <- c("white", "#fee8c8", "#ABD9E9", "#2C7BB6", "black")
color_palette <- c("#ef0105","#ff8645", "#a3ddf1", "#1b7dc7", "black")

Fig3 <- 
  ggplot(population_dynamics %>% arrange(desc(label), desc(init_pop)))+
  geom_sf(data = st_as_sf(region), fill = "white")+
  geom_point(mapping = aes(x = lon, y = lat, color = label, fill = label, size = init_pop),
             shape = 21, show.legend = T, stroke = 0.35)+
  geom_text(data = cities_labels, 
            aes(x=x,y=y,label=label),
            color = "black", fontface = "italic",
            size=3.2, hjust=0.5, vjust=0)+
  geom_curve(data = cities_curves,
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm"))
             )+
  scale_size_continuous(name = "Initial population, '000",
                        breaks = rev(c(0, 1000, 5000, 20000, 100000)), 
                        labels = rev(c("< 1", "1-5", "5-20", "20-100", ">100")), 
                        range = c(0.3, 12), 
                        guide = guide_legend(nrow=2,byrow=TRUE, title.position = "top",
                                             keywidth=0.1,
                                             keyheight=0.1,
                                             default.unit="inch"))+
  scale_fill_manual(name = "Population change, %",
                    values = fill_palette,
                    labels = c(">25", "0-25", "-25-0", "<-25", "abandoned"),
                    guide = guide_legend(nrow=2,
                                         byrow=TRUE,
                                         title.position = "top",
                                         keywidth=0.1,
                                         keyheight=0.1,
                                         default.unit="inch", order = 1, 
                                         override.aes = list(color = color_palette)))+
  scale_color_manual(name = "Population change, %",
                     values = color_palette,
                     labels = c(">25", "0-25", "-25-0", "<-25", "abandoned"),
                     guide = F)+
  scale_x_continuous(name = element_blank(), expand=c(0,0))+
  scale_y_continuous(name = element_blank(), expand=c(0,0))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  theme_minimal(base_family = "Helvetica", base_size = 13)+
  theme(legend.position = "bottom",
        plot.margin = margin(0, 0, 0, 0, "cm"))+
  facet_grid(.~Period)

# Export as jpeg and eps files
ggsave(Fig3, filename = "Figure 3.jpeg", path = "plots/",
       dpi = 300, width = 18, height = 11.5, units = "cm")

ggsave(plot = Fig3, filename = "Figure 3.eps", path = "plots/", 
       width = 18, height = 11.5, units = "cm", device = "eps")
