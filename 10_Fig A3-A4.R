# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Appendix. Figure A3. Agricultural dynamics in Tyumen province in 1990-2010.
# Appendix. Figure A4. Structural changes in cattle farming in Tyumen province in 1990-2010.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data (based on Rosstat estimates)
cattle <- readr::read_csv("data/Agriculture/Cattle_structure.csv")
sown_area_cattle <- readr::read_csv("data/Agriculture/Sown_area_cattle.csv")

# Figure A3. Agricultural dynamics in Tyumen province in 1990-2010.

years.labels <- 1990:2010
years.labels[-seq(1,25,2)] <- ""

FigA3 <-
  sown_area_cattle %>% 
  group_by(Indicator) %>% 
  mutate(Value = Value/Value[Year == 1990]*100) %>% # calculate ratio
  ggplot(aes(x = Year, y = Value, linetype = Indicator))+
  geom_line(lwd = 0.7)+
  geom_point(size=2, shape=21, fill="white", show.legend = F)+
  scale_x_continuous(breaks = 1990:2010, labels = years.labels)+
  scale_y_continuous(name = "% from 1990", limits = c(0,100),
                     breaks = seq(0, 110, 10))+
  scale_linetype(labels = c("Cattle", "Sown area"))+
  theme_bw(base_size = 14, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.12,0.13),
        plot.margin = margin(0,0,0,0, "cm"))

ggsave(plot = FigA3, filename = "Figure A3.jpeg",
       device = "jpeg", path = "plots/Appendix/",
       dpi = 300, width = 18, height = 10, units = "cm")
ggsave(plot = FigA3, filename = "Figure A3.eps", 
       path = "plots/Appendix/",
       width = 18, height = 10, units = "cm", device = "eps")

# Fig. A4. Structural changes in cattle farming in Tyumen oblast in 1990-2010
# Notes: “Corporate” refers to agricultural enterprises, “peasant” refers to family farms,
# and “household” refers to private small-scale farming.

FigA4 <- cattle %>% 
  ggplot(aes(x = Year, y = Value, linetype = FarmCategory))+
  geom_line(lwd = 0.7)+
  geom_point(size=2, shape=21, fill="white", show.legend = F)+
  scale_x_continuous(breaks = 1990:2010, labels = years.labels)+
  scale_y_continuous(name = "'000 heads", limits = c(0,700),
                     breaks = seq(0, 700, 100))+
  scale_linetype(labels = c("Corporate", "Household", "Peasant"))+
  theme_bw(base_size = 14, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_blank(),
        legend.position = c(0.86, 0.85),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.box = "horizontal", 
        plot.margin = margin(0,0,0,0, "cm"))

# Save the plot
ggsave(plot = FigA4, filename = "Figure A4.jpeg",
       device = "jpeg", path = "plots/Appendix/",
       dpi = 300, width = 18, height = 10, units = "cm")

ggsave(plot = FigA4, filename = "Figure A4.eps", 
       path = "plots/Appendix/",
       width = 18, height = 10, units = "cm", device = "eps")