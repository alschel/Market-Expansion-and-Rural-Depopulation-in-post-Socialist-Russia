# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Figure 2. Population in Tyumen province, 1981-2018.

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data
regional_population <- read_csv("data/Regional_population.csv")

# Customize labels
year.labels <- 1981:2018
year.labels[-seq(0, 39, 5)] <- ''

# Plot
Fig2 <- regional_population %>% 
  gather(Type, Population, Urban:Rural) %>% 
  ggplot(aes(x = Year, y = Population, linetype = Type)) +
  geom_rect(data = NULL, aes(xmin = 1990, xmax = 2010,
                             ymin = -Inf, ymax = +Inf),
            fill="grey90", inherit.aes = F)+
  geom_line(lwd = 0.8)+
  scale_x_continuous(breaks = seq(1981, 2018, 1), labels=year.labels)+
  scale_y_continuous(name = "'000 people", limits = c(300, 1000), breaks = seq(300, 1000, 100))+
  scale_linetype(labels = c("Rural", "Urban"))+
  theme_bw(base_size = 14, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.1,0.88),
        legend.background = element_rect(fill="white", size=0.5),
        plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

# Export as jpeg and eps files
ggsave(Fig2, filename = "Figure 2.jpeg",path = "plots/",
       dpi = 300, width = 18, height = 10, units = "cm")

ggsave(plot = Fig2, filename = "Figure 2.eps", path = "plots/", 
                width = 18, height = 10, units = "cm", device = "eps")
