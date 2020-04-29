# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Figure 5. Population change in individual settlements plotted as a function of distance to regional capital.

library(dplyr)
library(ggplot2)

load("data/population_dynamics.Rdata")

# Y-axis labels
pop.labels <- seq(0, 200, 25)
pop.labels[-seq(1, 9, 2)] <- ''

# Text annotation
cities_labels <- tibble(Period = c(rep("1990-2002", 3), rep("2002-2010", 3)),
                            x = rep(c(population_dynamics %>% 
                                        filter(ShortName == "г. Тюмень", Period == "1990-2002") %>% 
                                        pull(Dist2Tyumen)/1000,
                                      population_dynamics %>% 
                                        filter(ShortName == "г. Тобольск", Period == "1990-2002") %>% 
                                        pull(Dist2Tyumen)/1000,
                                      population_dynamics %>% 
                                        filter(ShortName == "г. Ишим", Period == "1990-2002") %>% 
                                        pull(Dist2Tyumen)/1000),2),
                            y = rep(c(160, 145, 145),2),
                            label = rep(c('Tyumen','Tobolsk','Ishim'),2))

rayon_centres <- tibble(Rayon = unique(population_dynamics$MunicipalDistrict),
                            Centre = c("с. Абатское", "с. Армизонское", "с. Аромашево",
                                       "с. Бердюжье", "с. Вагай", "с. Викулово",
                                       "пгт. Голышманово", "г. Заводоуковск", "с. Исетское",
                                       "г. Ишим", "с. Казанское", "с. Нижняя Тавда", 
                                       "с. Омутинское", "с. Сладково", "с. Большое Сорокино",
                                       "г. Тобольск", "г. Тюмень", "с. Уват", 
                                       "с. Упорово", "с. Юргинское", "г. Ялуторовск",
                                       "с. Ярково"))


pal <- c("#E69F00", "#19518a")

Fig5 <- 
  population_dynamics %>%
  filter(res_pop_to_init_pop < 200) %>%                                # remove outliers
  filter(Dist2Tyumen < 510000) %>%                                     # remove outliers
  filter(!(res_pop_to_init_pop == 0 & MunicipalDistrict == "Тюменский район")) %>%
  filter(!(res_pop_to_init_pop == 0 & MunicipalDistrict == "Тобольский район")) %>%
  ggplot(aes(x = Dist2Tyumen/1000, y = res_pop_to_init_pop))+
  geom_point(aes(size = init_pop, fill = Period), 
             shape = 21, stroke = 0, alpha = 0.4)+
  stat_smooth(method = "loess", aes(col = Period),
              lwd = 0.8, alpha = 1, se = F, span = 0.2)+
  geom_point(data = population_dynamics %>% 
               filter(ShortName %in% rayon_centres$Centre), 
             mapping = aes(size = init_pop, color = Period), alpha = 1, fill = "white",
             shape = 21, stroke = 0.7, show.legend = T)+
  geom_point(data = population_dynamics %>% 
               filter(res_pop_to_init_pop == 0) %>% filter(Dist2Tyumen < 510000),
             aes(size = init_pop, fill = Period), 
             shape = 21, stroke = 0, color = "black", alpha = 0.4, show.legend = F)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_size_continuous(name = "Initial population, '000",
                        breaks = rev(c(0, 100, 1000, 5000, 20000, 100000)), 
                        labels = rev(c("< 0.1", "0.1-1", "1-5", "5-20", "20-100", ">100")), 
                        range = c(0.9, 10), 
                        guide = guide_legend(title.position = "top"))+
  scale_x_continuous(name = "Distance to Tyumen, km", 
                     breaks = seq(0, 500, 50),
                     minor_breaks = seq(0 , 500, 50))+
  scale_y_continuous(name = "% of initial population at end of period",
                     breaks = seq(0, 200, 25), labels = pop.labels)+
  scale_color_manual(values = pal,
                     name = "", guide = guide_legend(nrow = 2))+
  scale_fill_manual(values = pal)+
  geom_text(data = cities_labels, 
            aes(x=x,y=y,label=label),
            color = "black",
            size=3.2, hjust=0.5, vjust=0)+
  geom_curve(data = data.frame(x=cities_labels$x,
                               xend=cities_labels$x,
                               y=cities_labels$y - 5, 
                               yend=cities_labels$y - 30),
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm")))+
  theme_bw(base_size = 13, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.1,0.1,2,0.1),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 13),
        legend.box = "horizontal")+
    guides(fill = FALSE)

# Save the plot
ggsave(plot = Fig5, filename = "Figure 5.jpeg", 
       device = "jpeg", path = "plots/", 
       dpi = 300, width = 18, height = 13, units = "cm")

ggsave(plot = Fig5, filename = "Figure 5.eps", path = "plots/", 
       width = 18, height = 13, units = "cm", device = cairo_ps)
