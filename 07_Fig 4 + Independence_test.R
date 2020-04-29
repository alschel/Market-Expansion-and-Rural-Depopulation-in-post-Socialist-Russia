# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Figure 4. The number of people (a) and the share of the working-age population (b) 
# involved in agrarian production in agricultural enterprises and family farms in 2006.
# +
# Test of Independence

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(sf)
library(coin)
library(viridis)

# ================
# 1. Preprocessing
# ================

# 1.1. Data on agricultural employment and working-age population in 2006

# Data sources: 
# Tyumenstat. 2008. Trudovye resursy i ikh kharakteristika v Tyumenskoi oblasti 
# [Labor Resources and Their Characteristics in the Tyumen Region]. 
# Part 2 of Itogi Vserossiiskoi sel'skokhozyaistvennoi perepisi 2006 goda 
# [Results of the 2006 All-Russian Agricultural Census]. Tyumen: Tyumenstat.
# Rosstats database on municipalities: https://www.gks.ru/dbscripts/munst/

# * For Zavodoukovsky rayon we count for rural population only

# Read data, filter 2006 and calculate ratio of jobs to working-age population in %
empl_work_age_pop <- 
  read.csv2("data/Agriculture/Agr_empl_&_work_age_pop_2006.csv", 
                  sep = ",", stringsAsFactors = F, encoding = "UTF-8") %>%
  as_tibble() %>% 
  mutate(Ratio_empl_to_wa_pop = Agricultural_employment / Working_age_population * 100)

# 1.2. Data on population in rayons

# Read, clean and transform to tidy data
population_rayons <- 
  read.csv("data/Population_rayons_1990_2019.csv", 
           stringsAsFactors = F, encoding = "UTF-8") %>% 
  as_tibble() %>% select(-X) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Year", 
               names_pattern = "([0-9]{4})", values_to = "Population") %>% 
  mutate(Year = as.integer(Year))

# Filter data and calculate changes from 2002 to 2010 in %
population_rayons %>%
  filter(Year %in% c(2002, 2010)) %>% 
  group_by(MunicipalDistrict_ru, MunicipalDistrict_en) %>%
  summarize(Population_change_2002_2010 = (Population[Year == 2010] - Population[Year == 2002])/Population[Year == 2002]*100) %>%
  ungroup() -> 
  pop_change

# 1.3. Combine into single dataset
data <- empl_work_age_pop %>%
  left_join(pop_change, by = "MunicipalDistrict_ru") %>% 
  select(MunicipalDistrict_en, MunicipalDistrict_ru, Year, Working_age_population, Agricultural_employment, 
         Ratio_empl_to_wa_pop, Population_change_2002_2010)
  
# 1.4. Combine with spatial data

load("data/crs.Rdata")
load("data/rayons_borders.Rdata")

data_sf <- rayons %>%
  select(MunicipalDistrict_en) %>%
  left_join(data, by = "MunicipalDistrict_en")

# =========================
# 2. Visualization (Fig. 4)
# =========================

load("data/cities_labels.Rdata")

# Absolute values
employment_absolute_plot <-
  ggplot()+
  geom_sf(data = data_sf,
          aes(fill = Agricultural_employment/1000), col = "black")+
  geom_text(data = cities_labels,
            aes(x=x,y=y,label=label),
            color = "black", fontface = "italic",
            size=3.5, hjust=0.5, vjust=0.2, 
            show.legend = F)+
  geom_curve(data = cities_curves,
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm")),
             show.legend = F)+
  scale_fill_viridis_c(option = "E", name = "'000 ",  begin = 0.05,
                       limits = c(0, 7.5), breaks = seq(0, 7.5, 1), 
                       na.value="white", guide = guide_colorbar(barheight = 10))+
  scale_x_continuous(name = element_blank(), expand=c(0,0))+
  scale_y_continuous(name = element_blank(), expand=c(0,0))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  ggtitle("a")+
  theme_minimal(base_family = "Helvetica", base_size = 13)+
  theme(legend.position = c(0.85, 0.25), plot.margin = unit(c(0,0,0,0), "cm"))

# Ratio to local working-age population
employment_ratio_plot <-
  ggplot()+
  geom_sf(data = data_sf,
          aes(fill = Ratio_empl_to_wa_pop), col = "black")+
  geom_text(data = cities_labels,
            aes(x=x,y=y,label=label),
            color = "black", fontface = "italic",
            size=3.5, hjust=0.5, vjust=0.2, 
            show.legend = F)+
  geom_curve(data = cities_curves,
             aes(x=x,y=y,xend=xend,yend=yend),
             color='black', size=.15, curvature = 0,
             arrow = arrow(type="closed", length = unit(0.1,"cm")),
             show.legend = F)+
  scale_fill_viridis_c(option = "E", name = "%",  begin = 0.05,
                       limits = c(0,21), breaks = seq(0, 21, 2.5),
                       na.value="white", guide = guide_colorbar(barheight = 10))+
  scale_x_continuous(name = element_blank(), expand=c(0,0))+
  scale_y_continuous(name = element_blank(), expand=c(0,0))+
  coord_sf(crs = pulkovo1942.GK12, datum = NA)+
  ggtitle("b")+
  theme_minimal(base_family = "Helvetica", base_size = 13)+
  theme(legend.position = c(0.85, 0.25), plot.margin = unit(c(0,0,0,0), "cm"))

Fig4 <- plot_grid(employment_absolute_plot, employment_ratio_plot)

ggsave(plot = Fig4, filename = "Figure 4.jpeg", path = "plots/",
       device = "jpeg", dpi = 300, width = 24, height = 12, units = "cm")

# Export to eps
ggsave(plot = Fig4, filename = "Figure 4.eps", path = "plots/",
       width = 24, height = 12, units = "cm", device = "eps")

# =======================
# 3. Test of independence
# =======================

set.seed(21)
spearman_test(Population_change_2002_2010 ~ Ratio_empl_to_wa_pop, data = data, 
              distribution=approximate(nresample=9999))
# Results:
# Approximative Spearman Correlation Test
# 
# Z = -1.167, p-value = 0.2566
# alternative hypothesis: true rho is not equal to 0

