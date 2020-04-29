# Market Expansion and Rural Depopulation in post-Socialist Russia
# Alexander Sheludkov, 2020

# Modeling population changes in towns and villages as a function of distance to regional capital and rayon centers
# Appendix A. Figure A1. Actual and predicted values (RIRS model) against distance to Tyumen.
# Appendix A. Figure A2. Actual and predicted (RIRS model) values against distance to rayon centers.

library(dplyr)
library(car)
library(lme4)
library(lmerTest)
library(tidyr)
library(stargazer)
library(MuMIn)

load("data/population_dynamics.Rdata")

# ================
# 1. Preprocessing
# ================

# - remove infinite values (initial population equals zero and resulting population > 0, 16 observations)
# - remove settlements, absorbed by urban sprawl (false '-100%', 13 observations)
# - transform m to 10 km
# - log of initial population
# - remove outliers beyond 3 median absolute deviations
# - split into datasets by periods 1990-2002 and 2002-2010

population_dynamics %>%
  filter(!is.infinite(res_pop_to_init_pop)) %>% 
  filter(!(res_pop_to_init_pop == 0 & nearest_center_dist < 20000 & MunicipalDistrict == "Тобольский район")) %>%
  filter(!(res_pop_to_init_pop == 0 & nearest_center_dist < 20000 & MunicipalDistrict == "Тюменский район")) %>%
  dplyr::select(id, ShortName, MunicipalDistrict, Period, init_pop, res_pop, res_pop_to_init_pop, Dist2Tyumen, dist2rayon_center) %>%
  mutate(Dist2Tyumen = Dist2Tyumen/10000,
         dist2rayon_center = dist2rayon_center/10000,
         MunicipalDistrict = as.factor(MunicipalDistrict),
         logPop = log(init_pop))  -> df

df %>%
  filter(Period == "1990-2002") %>%
  mutate(dev = res_pop_to_init_pop - median(res_pop_to_init_pop)) %>% 
  filter(dev <= 3*mad(res_pop_to_init_pop)) %>% 
  dplyr::select(-dev) -> df1

df %>%
  filter(Period == "2002-2010") %>%
  mutate(dev = res_pop_to_init_pop - median(res_pop_to_init_pop)) %>% 
  filter(dev <= 3*mad(res_pop_to_init_pop)) %>% 
  dplyr::select(-dev) -> df2

# Check the correlation between variables
cor_df1 <- cor(df1 %>% select(res_pop_to_init_pop, logPop, Dist2Tyumen, dist2rayon_center))
cor_df2 <- cor(df2 %>% select(res_pop_to_init_pop, logPop, Dist2Tyumen, dist2rayon_center))
corrplot(cor_df1, method = "number", type = "full", insig = "p-value", col = viridis::inferno(12))
corrplot(cor_df2, method = "number", type = "full", insig = "p-value", col = viridis::inferno(12))

# There is a strong correlation between logPop and population changes in settlements (as expected). 
# The independent variables are not strongly correlated, 
# so we make sure the estimations would not be affected by multicollinearity.

# ===========
# 2. Modeling
# ===========

# 1990-2002

# Random intercept
model1 <- lmer(res_pop_to_init_pop ~ logPop + Dist2Tyumen + dist2rayon_center + (1|MunicipalDistrict), REML = FALSE, data = df1)
summary(model1)
# Random intercept + random slopes
model2 <- lmer(res_pop_to_init_pop ~ logPop + Dist2Tyumen + dist2rayon_center + (1 + Dist2Tyumen|MunicipalDistrict) + (1 + dist2rayon_center|MunicipalDistrict), REML = FALSE, data = df1)
summary(model2)

# 2002-2010

# Random intercept
model3 <- lmer(res_pop_to_init_pop ~ logPop + Dist2Tyumen + dist2rayon_center + (1|MunicipalDistrict), REML = FALSE, data = df2)
summary(model3)
# Random intercept + random slopes
model4 <- lmer(res_pop_to_init_pop ~ logPop + Dist2Tyumen + dist2rayon_center + (1 + Dist2Tyumen|MunicipalDistrict) + (1 + dist2rayon_center|MunicipalDistrict), REML = FALSE, data = df2)
summary(model4)

# =================================
# 3. Diagnostics and interpretation 
# =================================

# Change the class of models for stargazer to take them as arguments
class(model1) <- "lmerMod"
class(model2) <- "lmerMod"
class(model3) <- "lmerMod"
class(model4) <- "lmerMod"

# Compare the estimates and the fitness of the models
stargazer(model1, model2, model3, model4, type = "text",
          dep.var.labels   = "% of initial population at end of period",
          covariate.labels = c("logPop", "Distance to Tyumen (10 km)", "Distance to rayon centre (10 km)"),
          column.labels = c("1990-2002", "2002-2010"), column.separate = c(2, 2), 
          out = "model.txt")
# According to AIC, models 2 and 4 (with random slopes) fit the data better

# Calculate RMSD and R-squared
bind_cols(Model = 1:4,
          RMSD = c(sd(resid(model1)), sd(resid(model2)), sd(resid(model3)), sd(resid(model4))),
          R2m = c(r.squaredGLMM(model1)[1],
                  r.squaredGLMM(model2)[1],
                  r.squaredGLMM(model3)[1],
                  r.squaredGLMM(model4)[1]),
          R2c = c(r.squaredGLMM(model1)[2],
                  r.squaredGLMM(model2)[2],
                  r.squaredGLMM(model3)[2],
                  r.squaredGLMM(model4)[2]))

# R2m - Marginal R_GLMM², represents the variance explained by the fixed effects; 
# R2c - Conditional R_GLMM², is interpreted as a variance explained by the entire model, 
# including both fixed and random effects*


# =================================================
# 3. Visualization: actual values vs predicted ones 
# =================================================

# Appendix A. Figure A1. Actual and predicted values (RIRS model) against distance to Tyumen.

# Y-axis labels
pop.labels <- seq(0, 150, 25)
pop.labels[-seq(1, 9, 2)] <- ''

# Plot
# Notes: Actual values are marked with grey points. Predicted values for different rayons
# are marked with colored circles.
FigA1 <-
  bind_rows(df1 %>% mutate(predVal = predict(model2)),
            df2 %>% mutate(predVal = predict(model4))) %>%
  ggplot(aes(x = Dist2Tyumen*10, y = res_pop_to_init_pop))+
  geom_point(stroke = 0, color = "black", alpha = 0.5, size = 1)+
  geom_point(aes(y = predVal, col = MunicipalDistrict), shape = 21, stroke = 0.5, size = 0.8, alpha = 0.8)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "% of initial population at end of period",
                     limits = c(0, 155),
                     breaks = seq(0, 150, 25),
                     labels = pop.labels)+
  scale_x_continuous(name = "Distance to Tyumen, km",
                     limits = c(0, 510),
                     breaks = seq(0, 500, 50),
                     minor_breaks = seq(0 , 500, 50))+
  scale_color_viridis_d()+
  guides(color = FALSE)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal",
        strip.background = element_blank())+
  guides(fill = FALSE)+
  facet_grid(rows = vars(Period))

# Save the plot
ggsave(plot = FigA1, filename = "Figure A1.jpeg",
       device = "jpeg", path = "plots/Appendix/",
       dpi = 300, width = 18, height = 15, units = "cm")

ggsave(plot = FigA1, filename = "Figure A1.eps", 
       path = "plots/Appendix/",
       width = 18, height = 15, units = "cm", device = cairo_ps)

# ====================================================================
# 3.2. Predicted values (RIRS model) against distance to rayon centers

# Add predicted values to dfs, combine dfs, add english labels to Municpal Districts
df_predvals <- 
  bind_rows(df1 %>% mutate(predVal = predict(model2)),
            df2 %>% mutate(predVal = predict(model4))) %>%
  mutate(MunicipalDistrict = factor(MunicipalDistrict, 
                                    labels = c("Abatsky", "Armizonsky", "Aromashevsky", "Berdyuzhsky", 
                                               "Vagaysky", "Vikulovsky","Golyshmanovsky", "Zavodoukovsky",
                                               "Isetsky", "Ishimsky", "Kazansky", "Nizhnetavdinsky",
                                               "Omutinsky", "Sladkovsky", "Sorokinsly", "Tobolsky",
                                               "Tyumensky", "Uvatsky", "Uporovsky", "Yurginsky", 
                                               "Yalutorovsky", "Yarkovsky")))


# We make 3 plots with 8(6) rayons on each
FigA2_1 <- 
  df_predvals %>%
  filter(MunicipalDistrict %in% levels(MunicipalDistrict)[1:8]) %>%
  ggplot(aes(x = dist2rayon_center*10, y = res_pop_to_init_pop))+
  geom_point(stroke = 0, color = "black", alpha = 0.5, size = 1)+
  geom_point(aes(y = predVal, col = MunicipalDistrict), shape = 21, stroke = 0.4, size = 0.8)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "% of initial population at end of period",
                     limits = c(0, 155),
                     breaks = seq(0, 150, 25), labels = pop.labels)+
  scale_x_continuous(name = "Distance to rayon centre, km",
                     limits = c(0, 75),
                     breaks = seq(0, 75, 25))+
  scale_color_viridis_d()+
  guides(color = FALSE)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal",
        strip.background = element_blank())+
  guides(fill = FALSE)+
  facet_wrap(MunicipalDistrict ~ Period, as.table = T, ncol = 4)

FigA2_2 <- 
  df_predvals %>%
  filter(MunicipalDistrict %in% levels(MunicipalDistrict)[9:16]) %>%
  ggplot(aes(x = dist2rayon_center*10, y = res_pop_to_init_pop))+
  geom_point(stroke = 0, color = "black", alpha = 0.5, size = 1)+
  geom_point(aes(y = predVal, col = MunicipalDistrict), shape = 21, stroke = 0.4, size = 0.8)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "% of initial population at end of period",
                     limits = c(0, 155),
                     breaks = seq(0, 150, 25), labels = pop.labels)+
  scale_x_continuous(name = "Distance to rayon centre, km",
                     limits = c(0, 75),
                     breaks = seq(0, 75, 25))+
  scale_color_viridis_d()+
  guides(color = FALSE)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal",
        strip.background = element_blank())+
  guides(fill = FALSE)+
  facet_wrap(MunicipalDistrict ~ Period, as.table = T, ncol = 4)

FigA2_3 <- 
  df_predvals %>%
  filter(MunicipalDistrict %in% levels(MunicipalDistrict)[17:22]) %>%
  ggplot(aes(x = dist2rayon_center*10, y = res_pop_to_init_pop))+
  geom_point(stroke = 0, color = "black", alpha = 0.5, size = 1)+
  geom_point(aes(y = predVal, col = MunicipalDistrict), shape = 21, stroke = 0.4, size = 0.8)+
  geom_hline(aes(yintercept = 100), linetype = "dashed", col = "grey3")+
  scale_y_continuous(name = "% of initial population at end of period",
                     limits = c(0, 155),
                     breaks = seq(0, 150, 25), labels = pop.labels)+
  scale_x_continuous(name = "Distance to rayon centre, km",
                     limits = c(0, 75),
                     breaks = seq(0, 75, 25))+
  scale_color_viridis_d()+
  guides(color = FALSE)+
  theme_bw(base_size = 12, base_family = "Helvetica")+
  theme(panel.grid = element_blank(),
        axis.ticks = element_line(),
        plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm"),
        legend.position = c(0.45, -0.22),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12),
        legend.box = "horizontal",
        strip.background = element_blank())+
  guides(fill = FALSE)+
  facet_wrap(MunicipalDistrict ~ Period, as.table = T, ncol = 4)

# Export as jpeg and eps files
ggsave(plot = FigA2_1, filename = "Figure A2-1.jpeg", path = "plots/Appendix/",
       device = "jpeg", dpi = 300, width = 18, height = 23, units = "cm")
ggsave(plot = FigA2_1, filename = "Figure A2-1.eps", path = "plots/Appendix/",
       width = 18, height = 23, units = "cm", device = cairo_ps)
ggsave(plot = FigA2_2, filename = "Figure A2-2.jpeg", path = "plots/Appendix/",
       device = "jpeg",  dpi = 300, width = 18, height =23, units = "cm")
ggsave(plot = FigA2_2, filename = "Figure A2-2.eps", path = "plots/Appendix/",
       device = cairo_ps, width = 18, height =23, units = "cm")
ggsave(plot = FigA2_3, filename = "Figure A2-3.jpeg", path = "plots/Appendix/",
       device = "jpeg", dpi = 300, width = 18, height = 18, units = "cm")
ggsave(plot = FigA2_3, filename = "Figure A2-3.eps", path = "plots/Appendix/",
       device = cairo_ps, width = 18, height = 18, units = "cm")

