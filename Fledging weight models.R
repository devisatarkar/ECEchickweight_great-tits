
#### FLEDGING WEIGHT MODELS ####

#---------- REQUIRED LIBRARIES ----------------

library(dplyr)
library(tidyr)
library(ggeffects)
library(ggplot2)
library(lme4)
library(lmerTest)
library(splines)

# ---------- PREPARE DATA ----------------

gtchicks_alt <- read.csv("/ENTERYOURDIRECTORY/chickweight_ECEdata_5perc1perc.csv", na.strings=c("", "NA"))

gtchicks_alt <- gtchicks_alt %>% filter(chickweight >2)
gtchicks_alt <- gtchicks_alt %>% filter(chickweight <25)
hist(gtchicks_alt$chickweight, breaks = 50, main = "Chick Weight Distribution", xlab = "Chick Weight")

summary(gtchicks_alt$chickweight)

gtchicks_alt <- gtchicks_alt[!is.na(gtchicks_alt$average_temperature_per1), ] #removing values with NA in hatchdate because we won't have climate information for it

gtchicks_altld <- gtchicks_alt[!is.na(gtchicks_alt$april_birthlaydate), ] #removing values with NA in laydate

gtchicks_alt <- gtchicks_alt %>%
  filter(substr(broodid, 5, 5) != '2') #removing second broods

#1965 onwards for letting tits acclimitise to new boxes provided in 1961

gtchicks_alt <- gtchicks_alt %>% filter(birthyear>1964)

gtchicks_alt <- gtchicks_alt %>% rename (natalclutch_size = "clutch_size", natalnum_chicks = "num_chicks")

gtchicks_alt <- gtchicks_alt[gtchicks_alt$id != "TS45000", ] #remove duplicate entry

# ---------- RUN MODELS ----------------

                    #########  AMBIENT CLIMATE AND FREQUENCY OF 5 % ECES  #########

#### -------------------------- average temperature --------------------------- ####

chickweight_meantemp_per1 <- lmer(chickweight ~ scale(average_temperature_per1) + scale(april_birthlaydate) +
                                    I(scale(average_temperature_per1)^2) + 
                                    scale(natalclutch_size) +
                                    (1 | birthyear) + (1 | broodid)
                                  + (1|mother) + (1|natalbox), 
                                  data = gtchicks_alt)
summary(chickweight_meantemp_per1)

chickweight_meantemp_per2 <- lmer(chickweight ~ scale(average_temperature_per2) + scale(april_birthlaydate) +
                                    I(scale(average_temperature_per2)^2) + 
                                    scale(natalclutch_size) +
                                    (1 | birthyear) + (1 | broodid)
                                  + (1|mother) + (1|natalbox), 
                                  data = gtchicks_alt)
summary(chickweight_meantemp_per2)

chickweight_meantemp_per1_spline <- lmer(chickweight ~ ns(scale(average_temperature_per1), df=5) + 
                                           scale(april_birthlaydate) +
                                           scale(natalclutch_size) +
                                           (1 | birthyear) + (1 | broodid)
                                         + (1|mother) + (1|natalbox), 
                                         data = gtchicks_alt)
summary(chickweight_meantemp_per1_spline)

chickweight_meantemp_per2_spline <- lmer(chickweight ~ ns(scale(average_temperature_per2), df=5) + 
                                           scale(april_birthlaydate) +
                                           scale(natalclutch_size) +
                                           (1 | birthyear) + (1 | broodid)
                                         + (1|mother) + (1|natalbox), 
                                         data = gtchicks_alt)
summary(chickweight_meantemp_per2_spline)

#predictions for plots

pred_meantemp_per1 <- ggpredict(chickweight_meantemp_per1, terms = c("average_temperature_per1 [all]"))
pred_meantemp_per2 <- ggpredict(chickweight_meantemp_per2, terms = c("average_temperature_per2 [all]"))

pred_meantemp_per1_spline <- ggpredict(chickweight_meantemp_per1_spline, terms = c("average_temperature_per1 [all]"))
pred_meantemp_per2_spline <- ggpredict(chickweight_meantemp_per2_spline, terms = c("average_temperature_per2 [all]"))

#plots for Figure 1

raw_data_meantemp_per1 <- gtchicks_alt %>% #for raw data points
  mutate(temp_bin = cut(average_temperature_per1, breaks = seq(floor(min(average_temperature_per1)), ceiling(max(average_temperature_per1)), by = 1))) %>%
  group_by(temp_bin) %>%
  summarise(
    mean_weight = mean(chickweight, na.rm = TRUE),
    se_weight = sd(chickweight, na.rm = TRUE) / sqrt(n()),
    mid_temp = mean(average_temperature_per1),
    mass_n = n() ######## bin it with 1 or 0.5
  ) 

print(
  ggplot(pred_meantemp_per1_spline, aes(x = x, y = predicted)) + #predicted values here
    geom_line(colour = "#FF894F", linewidth = 1.1, alpha = 1, lineend = "round") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FF894F") +
    geom_errorbar(
      data = raw_data_meantemp_per1, 
      aes(x = mid_temp, y = mean_weight, 
          ymin = mean_weight - se_weight, 
          ymax = mean_weight + se_weight), 
      width = 0.05, color = "#272727", alpha = 0.9, inherit.aes = FALSE
    ) +
    geom_point(
      data = raw_data_meantemp_per1,  
      aes(x = mid_temp, y = mean_weight), 
      alpha = 0.8, fill = "#FF894F", 
      size = 3, shape = 21, stroke = 0, inherit.aes = FALSE
    ) +
    labs(
      x = "Average temperature (Hatchling period)",
      y = "Fledging Weight"
    ) +
    theme_classic() +
    scale_y_continuous(limits = c(16.4, 19), breaks = seq(1, 19, 0.2)) +
    scale_x_continuous(limits = c(6.5, 19.5), breaks = seq(6, 20, 1)) +
    annotation_custom(
      grob = ggplotGrob(
        ggplot(gtchicks_alt, aes(x = average_temperature_per1)) +
          geom_density(fill = "#FF894F", color = "#FF894F", alpha = 0.7) +
          theme_void() +
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
      ),
      xmin = 6, xmax = 19.8,
      ymin = 16.4, ymax = 17   # Adjust for desired height/placement
    )
)

raw_data_meantemp_per2 <- gtchicks_alt %>%
  mutate(temp_bin = cut(average_temperature_per2, breaks = seq(floor(min(average_temperature_per2)), ceiling(max(average_temperature_per2)), by = 1))) %>%
  group_by(temp_bin) %>%
  summarise(
    mean_weight = mean(chickweight, na.rm = TRUE),
    se_weight = sd(chickweight, na.rm = TRUE) / sqrt(n()),
    mid_temp = mean(average_temperature_per2),
    mass_n = n() ######## bin it with 1 or 0.5
  )

print(
  ggplot(pred_meantemp_per2_spline, aes(x = x, y = predicted)) + #predicted values here
    geom_line(colour="#FF894F", linewidth = 1.1, alpha = 1,
              lineend = "round") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#FF894F") +
    
    labs(
      x = "Average temperature (Nestling period)",
      y = "Fledging Weight") +
    theme_classic() + 
    geom_errorbar(
      data = raw_data_meantemp_per2, 
      aes(x = mid_temp, y = mean_weight, 
          ymin = mean_weight - se_weight, 
          ymax = mean_weight + se_weight), 
      width = 0.05, 
      color = "#272727",
      alpha=0.9
    ) +
    geom_point(
      data = raw_data_meantemp_per2,  
      aes(x = mid_temp, y = mean_weight), 
      alpha=0.8,
      fill = "#FF894F", 
      size = 3, 
      shape = 21, 
      stroke = 0
    ) +
    scale_y_continuous(limits = c(17, 19), breaks = seq(1, 19, 0.2)) +
    scale_x_continuous(limits = c(6.5, 19.5), breaks = seq(6, 20, 1)) +
    annotation_custom(
      grob = ggplotGrob(
        ggplot(gtchicks_alt, aes(x = average_temperature_per2)) +
          geom_density(fill = "#FF894F", color = "#FF894F", alpha = 0.7) +
          theme_void() +
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
      ),
      xmin = 6, xmax = 20,
      ymin = 17, ymax = 17.4   # Adjust for desired height/placement
    )
)

#### ------------------------------ average rainfall ----------------------------  ####

chickweight_meanrain_per1 <- lmer(chickweight ~ scale(average_rainfall_per1)  + scale(april_birthlaydate) +
                                    scale(natalclutch_size) + 
                                    (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(chickweight_meanrain_per1)

chickweight_meanrain_per2 <- lmer(chickweight ~ scale(average_rainfall_per2)  + scale(april_birthlaydate) +
                                    scale(natalclutch_size) + 
                                    (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(chickweight_meanrain_per2)

#predictions for plots

pred_meanrain_per1 <- ggpredict(chickweight_meanrain_per1, terms = c("average_rainfall_per1 [all]"))
pred_meanrain_per2 <- ggpredict(chickweight_meanrain_per2, terms = c("average_rainfall_per2 [all]"))

#plots for Figure 1

raw_data_meanrain_per1 <- gtchicks_alt %>%
  mutate(rain_bin = cut(average_rainfall_per1, 
                        breaks = seq(min(average_rainfall_per1, na.rm = TRUE),
                                     max(average_rainfall_per1, na.rm = TRUE),
                                     length.out = 20),  # Adjust the number of bins as needed
                        include.lowest = TRUE,
                        labels = FALSE)) %>% group_by(rain_bin) %>%
  summarise(
    mean_weight = mean(chickweight, na.rm = TRUE),
    se_weight = sd(chickweight, na.rm = TRUE) / sqrt(n()),
    mid_rain = mean(average_rainfall_per1, na.rm = TRUE),
    mass_n = n()
  ) 

print(
  ggplot(pred_meanrain_per1, aes(x = x, y = predicted)) +
    geom_line(colour="#465C88", linewidth = 1.1, alpha = 1,
              lineend = "round") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#465C88") +
    
    labs(
      x = "Average rainfall (Hatchling period)",
      y = "Fledging Weight") +
    theme_classic() + 
    geom_errorbar(
      data = raw_data_meanrain_per1, 
      aes(x = mid_rain, y = mean_weight, 
          ymin = mean_weight - se_weight, 
          ymax = mean_weight + se_weight), 
      width = 0.05, 
      color = "#272727",
      alpha=0.9
    ) +
    geom_point(
      data = raw_data_meanrain_per1,  
      aes(x = mid_rain, y = mean_weight), 
      alpha=0.8,
      fill = "#465C88", 
      size = 3, 
      shape = 21, 
      stroke = 0
    ) + scale_y_continuous(limits = c(16.7, 19.6), breaks = seq(16.8, 19.6, 0.2))+
    scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 0.5))+
    annotation_custom(
      grob = ggplotGrob(
        ggplot(gtchicks_alt, aes(x = average_rainfall_per1)) +
          geom_density(fill = "#465C88", color = "#465C88", alpha = 0.7) +
          theme_void() +
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
      ),
      xmin = -0.3, xmax = 8.2,
      ymin = 16.75, ymax = 17.6   # Adjust for desired height/placement
    )
)

raw_data_meanrain_per2 <- gtchicks_alt %>%
  mutate(rain_bin = cut(average_rainfall_per2, 
                        breaks = seq(min(average_rainfall_per2, na.rm = TRUE),
                                     max(average_rainfall_per2, na.rm = TRUE),
                                     length.out = 20),  # Adjust the number of bins as needed
                        include.lowest = TRUE,
                        labels = FALSE)) %>% group_by(rain_bin) %>%
  summarise(
    mean_weight = mean(chickweight, na.rm = TRUE),
    se_weight = sd(chickweight, na.rm = TRUE) / sqrt(n()),
    mid_rain = mean(average_rainfall_per2, na.rm = TRUE),
    mass_n = n()
  ) 

print(
  ggplot(pred_meanrain_per2, aes(x = x, y = predicted)) +
    geom_line(colour="#465C88", linewidth = 1.1, alpha = 1,
              lineend = "round") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#465C88") +
    
    labs(
      x = "Average rainfall (Nestling period)",
      y = "Fledging Weight") +
    theme_classic() + 
    geom_errorbar(
      data = raw_data_meanrain_per2, 
      aes(x = mid_rain, y = mean_weight, 
          ymin = mean_weight - se_weight, 
          ymax = mean_weight + se_weight), 
      width = 0.05, 
      color = "#272727",
      alpha=0.9
    ) +
    geom_point(
      data = raw_data_meanrain_per2,  
      aes(x = mid_rain, y = mean_weight), 
      alpha=0.8,
      fill = "#465C88", 
      size = 3, 
      shape = 21, 
      stroke = 0
    )  + scale_y_continuous(limits = c(16.7, 19.6), breaks = seq(16.8, 19.6, 0.2))+
    scale_x_continuous(limits = c(0, 8.5), breaks = seq(0, 8, 0.5))+
    annotation_custom(
      grob = ggplotGrob(
        ggplot(gtchicks_alt, aes(x = average_rainfall_per2)) +
          geom_density(fill = "#465C88", color = "#465C88", alpha = 0.7) +
          theme_void() +
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
      ),
      xmin = -0.3, xmax = 8.5,
      ymin = 16.75, ymax = 17.6   # Adjust for desired height/placement
    )
)

##### ---------------------------- hot ECEs --------------------------  ####

hotece_per1_alt <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                          scale(natalclutch_size) + scale(hot_ece1_5) +
                          (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(hotece_per1_alt)

hotece_per2_alt <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                          scale(natalclutch_size) + scale(hot_ece2_5) +
                          (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(hotece_per2_alt)

hotece_per2_spline <- lmer(chickweight ~ ns(hot_ece2_5, df = 4) + scale(average_temperature_per2)  + scale(april_birthlaydate) +
                             scale(natalclutch_size)  +  (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_remove5)
summary(hotece_per2_spline)

#predictions 

pred_hotece_per1 <- ggpredict(hotece_per1_alt, terms = c("hot_ece1_5 [all]"))
pred_hotece_per2 <- ggpredict(hotece_per2_alt, terms = c("hot_ece2_5 [all]")) 
pred_hotece_per2_spline <- ggpredict(hotece_per2_spline, terms = c("hot_ece2_5 [all]"))

#plot for Figure 2

raw_hotece_per2_alt <- gtchicks_alt_remove5 %>%
  group_by(hot_ece2_5) %>%
  summarise(
    mass_m = mean(chickweight),
    mass_sd = sd(chickweight),
    mass_n = n()
  ) %>%
  mutate(mass_se = mass_sd / sqrt(mass_n))

ggplot(pred_hotece_per2_spline, aes(x = x, y = predicted)) +
  geom_line(colour="#B33A3A", linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#e92c4e") +
  labs(
    x = "Number of Hot ECEs (Nestling period)",
    y = "Fledging Weight") +
  theme_classic() + 
  geom_errorbar(
    data = raw_hotece_per2_alt, 
    aes(x = hot_ece2_5, y = mass_m, ymin = mass_m - mass_se, ymax = mass_m + mass_se), 
    width = 0.05, 
    color = "#272727",
    alpha=0.9
  ) +
  geom_point(
    data = raw_hotece_per2_alt,  
    aes(x = hot_ece2_5, y = mass_m), 
    alpha=0.8,
    fill = "#B33A3A", 
    size = 3, 
    shape = 21, 
    stroke = 0
  ) + 
  geom_text(
    data = raw_hotece_per2_alt,
    aes(
      x = hot_ece2_5,
      y = mass_m + mass_se + 0.015,  
      label = mass_n
    ),
    size = 2.5,             
    vjust = 0,            
    color = "#272727",
    family = "Roboto Condensed",
    fontface="bold"
  ) + scale_y_continuous(limits = c(18.0, 19.6), breaks = seq(18.0, 19.6, 0.2))+
  scale_x_continuous(limits = c(0, 7.05), breaks = seq(0,7, 1)) +
  theme(plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  )) 

##### ---------------------------- cold ECEs --------------------------  #####

coldece_per1 <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                           scale(natalclutch_size) + scale(cold_ece1_5) +
                           (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(coldece_per1)

coldece_per2 <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                     scale(natalclutch_size) + scale(cold_ece2_5) +
                       (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(coldece_per2)

#predictions
pred_coldece_per1_alt <- ggpredict(coldece_per1, terms = c("cold_ece1_5 [all]"))
pred_coldece_per2 <- ggpredict(coldece_per2, terms = c("cold_ece2_5 [all]"))

#plot for Figure 2

raw_coldece_per1_alt <- gtchicks_alt %>%
  group_by(cold_ece1_5) %>%
  summarise(
    mass_m = mean(chickweight),
    mass_sd = sd(chickweight),
    mass_n = n()
  ) %>%
  mutate(mass_se = mass_sd / sqrt(mass_n))

ggplot(pred_coldece_per1_alt, aes(x = x, y = predicted)) +
  geom_line(colour="#539ca1", linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#3acaff") +
  labs(
    x = "Number of Cold ECEs (Hatchling period)",
    y = "Fledging Weight") +
  theme_classic() + 
  geom_errorbar(
    data = raw_coldece_per1_alt, 
    aes(x = cold_ece1_5, y = mass_m, ymin = mass_m - mass_se, ymax = mass_m + mass_se), 
    width = 0.05, 
    color = "#272727",
    alpha=0.9
  ) +
  geom_point(
    data = raw_coldece_per1_alt,  
    aes(x = cold_ece1_5, y = mass_m), 
    alpha=0.8,
    fill = "#539ca1", 
    size = 3, 
    shape = 21, 
    stroke = 0
  ) + 
  geom_text(
    data = raw_coldece_per1_alt,
    aes(
      x = cold_ece1_5,
      y = mass_m + mass_se + 0.015,  # Adjust vertical offset as needed
      label = mass_n
    ),
    size = 2.5,             # Adjust text size as needed
    vjust = 0,            # Align text above the point
    color = "#272727",
    family = "Roboto Condensed",
    fontface="bold"
  ) + scale_y_continuous(limits = c(17.2, 19.0), breaks = seq(17.2, 19.0, 0.2))+
  scale_x_continuous(limits = c(0, 6.2), breaks = seq(0,6, 1)) +
  theme(plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  ))

##### ---------------------------- rain ECEs --------------------------  #####

rainece_per1 <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                       scale(natalclutch_size) + scale(rain_ece1_5) +
                       (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(rainece_per1)

rainece_per2 <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                       scale(natalclutch_size) + scale(rain_ece2_5) +
                       (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt)
summary(rainece_per2)

#predictions

pred_rainece_per1 <- ggpredict(rainece_per1, terms = c("rain_ece1_5 [all]")) 
pred_rainece_per2 <- ggpredict(rainece_per2, terms = c("rain_ece2_5 [all]")) 

#plot for Figure 3

raw_rainece_per2 <- gtchicks_alt %>%
  group_by(rain_ece2_5) %>%
  summarise(
    mass_m = mean(chickweight),
    mass_sd = sd(chickweight),
    mass_n = n()
  ) %>%
  mutate(mass_se = mass_sd / sqrt(mass_n))

ggplot(pred_rainece_per2, aes(x = x, y = predicted)) +
  geom_line(colour="#8BAD66", linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill="#5e9a1f") +
  labs(
    x = "Number of Rain ECEs (Nestling period)",
    y = "Fledging Weight") +
  theme_classic() + 
  geom_errorbar(
    data = raw_rainece_per2, 
    aes(x = rain_ece2_5, y = mass_m, ymin = mass_m - mass_se, ymax = mass_m + mass_se), 
    width = 0.05, 
    color = "#272727",
    alpha=0.9
  ) +
  geom_point(
    data = raw_rainece_per2,  
    aes(x = rain_ece2_5, y = mass_m), 
    alpha=0.8,
    fill = "#8BAD66", 
    size = 3, 
    shape = 21, 
    stroke = 0
  ) + 
  geom_text(
    data = raw_rainece_per2,
    aes(
      x = rain_ece2_5,
      y = mass_m + mass_se + 0.015,  # Adjust vertical offset as needed
      label = mass_n
    ),
    size = 2.5,             # Adjust text size as needed
    vjust = 0,            # Align text above the point
    color = "#272727",
    family = "Roboto Condensed",
    fontface="bold"
  ) + scale_y_continuous(limits = c(17.4, 18.65), breaks = seq(17.4, 18.6, 0.2))+
  scale_x_continuous(limits = c(0, 4.2), breaks = seq(0,4, 1)) +
  theme(plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  ))

                    #########  BINARY MODELS FOR 5 % AND 1% ECES  #########

#creating a dataset that contains information on whether an individual experienced at least 1 ECE of a certain type and threshold

gtchicks_alt_1perc <- gtchicks_alt %>%
  mutate(
    hot_ece1_1_bin = ifelse(hot_ece1_1 > 0, 1, 0),
    cold_ece1_1_bin = ifelse(cold_ece1_1 > 0, 1, 0),
    rain_ece1_1_bin = ifelse(rain_ece1_1 > 0, 1, 0),
    hot_ece2_1_bin = ifelse(hot_ece2_1 > 0, 1, 0),
    cold_ece2_1_bin = ifelse(cold_ece2_1 > 0, 1, 0),
    rain_ece2_1_bin = ifelse(rain_ece2_1 > 0, 1, 0)
  )

gtchicks_alt_1perc <- gtchicks_alt_1perc %>%
  mutate(
    hot_ece1_5_bin = ifelse(hot_ece1_5 > 0, 1, 0),
    cold_ece1_5_bin = ifelse(cold_ece1_5 > 0, 1, 0),
    rain_ece1_5_bin = ifelse(rain_ece1_5 > 0, 1, 0),
    hot_ece2_5_bin = ifelse(hot_ece2_5 > 0, 1, 0),
    cold_ece2_5_bin = ifelse(cold_ece2_5 > 0, 1, 0),
    rain_ece2_5_bin = ifelse(rain_ece2_5 > 0, 1, 0)
  )

### run models

#hot eces
hotece_hat_1perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                           scale(natalclutch_size) + scale(hot_ece1_1_bin) +
                           (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(hotece_hat_1perc)

hotece_hat_5perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                           scale(natalclutch_size) + scale(hot_ece1_5_bin) +
                           (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(hotece_hat_5perc)


hotece_nes_1perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                           scale(natalclutch_size) + scale(hot_ece2_1_bin) +
                           (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(hotece_nes_1perc)

hotece_nes_5perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                           scale(natalclutch_size) + scale(hot_ece2_5_bin) +
                           (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(hotece_nes_5perc)

#cold eces
coldece_hat_1perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(cold_ece1_1_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(coldece_hat_1perc)

coldece_hat_5perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(cold_ece1_5_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(coldece_hat_5perc)


coldece_nes_1perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(cold_ece2_1_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(coldece_nes_1perc)

coldece_nes_5perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(cold_ece2_5_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(coldece_nes_5perc)

#rain eces
rainece_hat_1perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(rain_ece1_1_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(rainece_hat_1perc)

rainece_hat_5perc <- lmer(chickweight ~ scale(average_temperature_per1)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(rain_ece1_5_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(rainece_hat_5perc)


rainece_nes_1perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(rain_ece2_1_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(rainece_nes_1perc)

rainece_nes_5perc <- lmer(chickweight ~ scale(average_temperature_per2)  + scale(april_birthlaydate) +
                            scale(natalclutch_size) + scale(rain_ece2_5_bin) +
                            (1 | birthyear) + (1 | broodid) + (1|mother) + (1|natalbox), data = gtchicks_alt_1perc)
summary(rainece_nes_5perc)


                            #########  INTERACTION MODELS  #########

#### -------------------------- rain ece x mean temp  --------------------------- ####

int_rainece1xmeantemp <- lmer(chickweight ~ scale(average_temperature_per1) + scale(april_birthlaydate)+
                                I(scale(average_temperature_per1)^2) + 
                                scale(natalclutch_size) + 
                                scale(rain_ece1_5) * scale(average_temperature_per1) +
                                scale(rain_ece1_5) * I(scale(average_temperature_per1)^2)+
                                (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                              data = gtchicks_alt)
summary(int_rainece1xmeantemp)

int_rainece2xmeantemp <- lmer(chickweight ~ scale(average_temperature_per2) + scale(april_birthlaydate)+
                                I(scale(average_temperature_per2)^2) + 
                                scale(natalclutch_size) + 
                                scale(rain_ece2_5) * scale(average_temperature_per2) +
                                scale(rain_ece2_5) * I(scale(average_temperature_per2)^2)+
                                (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                              data = gtchicks_alt)
summary(int_rainece2xmeantemp)

pred_int_rainece1xmeantemp <- ggpredict(int_rainece1xmeantemp, 
                                        terms = c("average_temperature_per1 [all]", 
                                                  "rain_ece1_5 [all]"))

#Plot for Figure 3

pred_int_rainece1xmeantempclean <- pred_int_rainece1xmeantemp[pred_int_rainece1xmeantemp$group %in% c("0", "2", "4"), ]

ggplot(pred_int_rainece1xmeantempclean,  aes(x = x, y = predicted, color = group, fill= group)) +
  geom_line(linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color=NA) +
  scale_color_manual(values = c("0" = "#DD88CF", "2" = "#89AC46", "4" = "#8390fa")) +
  scale_fill_manual(values = c("0" = "#d90368", "2" = "#6a994e", "4" = "#8B5DFF")) +
  labs(color = "Number of Rain ECEs",
       fill = "Number of Rain ECEs",
       x = "Average Temperature (Hatchling period)",
       y = "Fledging Weight") +
  scale_y_continuous(limits = c(13.5, 19.5), breaks = seq(13.5, 19.5, 0.5)) +
  scale_x_continuous(limits = c(6, 19.5), breaks = seq(6, 20, 1)) +
  theme_classic() + theme(legend.position = "bottom", plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  )) 

#### -------------------------- hot ece x mean rainfall  --------------------------- ####

int_hotece1xmeanrain <- lmer(chickweight ~ scale(average_rainfall_per1) + scale(april_birthlaydate)+
                               scale(hot_ece1_5) + 
                               scale(natalclutch_size) + 
                               scale(hot_ece1_5) * scale(average_rainfall_per1) +
                               (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                             data = gtchicks_alt)
summary(int_hotece1xmeanrain)

int_hotece2xmeanrain <- lmer(chickweight ~ scale(average_rainfall_per2) + scale(april_birthlaydate)+
                               scale(hot_ece2_5) + 
                               scale(natalclutch_size) + 
                               scale(hot_ece2_5) * scale(average_rainfall_per2) +
                               (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                             data = gtchicks_alt)
summary(int_hotece2xmeanrain)

pred_int_hotece1xmeanrain <- ggpredict(int_hotece1xmeanrain, 
                                       terms = c("average_rainfall_per1 [all]", 
                                                 "hot_ece1_5 [all]"))
pred_int_hotece2xmeanrain <- ggpredict(int_hotece2xmeanrain, 
                                       terms = c("average_rainfall_per2 [all]", 
                                                 "hot_ece2_5 [all]"))

#Plot for Figure 3

pred_int_hotece1xmeanrainclean <- pred_int_hotece1xmeanrain %>%
  filter(group %in% c("0", "3", "6"))

ggplot(pred_int_hotece1xmeanrainclean,  aes(x = x, y = predicted, color = group, fill= group)) +
  geom_line(linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color=NA) +
  scale_color_manual(values = c("0" = "#8390fa", "3" = "#fb8b24", "6" = "#d90368")) +
  scale_fill_manual(values = c("0" = "#8390fa", "3" = "#fb8b24", "6" = "#d90368")) +
  labs(color = "Number of Hot ECEs",
       fill = "Number of Hot ECEs",
       x = "Average Rainfall (Hatchling period)",
       y = "Fledging Weight") +
  scale_y_continuous(limits = c(12.5, 19.5), breaks = seq(12.5, 19.5, 1)) +
  # scale_x_continuous(limits = c(6, 19.5), breaks = seq(6, 20, 1)) +
  theme_classic() + theme(legend.position = "bottom", plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  )) 

#### -------------------------- cold ece x mean rainfall  --------------------------- ####

int_coldece1xmeanrain <- lmer(chickweight ~ scale(average_rainfall_per1) + scale(april_birthlaydate)+
                                scale(cold_ece1_5) + 
                                scale(natalclutch_size) + 
                                scale(cold_ece1_5) * scale(average_rainfall_per1) +
                                (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                              data = gtchicks_alt)
summary(int_coldece1xmeanrain)

int_coldece2xmeanrain <- lmer(chickweight ~ scale(average_rainfall_per2) + scale(april_birthlaydate)+
                                scale(cold_ece2_5) + 
                                scale(natalclutch_size) + 
                                scale(cold_ece2_5) * scale(average_rainfall_per2) +
                                (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                              data = gtchicks_alt)
summary(int_coldece2xmeanrain)

#### -------------------------- hot ece x relative lay date  --------------------------- ####

#calculate relative laydates

gtchicks_alt <- gtchicks_alt %>%
  group_by(birthyear) %>%
  mutate(mean_ld = mean(april_birthlaydate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(relative_ld = april_birthlaydate - mean_ld)

laydatemodel <- lmer(chickweight ~   scale(natalclutch_size) + 
                       scale(relative_ld) +  I(scale(relative_ld)^2) +
                       (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                     data = gtchicks_alt)
summary(laydatemodel)


#run model

int_hotece1xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per1) + scale(relative_ld)+
                                  I(scale(relative_ld)^2) + 
                                  scale(natalclutch_size) + 
                                  scale(hot_ece1_5) * scale(relative_ld) +
                                  scale(hot_ece1_5) * I(scale(relative_ld)^2)+
                                  (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                data = gtchicks_alt)
summary(int_hotece1xlaydatequad)
int_hotece2xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per2) + scale(relative_ld)+
                                  I(scale(relative_ld)^2) + 
                                  scale(natalclutch_size) + 
                                  scale(hot_ece2_5) * scale(relative_ld) +
                                  scale(hot_ece2_5) * I(scale(relative_ld)^2)+
                                  (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                data = gtchicks_alt)
summary(int_hotece2xlaydatequad)

pred_int_hotece1xlaydatequad <- ggpredict(int_hotece1xlaydatequad, 
                                          terms = c("relative_ld [all]", 
                                                    "hot_ece1_5 [all]"))
pred_int_hotece2xlaydatequad <- ggpredict(int_hotece2xlaydatequad, 
                                          terms = c("relative_ld [all]", 
                                                    "hot_ece2_5 [all]"))

#plot for Figure 3

pred_int_hotece1xlaydatequadclean <- pred_int_hotece1xlaydatequad %>%
  filter(group %in% c("0", "3", "6"))

ggplot(pred_int_hotece1xlaydatequadclean,  aes(x = x, y = predicted, color = group, fill= group)) +
  geom_line(linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color=NA) +
  scale_color_manual(values = c("0" = "#8390fa", "3" = "#fb8b24", "6" = "#d90368")) +
  scale_fill_manual(values = c("0" = "#8390fa", "3" = "#fb8b24", "6" = "#d90368")) +
  labs(color = "Number of Hot ECEs",
       fill = "Number of Hot ECEs",
       x = "Relative Laydate",
       y = "Fledging Weight") +
  scale_y_continuous(limits = c(11, 21), breaks = seq(11, 21, 1)) +
  scale_x_continuous(limits = c(-25, 60), breaks = seq(-25, 60, 5)) +
  theme_classic() + theme(legend.position = "bottom", plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  )) 

#### -------------------------- cold ece x relative lay date  --------------------------- ####

int_coldece1xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per1) + scale(relative_ld)+
                                   I(scale(relative_ld)^2) + 
                                   scale(natalclutch_size) + 
                                   scale(cold_ece1_5) * scale(relative_ld) +
                                   scale(cold_ece1_5) * I(scale(relative_ld)^2)+
                                   (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                 data = gtchicks_alt)
summary(int_coldece1xlaydatequad)

int_coldece2xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per2) + scale(relative_ld)+
                                   I(scale(relative_ld)^2) + 
                                   scale(natalclutch_size) + 
                                   scale(cold_ece2_5) * scale(relative_ld) +
                                   scale(cold_ece2_5) * I(scale(relative_ld)^2)+
                                   (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                 data = gtchicks_alt)
summary(int_coldece2xlaydatequad)

#### -------------------------- rain ece x relative lay date  --------------------------- ####

int_rainece1xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per1) + scale(relative_ld)+
                                   I(scale(relative_ld)^2) + 
                                   scale(natalclutch_size) + 
                                   scale(rain_ece1_5) * scale(relative_ld) +
                                   scale(rain_ece1_5) * I(scale(relative_ld)^2)+
                                   (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                 data = gtchicks_alt)
summary(int_rainece1xlaydatequad)

int_rainece2xlaydatequad <- lmer(chickweight ~ scale(average_temperature_per2) + scale(relative_ld)+
                                   I(scale(relative_ld)^2) + 
                                   scale(natalclutch_size) + 
                                   scale(rain_ece2_5) * scale(relative_ld) +
                                   scale(rain_ece2_5) * I(scale(relative_ld)^2)+
                                   (1 | birthyear) + (1 | broodid) + (1 | mother) + (1|natalbox), 
                                 data = gtchicks_alt)
summary(int_rainece2xlaydatequad)

pred_int_rainece1xlaydatequad <- ggpredict(int_rainece1xlaydatequad, 
                                           terms = c("relative_ld [all]", 
                                                     "rain_ece1_5 [all]"))
pred_int_rainece2xlaydatequad <- ggpredict(int_rainece2xlaydatequad, 
                                           terms = c("relative_ld [all]", 
                                                     "rain_ece2_5 [all]"))

#Plot for Figure 3
pred_int_rainece2xlaydatequadclean <- pred_int_rainece2xlaydatequad %>%
  filter(group %in% c("0", "2", "4"))

ggplot(pred_int_rainece2xlaydatequadclean,  aes(x = x, y = predicted, color = group, fill= group)) +
  geom_line(linewidth = 1.1, alpha = 1,
            lineend = "round") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color=NA) +
  scale_color_manual(values = c("0" = "#DD88CF", "2" = "#89AC46", "4" = "#8390fa")) +
  scale_fill_manual(values = c("0" = "#d90368", "2" = "#6a994e", "4" = "#8B5DFF")) +
  labs(color = "Number of Rain ECEs",
       fill = "Number of Rain ECEs",
       x = "Relative laydate",
       y = "Fledging Weight") +
  scale_y_continuous(limits = c(11, 21), breaks = seq(11, 21, 1)) +
  scale_x_continuous(limits = c(-25, 60), breaks = seq(-25, 60, 5)) +
  theme_classic() + theme(legend.position = "bottom", plot.background = ggplot2::element_rect(
    fill = "white", colour = NA
  )) 


