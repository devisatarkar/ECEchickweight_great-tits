
#### LOCAL RECRUITMENT MODELS ####

#---------- REQUIRED LIBRARIES ----------------

library(dplyr)
library(tidyr)
library(ggeffects)
library(ggplot2)
library(lme4)
library(lmerTest)


# ---------- PREPARE DATA ----------------

survivalgt <- read.csv("/ENTERYOURDIRECTORY/gretisurvivaldata_updated_2025.csv", na.strings=c("", "NA"))

print(mean(survivalgt$local_recruitment)) #9.1%
print(sum(survivalgt$local_recruitment == 0)) #76293 birds did not recruit
print(sum(survivalgt$local_recruitment == 1)) #7642 birds recruited

# ---------- RUN MODELS ----------------

######## cold ECEs #########

#categorising ECE variables

gtchicks_altcat <- survivalgt %>%
  mutate(
    cat_cold_ece1 = case_when(
      cold_ece1_5 >= 4 ~ "4+",
      TRUE ~ as.character(cold_ece1_5)),
    cat_cold_ece2 = case_when(
      cold_ece2_5 >= 4 ~ "4+",
      TRUE ~ as.character(cold_ece2_5))) %>%
  mutate(
    cat_cold_ece1 = factor(cat_cold_ece1, levels = c("0", "1", "2", "3", "4+")),
    cat_cold_ece2 = factor(cat_cold_ece2, levels = c("0", "1", "2", "3", "4+")))

              ### -------- cold ECEs (hatchling stage) ---------- ###

survival_coldece1cat <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                                cat_cold_ece1 +  
                                scale(natalclutch_size) +
                                (1 | birthyear) + (1 | broodid)
                              + (1|mother) + (1|natalbox),
                              family = binomial,
                              data = gtchicks_altcat)
summary(survival_coldece1cat)

survival_coldece1cat_withld <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                                       cat_cold_ece1 +  
                                       scale(natalclutch_size) + scale(april_birthlaydate) +
                                       (1 | birthyear) + (1 | broodid)
                                     + (1|mother) + (1|natalbox),
                                     family = binomial,
                                     data = gtchicks_altcat)
summary(survival_coldece1cat_withld) 

survivalpred_coldece1cat <- ggpredict(survival_coldece1cat, terms = c("cat_cold_ece1 [all]"))
survivalpred_coldece1cat_withld <- ggpredict(survival_coldece1cat_withld, terms = c("cat_cold_ece1 [all]"))

#plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_coldece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_coldece1cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_coldece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_coldece1cat_withld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_coldece1cat_withld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_coldece1cat_withld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#3674B5", "With Laydate" = "#0C0950")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_coldece1cat$x)),
    labels = levels(survivalpred_coldece1cat$x)
  ) +
  scale_y_continuous(limits = c(0.03, 0.13), breaks = seq(0.03, 0.13, 0.01) )+
  # scale_y_continuous(limits = c(0.03, 0.16), breaks = seq(0.03, 0.16, 0.01))+
  labs(
    x = "Number of Cold ECEs (Hatchling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic() 

                ### -------- cold ECEs (nestling stage) ---------- ###

survival_coldece2cat <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                                cat_cold_ece2 +  
                                scale(natalclutch_size) +
                                (1 | birthyear) + (1 | broodid)
                              + (1|mother) + (1|natalbox),
                              family = binomial,
                              data = gtchicks_altcat)
summary(survival_coldece2cat)

survival_coldece2catld <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                                  cat_cold_ece2 +  scale(april_birthlaydate) +
                                  scale(natalclutch_size) +
                                  (1 | birthyear) + (1 | broodid)
                                + (1|mother) + (1|natalbox),
                                family = binomial,
                                data = gtchicks_altcat)
summary(survival_coldece2catld)

survivalpred_coldece2cat <- ggpredict(survival_coldece2cat, terms = c("cat_cold_ece2 [all]"))
survivalpred_coldece2catld <- ggpredict(survival_coldece2catld, terms = c("cat_cold_ece2 [all]"))

#Plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_coldece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_coldece2cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_coldece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_coldece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_coldece2catld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_coldece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#3674B5", "With Laydate" = "#0C0950")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_coldece2cat$x)),
    labels = levels(survivalpred_coldece2cat$x)
  ) +
  scale_y_continuous(limits = c(0.03, 0.13), breaks = seq(0.03, 0.13, 0.01) )+
  labs(
    x = "Number of Cold ECEs (Nestling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic() 

######## hot ECEs #########

#categorising ECE variables

gtchicks_altcathot <- survivalgt %>%
  mutate(
    cat_hot_ece1 = case_when(
      hot_ece1_5 >= 5 ~ "5+",
      TRUE ~ as.character(hot_ece1_5) ),
    cat_hot_ece2 = case_when(
      hot_ece2_5 >= 5 ~ "5+",
      TRUE ~ as.character(hot_ece2_5) ) ) %>%
  mutate(cat_hot_ece1 = factor(cat_hot_ece1, levels = c("0", "1", "2", "3", "4", "5+")),
         cat_hot_ece2 = factor(cat_hot_ece2, levels = c("0", "1", "2", "3", "4", "5+")) )

                     ### -------- hot ECEs (hatchling stage) ---------- ###

survival_hotece1cat <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                               cat_hot_ece1 + 
                               scale(natalclutch_size)+
                               (1 | birthyear) + (1 | broodid)
                             + (1|mother) + (1|natalbox),
                             family = binomial,
                             data = gtchicks_altcathot)
summary(survival_hotece1cat)

survival_hotece1catld <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                                 cat_hot_ece1 + scale(april_birthlaydate) +
                                 scale(natalclutch_size)+
                                 (1 | birthyear) + (1 | broodid)
                               + (1|mother) + (1|natalbox),
                               family = binomial,
                               data = gtchicks_altcathot)
summary(survival_hotece1catld)

survivalpred_hotece1cat <- ggpredict(survival_hotece1cat, terms = c("cat_hot_ece1 [all]"))
survivalpred_hotece1catld <- ggpredict(survival_hotece1catld, terms = c("cat_hot_ece1 [all]"))

#Plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_hotece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_hotece1cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_hotece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_hotece1catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_hotece1catld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_hotece1catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#fb8b24", "With Laydate" = "#B33A3A")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_hotece1cat$x)),
    labels = levels(survivalpred_hotece1cat$x)
  ) +
  scale_y_continuous(limits = c(0.05, 0.165), breaks = seq(0.05, 0.16, 0.01))+
  labs(
    x = "Number of Hot ECEs (Hatchling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic() 

                        ### -------- hot ECEs (nestling stage) ---------- ###

survival_hotece2cat <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                               cat_hot_ece2 + 
                               scale(natalclutch_size)+
                               (1 | birthyear) + (1 | broodid)
                             + (1|mother) + (1|natalbox),
                             family = binomial,
                             data = gtchicks_altcathot)
summary(survival_hotece2cat)

survival_hotece2catld <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                                 cat_hot_ece2 + scale(april_birthlaydate) +
                                 scale(natalclutch_size)+
                                 (1 | birthyear) + (1 | broodid)
                               + (1|mother) + (1|natalbox),
                               family = binomial,
                               data = gtchicks_altcathot)
summary(survival_hotece2catld)

survivalpred_hotece2cat <- ggpredict(survival_hotece2cat, terms = c("cat_hot_ece2 [all]"))
survivalpred_hotece2catld <- ggpredict(survival_hotece2catld, terms = c("cat_hot_ece2 [all]"))

#Plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_hotece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_hotece2cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_hotece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_hotece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_hotece2catld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_hotece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#fb8b24", "With Laydate" = "#B33A3A")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_hotece2cat$x)),
    labels = levels(survivalpred_hotece2cat$x)
  ) +
  scale_y_continuous(limits = c(0.05, 0.167), breaks = seq(0.05, 0.16, 0.01))+
  labs(
    x = "Number of Hot ECEs (Nestling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic() 

######## rain ECEs #########

#categorising ECE variables

gtchicks_altcatrain <- survivalgt %>%
  mutate(
    cat_rain_ece1 = case_when(
      rain_ece1_5 >= 3 ~ "3+",
      TRUE ~ as.character(rain_ece1_5) ),
    cat_rain_ece2 = case_when(
      rain_ece2_5 >= 3 ~ "3+",
      TRUE ~ as.character(rain_ece2_5) ) ) %>%
  mutate( cat_rain_ece1 = factor(cat_rain_ece1, levels = c("0", "1", "2", "3+")),
          cat_rain_ece2 = factor(cat_rain_ece2, levels = c("0", "1", "2", "3+")) )

                           ### -------- rain ECEs (hatchling stage) ---------- ###

survival_rainece1cat <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                                cat_rain_ece1 + 
                                scale(natalclutch_size)+
                                (1 | birthyear) + (1 | broodid)
                              + (1|mother) + (1|natalbox),
                              family = binomial,
                              data = gtchicks_altcatrain)
summary(survival_rainece1cat)

survival_rainece1catld <- glmer(local_recruitment ~ scale(average_temperature_per1) + 
                                  cat_rain_ece1 + 
                                  scale(natalclutch_size)+ scale(april_birthlaydate) +
                                  (1 | birthyear) + (1 | broodid)
                                + (1|mother) + (1|natalbox),
                                family = binomial,
                                data = gtchicks_altcatrain)
summary(survival_rainece1catld)

survivalpred_rainece1cat <- ggpredict(survival_rainece1cat, terms = c("cat_rain_ece1 [all]"))
survivalpred_rainece1catld <- ggpredict(survival_rainece1catld, terms = c("cat_rain_ece1 [all]"))

#Plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_rainece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_rainece1cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_rainece1cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_rainece1catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_rainece1catld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_rainece1catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#89AC46", "With Laydate" = "#1F4529")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_rainece1cat$x)),
    labels = levels(survivalpred_rainece1cat$x)
  ) +
  scale_y_continuous(limits = c(0.04, 0.14), breaks = seq(0.04, 0.14, 0.01))+
  labs(
    x = "Number of Rain ECEs (Hatchling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic()

                     ### -------- rain ECEs (nestling stage) ---------- ###

survival_rainece2cat <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                                cat_rain_ece2 + 
                                scale(natalclutch_size)+
                                (1 | birthyear) + (1 | broodid)
                              + (1|mother) + (1|natalbox),
                              family = binomial,
                              data = gtchicks_altcatrain)
summary(survival_rainece2cat)

survival_rainece2catld <- glmer(local_recruitment ~ scale(average_temperature_per2) + 
                                  cat_rain_ece2 + scale(april_birthlaydate) +
                                  scale(natalclutch_size)+
                                  (1 | birthyear) + (1 | broodid)
                                + (1|mother) + (1|natalbox),
                                family = binomial,
                                data = gtchicks_altcatrain)
summary(survival_rainece2catld)

survivalpred_rainece2cat <- ggpredict(survival_rainece2cat, terms = c("cat_rain_ece2 [all]"))
survivalpred_rainece2catld <- ggpredict(survival_rainece2catld, terms = c("cat_rain_ece2 [all]"))

#Plot for Figure 4

ggplot() +
  # First (main) model
  geom_point(
    data = survivalpred_rainece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, color = "Without Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_rainece2cat,
    aes(x = as.numeric(x) - 0.1, ymin = conf.low, ymax = conf.high, color = "Without Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_rainece2cat,
    aes(x = as.numeric(x) - 0.1, y = predicted, group = 1, color = "Without Laydate"),
    alpha = 0.7,
    linewidth = 1.2
  ) +
  # Second model: with laydate
  geom_point(
    data = survivalpred_rainece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, color = "With Laydate"),
    size = 5,
    shape="diamond"
  ) +
  geom_errorbar(
    data = survivalpred_rainece2catld,
    aes(x = as.numeric(x) + 0.1, ymin = conf.low, ymax = conf.high, color = "With Laydate"),
    width = 0.1,
    alpha=0.5
  ) +
  geom_line(
    data = survivalpred_rainece2catld,
    aes(x = as.numeric(x) + 0.1, y = predicted, group = 1, color = "With Laydate"),
    alpha = 0.7,
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Model",
    values = c("Without Laydate" = "#89AC46", "With Laydate" = "#1F4529")
  ) +
  scale_x_continuous(
    breaks = 1:length(levels(survivalpred_rainece2cat$x)),
    labels = levels(survivalpred_rainece2cat$x)
  ) +
  scale_y_continuous(limits = c(0.04, 0.1), breaks = seq(0.04, 0.1, 0.01))+
  labs(
    x = "Number of Rain ECEs (Nestling Period)",
    y = "Probability of Local Recruitment"
  ) +
  theme_classic()

