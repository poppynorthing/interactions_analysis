#Author: Poppy Northing
#Ecology 600B: Interactions and Behavior Assignment
#Last edited: 12NOV2023

#load data
inflorescence <- read.csv("infloresence_data.csv")
as.factor(inflorescence$bush_number)
visits <- read.csv("visitation_data.csv")

visits[visits == "pinnk"] <- "pink"

visits_counted <- na.omit(visits)

#load libraries
library(tidyverse)
library(lme4)
library(ggpattern)
library(egg)
library(patchwork)

#getting summary data about inflorescences

  inflorescence_summary <- inflorescence %>% summarize(avg_yellow = mean(yellow_count),
                                                       error_yellow = sd(yellow_count),
                                                       avg_pink = mean(pink_count),
                                                       error_pink = sd(pink_count))
  inflorescence_summary
  
  #check for differences in inflorescence makeup between bushes to be safe
  infl_yellow_aov <- aov(yellow_count ~ bush_number, data = inflorescence)
  hist(infl_yellow_aov$residuals)
  summary(infl_yellow_aov) #no significant differences in inflorescence flower makeup
  
  infl_pink_aov <- aov(pink_count ~ bush_number, data = inflorescence)
  summary(infl_pink_aov) #no significant differences in inflorescence flower makeup
  
  #check qqplots for normality
  qqnorm(inflorescence$pink_count)
  qqline(inflorescence$pink_count)
  
  qqnorm(inflorescence$yellow_count)
  qqline(inflorescence$yellow_count)
  
#analysis of visits to inflorescences
  #summary of total visits to only yellow flowers, only pink flowers, or both colors on inflorescence
  visits_summary <- visits %>% group_by(flower_color) %>% summarize(counts = n())
  visits_summary
  
  #look at number of visitations for each color chosen (yellow, pink, or both)
  visits_counted_sum <- visits_counted %>% filter(flower_visits > 1) %>% group_by(flower_color) %>% summarize(color_counts = n(), 
                                                                                mean_flowers_visited = mean(flower_visits),
                                                                                error_visited = sd(flower_visits))
  visits_counted_sum
  
  #filter out the observations with only 1 flower visited
  visits_counted_models <- visits_counted %>% filter(flower_visits > 1)
  
  #anova
  visits_aov <- aov(flower_visits ~ flower_color, data = visits_counted_models)
  summary(visits_aov)
  
  #glm with quasipoisson family
  visits_glm <- glm(flower_visits ~ flower_color, data = visits_counted_models, family = quasi)
  summary(visits_glm)
  
  visits_glm_aov <- aov(visits_glm)
  TukeyHSD(visits_glm_aov)
  
  #normalizing data for flower color and then doing glm
  visits_counted_models_norm <- mutate(visits_counted_models, visits_norm = case_when(flower_color == "both" ~ flower_visits/17, flower_color == "yellow" ~ flower_visits/4, flower_color == "pink" ~ flower_visits/13))
  visits_counted_models_norm
  
  visits_norm_glm <- glm(visits_norm ~ flower_color, data = visits_counted_models_norm, family = quasi)
  summary(visits_norm_glm)
  
  #Data visualization
  #Total visits
  p1 <- ggplot(data = visits, aes(x = flower_color, fill = flower_color)) + 
    geom_bar(fill = c("sandybrown", "lightpink", "lightyellow")) +
    theme_article(base_size = 18) +
    xlab("Flower Color") +
    ylab("Number of Visits")

  #Visits with count data
  p2 <- ggplot(data = visits_counted_models, aes(x = flower_color, y = flower_visits, fill = flower_color)) +
    geom_boxplot(fill = c("sandybrown", "lightpink", "lightyellow")) +
    geom_jitter(color = "black", alpha = 0.8, size = 0.4) +
    theme_article(base_size = 18) +
    theme(legend.position = "none") +
    xlab("Flower Color") +
    ylab("Flower Visits per Inflorescence")
  
  final_plot <- p1 + p2
  final_plot + plot_annotation(tag_levels = "A")
  