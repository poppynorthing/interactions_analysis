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

#Data visualization
  #Total visits
  ggplot(data = visits, aes(x = flower_color, fill = flower_color)) + 
    geom_bar(fill = c("lightpink", "lightpink", "lightyellow")) +
    xlab("Flower Color") +
    ylab("Number of Visits")

  #Visits with count data
  ggplot(data = visits_counted, aes(x = flower_color, y = flower_visits, fill = flower_color)) +
    geom_boxplot(fill = c("lightpink", "lightpink", "lightyellow")) +
    xlab("Flower Color") +
    ylab("Number of Flower Visits per Inflorescence")
  