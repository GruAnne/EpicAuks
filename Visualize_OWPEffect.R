## Project: EPIC - Habitat Loss of AUK species
## Wageningen Marine Research, Den Helder, The Netherlands
## Script by Anne Grundlehner (anne.grundlehner@utas.edu.au)


## In this script we use the results from the modelling and simulations
## We visualize the effect of OWP on habitat loss over time and over space
## See: [BIRD]_ZAG_V3.R for the modelling and all the simulations
## and OWP_Effect_BoundaryArea_Seperate.R for just the simulation procedure for distances x to the OWP


#### Legend ####
## RB = Razobill density, n/km2
## GM = Guillemot density, nkm2
## AUK = Auk (GM + RB + unidentified auks), n/km2

## In this script we work with the output of the models
## More specifically, we work with bayesian test statistics from the simulations
## For more on that, see the *V3.R script
## A Bayesian test statistic of 0.5 or larger indicates a significant OWP effect,
## in other words, that the bird density is smaller in the area of interest compared to the rest of the study area


## Install packages
library(ggplot2)
library(dplyr)
library(readxl)
library(viridis)
library(RColorBrewer)

my_wd = "[insert your working directory]"

## Get data
setwd(my_wd)
RB <- read_xlsx("BayesianTestStatistics_OWPEffects_simulations.xlsx",
                  sheet = "RB", col_types = "text")%>%
  pivot_longer(cols = !c("Survey"))%>%
  filter(Survey != "Decription")%>%
  rename(Area = name, BayStat = value)

GM <- read_xlsx("BayesianTestStatistics_OWPEffects_simulations.xlsx",
                sheet = "GM", col_types = "text")%>%
  pivot_longer(cols = !c("Survey"))%>%
  filter(Survey != "Decription")%>%
  rename(Area = name, BayStat = value)

RB$BayStat = as.numeric(RB$BayStat)

RB$Area = factor(RB$Area, levels=c("BU", "Middle", "ZE", "Gemini", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
RBM = filter(RB, Survey == "Mean") # mean values
RBS = filter(RB, Survey != "Mean") # per survey
RBS$Area = factor(RBS$Area, levels = c("BU", "Middle", "ZE", "Gemini"))

# OWPs
RB1 = filter(RB, Area %in% c("BU", "ZE", "Middle")==T)%>%
  filter(Survey != "Mean")%>%
  mutate(Survey = as.factor(Survey))
RB1.mean = filter(RB, Area %in% c("BU", "ZE", "Middle")==T)%>%
  filter(Survey == "Mean")%>%
  mutate(Survey = as.factor(Survey))


# Plot OWP area  
ggplot(RB1)+
  geom_point(aes(x = Area, y=BayStat, group=Survey, col=Survey))+
  scale_color_brewer(palette = "Dark2")+
  geom_line(aes(x = Area, y=BayStat, group=Survey, col=Survey))+
  geom_line(data = RB1.mean, aes(x = Area, y=BayStat, col=Survey, group=Survey), 
             linewidth=1.5, col="black")+
  theme_bw()+ylim(0,1)+xlab("Part of OWP")+
  geom_hline(yintercept = 0.5, linetype = "dashed", col="darkgrey")+
  ylab("Value of test statistic")+
  ggtitle("RB")


RB2 = filter(RB, Area %in% c("BU", "ZE", "Middle")==F)%>%
  filter(Survey != "Mean")
RB2.mean = filter(RB, Area %in% c("BU", "ZE", "Middle")==F)%>%
  filter(Survey == "Mean")

## Plot per survey and mean for OWP and distances
ggplot(RB2)+
  geom_point(aes(x = Area, y=BayStat, group=Survey, col=Survey))+
  geom_line(aes(x = Area, y=BayStat, group=Survey, col=Survey))+
  scale_color_brewer(palette = "Dark2")+
  #geom_point(data = RB2.mean, aes(x = Area, y=BayStat, group=Survey, col=Survey), col="black")+
  geom_line(data = RB2.mean, aes(x = Area, y=BayStat, col=Survey, group=Survey),
            col="black", linewidth=1.5)+
  theme_bw()+ylim(0,1)+xlab("Distance from OWP (km)")+
  geom_hline(yintercept = 0.5, linetype = "dashed", col="darkgrey")+
  ylab("Value of test statistic")
  

ggplot(RB2)+ ### Add all species here
  geom_boxplot(aes(x = Area, y=BayStat))+
  #geom_line(aes(x = Area, y=BayStat, group=Survey, col=Survey))+
  geom_line(data = RB2.mean, aes(x = Area, y=BayStat, col=Survey, group=Survey), col="black", linewidth=1.5)


## Per OWP area, per survey (temporal)
## ADD ALL SPECIES HERE!
ggplot(RB[RB$Area %in% c("BU", "ZE", "Middle", "Gemini") & RB$Survey != "Mean",])+
  geom_point(aes(x = Survey, y=BayStat))+
  geom_line(aes(x = Survey, y=BayStat, group=Area))+theme_bw()+
  facet_wrap(~Area)+
  ggtitle("Temporeel verloop")+ylab("Value test statistic")+
  geom_hline(yintercept = 0.5, linetype = "dashed", col="darkgrey")+
  xlab("Survey")+ylim(0,1)

ggplot(GM[GM$Area %in% c("BU", "ZE", "Middle", "Gemini") & GM$Survey != "Mean",])+
  geom_point(aes(x = Survey, y=BayStat))+
  geom_line(aes(x = Survey, y=BayStat, group=Area))+theme_bw()+
  facet_wrap(~Area)+
  ggtitle("Temporeel verloop")+ylab("Value test statistic")+
  geom_hline(yintercept = 0.5, linetype = "dashed", col="darkgrey")+
  xlab("Survey")+ylim(0,1)+ggtitle("GM")

