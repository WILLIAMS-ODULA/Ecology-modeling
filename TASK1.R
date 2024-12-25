#Одула Джон Вильям для района Южное Медведково докажите что диаметр 
#стволов родов Клен и ТОПОЛЬ плакучая значимо отличаются.
#Odula John Williams for the Yuzhnoye Medvedkovo region prove that the diameter of the trunks of the 
#genera Chestnut and Weeping Larch differ significantly.
getwd()

rm(list=ls())
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)
greendb = read.csv("greendb.csv")
spec = greendb$species_ru
genus = stringr::str_split(spec, pattern = " ", simplify = T)[,1]

data  = greendb %>% mutate(Genus = genus)
data = data %>%  filter(Genus %in% c("Клен","Тополь")) %>%
  filter(adm_region== "район Южное Медведково")



data.aov <- aov(d_trunk_m ~ Genus, data = data)

summary(data.aov)
TukeyHSD(data.aov)



