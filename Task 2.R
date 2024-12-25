#Oдула Джон постройте картосхему максимальных высот стволов деревьев родов Каштан и Лиственница, с объемом ствола более 5м3.
library(tidyr)
library(dplyr)
library(sf)
library(ggplot2)

getwd()

# Считаем данные в переменные/Reading data into variables
greendb = read.csv("greendb.csv") 
map = sf::read_sf("moscow.geojson")

#График с заливкой
ggplot(map) + geom_sf(aes(fill = NAME)) + theme(legend.position = "none")

# Фильтруем данные для Каштан и Лиственница, с объемом ствола более 5м3.
spec = greendb$species_ru
genus = stringr::str_split(spec, pattern = " ", simplify = TRUE)[, 1]
data = greendb %>% mutate(Genus = genus)


vol = calculate_volume <- function(diameter, height) {
  radius <- diameter / 2
  volume <- pi * (radius^2) * height
  return(volume) # Volume in cubic meters
}
# Фильтрация по диаметру и группировка по региону и роду
max_height = data %>%
  group_by(adm_region, Genus,d_trunk_m) %>%
  filter(volume> 5) %>%  # Условие для диаметра ствола
  summarise(max_h = max(height_m, na.rm = TRUE), .groups = "drop") %>%  # Условия для родов
  filter(Genus %in% c("Каштан","Лиственница"))  # Условия для родов

# Преобразуем данные в широкий формат
max_height= pivot_wider(max_height, names_from = Genus, values_from = max_h)

# Объединяем данные с картой
map = map %>% mutate(adm_region = NAME)
map = left_join(map, vol, by = "adm_region")

# Построение картосхемы для Ели
ggplot(map) +
  geom_sf(aes(fill = `Лиственница`)) + 
  theme()
map=left_join(map,vol, by="adm_region")


ggplot(map)+
  geom_sf(aes(fill=`Каштан`))+theme()

