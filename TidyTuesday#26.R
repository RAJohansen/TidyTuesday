### Tidy Tuesday Week 26 (September 25 2018)
library(raster)                               
library(tidyverse)
library(maps)

df_invasion <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_26/table_1.csv")
df_cost <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_26/table_2.csv")
df <- merge(df_cost,df_invasion,by="country")

colnames(df) <- c("Country","Cost", "Rank_Cost", "Rank_Threat", "Threat")
df$region <- df$Country

world <- map_data("world")

choro <- merge(world, df, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Cost)) +
  coord_map(projection = "mercator") + 
  theme_minimal()



ggplot(choro, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = Rank_Cost*Rank_Threat)) +
  coord_map(projection = "mercator") + 
  theme_minimal()
