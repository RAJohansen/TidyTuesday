### Tidy Tuesday Week 26 (September 25 2018)
library(raster)                               
library(tidyverse)
library(tmap)
library(sp)

df_invasion <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_26/table_1.csv")
df_cost <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_26/table_2.csv")
df <- merge(df_cost,df_invasion,by="country")
colnames(df) <- c("name","Cost", "Rank_Cost", "Rank_Threat", "Threat")

World_Pest <- sp::merge(World, df, by ="name", duplicateGeoms = TRUE)

tm_shape(World_Pest) +
  tm_fill(col = "Threat", n = 10, colorNA = "white") +
  tm_borders("grey50") +  
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_layout(frame = FALSE)

tm_shape(World_Pest) +
  tm_fill(col = "Cost", n = 10, colorNA = "white") +
  tm_borders("grey50") +  
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_layout(frame = FALSE)
