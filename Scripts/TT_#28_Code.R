#Load Required Packages
library(tidyverse)
library(geofacet)
library(maps)
library(raster)
library(cartogram)  

#Import Turnout Dataset
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_28/voter_turnout.csv")

#View Data and Structure
View(df)
str(df)

#Create voter turnout percetage
df$Turnout <- (df$votes/df$eligible_voters) *100

ggplot(df) +
  geom_point(aes(year, Turnout), color = "lightgrey") +
  geom_smooth(aes(year, Turnout), se = FALSE, color = "#6495ED") + 
  facet_geo(~state, grid = us_state_grid2) +
  theme_classic() +
  labs(title = "Tidy Tuesday #28",
       subtitle = "United States Voter Turnout 1980-2014",
       x = "Year",
       y = "Voter Turnout\n(%)",
       caption = "Data from: data.world") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.y = element_text(angle = 0, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))
