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
df$Turnout <- df$votes/df$eligible_voters
df$region <- as.character(df$state)

#Create Cartogram
states <- map_data("state")
voters_state <- sp::merge(states, df, by='region', all.x = TRUE)
vs_sf = st_as_sf(voters_state)
US_Cartogram = cartogram_cont(voters_state, "votes", maxSizeError = 1.5)
plot(world_carto1["population"])

#Create voter turnout percetage
df$Turnout <- df$votes/df$eligible_voters

ggplot(df) +
  geom_point(aes(year, Turnout), color = "#6495ED") +
  geom_smooth(aes(year, Turnout), se = FALSE, color = "pink") + 
  facet_geo(~state, grid = us_state_grid2) +
  theme_classic() +
  labs(title = "Voter Turnout ",
       subtitle = "",
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
