#Load Required Packages
library(tidyverse)

#Read CSV Data
ap <- read.csv("C:/temp/us-airports.csv")

#Explore data structure and basic summary
str(ap)
summary(ap)

#Group Data by region, state, and year
#Summarize data by mean rank, total passengers, average passengers
ap_group <- ap %>% 
  group_by(region, state, year, hub_type) %>%
  summarize(rank = mean(yearly_rank),
            rank_max = max(yearly_rank),
            rank_min = min(yearly_rank),
                passengers = sum(passengers),
                ave_passengers = mean(passengers))


#Reorder Hub Type Factors
ap_group$hub_type <- factor(ap_group$hub_type, levels = c("Large", "Medium", "Small", "Nonhub"))

#Plot data using ggplot
ggplot(na.omit(ap_group)) + 
  geom_col(aes(x=reorder(region,-passengers),y = passengers,
               fill = hub_type)) +
  facet_wrap(~year) + theme_classic() +
  labs(title = "Regional Airline Travel from 2012-2017",x = "Geographic Region", y = "Airline\nPassengers") +
  scale_fill_manual("Hub Type",values=c("#0868ac", "#2ca25f", "#fd8d3c","lightgrey")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))

#Geo_Facet Example
library(geofacet)
ggplot(ap_group, aes(year, passengers)) +
  geom_col() +
  facet_geo(~state, grid = us_state_grid2) +
  theme_bw()
