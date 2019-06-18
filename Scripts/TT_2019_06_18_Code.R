#Tidy Tuesday Date 2019-06-18
#By Richard Johansen
#Twitter: @Johansen_PhD
#GitHub: RAJohansen

#load tidyverse!
library(tidyverse)

# Load Data
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_2019_06_18/bird_counts.csv")

# Explore year and species grouping
df %>% group_by(year, species) %>% 
  summarise(count_mean = mean(how_many_counted),
            count_sum = sum(how_many_counted))

# Still too many individual observations
# Group by just year or just species

df %>% group_by(year) %>% 
  summarise(count_mean = mean(how_many_counted),
            count_sum = sum(how_many_counted))

df %>% group_by(species) %>% 
  summarise(count_mean = mean(how_many_counted),
            count_sum = sum(how_many_counted))

# Create time series of counts
lims <- as.Date(strptime(c("1921-06-17","2018-06-19"), format = "%Y-%m-%d"))    

df$year <- as.character(df$year)
df$year <- as.Date(df$year, "%Y")
df %>% group_by(year) %>% 
  summarise(count_mean = mean(how_many_counted),
            count_sum = sum(how_many_counted)) %>% 
ggplot(aes(year,count_mean))+
  geom_point(size = 2)+
  geom_smooth(se =FALSE) +
  labs(title = "Christmas Bird Counts for \nHamilton, Ontario, Canada",y = "Bird Counts\n(mean)", x = "\n Year") +
  scale_x_date(date_labels ="%Y" , date_breaks = "6 year", limits =lims) +
  scale_y_continuous(name="Cumulative\nTotal", breaks = seq(0,600, by = 100)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x=element_text(angle=0,vjust = 0.5,size = 12),
        axis.text.y=element_text(size = 12),
        axis.title.y=element_text(angle = 0,size = 12, vjust = 0.5,face="bold"),
        axis.title.x=element_text(face="bold"))


jpeg("jpegs/Xmas_Bird_Counts_2019_06_18.jpeg", width = 12, height = 8, units = 'in', res = 600)
dev.off()

