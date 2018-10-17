##Tidy Tuesday Week #29
###Example modeled off of the example from:
# https://www.r-bloggers.com/the-grammar-of-graphics-and-radar-charts/

#Load Required Packages
library(tidyverse)
library(scales)
library(reshape2)
library(tibble)
#Import Turnout Dataset
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_29/grads.csv")

#View Data and Structure
View(df)
str(df)

#Group By Major_category and summarize all variables by mean
df_major <- df %>% group_by(Major_category) %>% summarise_all(funs(mean))

#Select subset of a few major variables 
#Major_Categorgy, Men, Women, Employed, Median Income
df_major <- df_major[,c(1,6,7,10,16)]

#Reshape data 
df_major <-  df_major %>%
  mutate_each(funs(rescale), -Major_category) %>% 
  melt(id.vars=c('Major_category'), measure.vars=colnames(df_major)) %>% 
  arrange(Major_category)

# Remove rows that the variable is major category (error)
df_major<-df_major[!(df_major$variable=="Major_category"),]

#Convert Value to numeric
df_major$value <- as.numeric(df_major$value)

#Plot coord_polar chart
df_major %>%
  ggplot(aes(x=variable, y=value, group=Major_category, color=Major_category)) + 
  geom_polygon(fill=NA) +
  theme_bw() +
  coord_polar() + facet_wrap(~ Major_category) 

