#Tidy Tuesday Week #38
#By Richard Johansen
#Twitter: @DataVizJohansen
#GitHub: RAJohansen

#load tidyverse!
library(tidyverse)


# Load Data
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_38/allCetaceanData.csv")

#Explore structure and data 
str(df)
summary(df)

#Remove all rows where birth year is NA
df1 <- df[!is.na(df$birthYear),]

#Convert birth year from factor to numeric
df1$birthYear <- as.numeric(as.character(df1$birthYear))

#Plot Number of animals for each gender by species over time
ggplot(df1) +
  geom_histogram(aes(birthYear, fill = sex), binwidth =3, position = position_dodge2(preserve = "single")) +
  facet_wrap(~species) + 
  scale_y_log10() +
  theme_classic()
