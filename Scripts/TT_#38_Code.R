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

#Plot Number of Cetacean Birth Types by species from 1938-2017

jpeg("C:/R_Packages/TidyTuesday/Jpegs/TT_#39.jpeg", width = 20, height = 16, units = 'in', res = 300)

ggplot(df1) +
  geom_histogram(aes(birthYear, fill = acquisition), binwidth =5, position = position_dodge2(preserve = "single")) +
  facet_wrap(~species) + 
  scale_y_log10() +
  theme_classic() +
  labs(title= "Cetacean Birth Types by species from 1938-2017", x = "Animal's Birth Year", y= "Count") +
  theme(axis.title.y = element_text(angle=0,vjust = 0.5),
        plot.title = element_text(hjust = 0.5))
  
#ggsave("C:/R_Packages/TidyTuesday/Jpegs/TT_#39.jpeg", units="in", width=20, height=16, dpi=300, compression = 'lzw')
dev.off()
