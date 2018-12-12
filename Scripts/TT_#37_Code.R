#Tidy Tuesday Week #37
#By Richard Johansen
#Twitter: @DataVizJohansen
#GitHub: RAJohansen

#load tidyverse!
library(tidyverse)

### Import and save data (Script from Tidy Tuesday Week 37) --------------------
#install.packages("janitor")
set.seed(20181209)
# You can use this url to download the data directly into R (will take a few seconds)
df <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

# Cleaning names with janitor, sampling 300,000 records, and dropping some variables
sampled_df <- df %>% 
  janitor::clean_names() %>%
  select(-phone, -grade_date, -record_date, -building, -street) %>% 
  sample_n(size = 300000)

# save the .csv
write_csv(sampled_df, "C:/R_Packages/TidyTuesday/Data/TT_37/nyc_restaurants.csv")

### Tidy Tuesday Week #37 ----------------------------------------------------

#Load data set 
df <- read_csv("C:/R_Packages/TidyTuesday/Data/TT_37/nyc_restaurants.csv")

#Explore structure and data 
str(df)
summary(df)

#Convert Boro, Cuisine Type, and grade to factor
df$boro <- as.factor(df$boro)
df$cuisine_description <- as.factor(df$cuisine_description)
df$grade <- as.factor(df$grade)
df$violation_code <- as.factor(df$violation_code)
df <- mutate(df,"Code_Num" = gsub("[[:alpha:]]","",df$violation_code))
df$Code_Num <- as.factor(df$Code_Num)
df$violation_description <- as.factor(df$violation_description)

#Subset data where grades are posted (A,B, or C)
df_grade <- df[df$grade == "A" | df$grade == "B" | df$grade == "C" ,]

df_MH <- df_grade[df_grade$boro == "MANHATTAN",]
df_rest <- df_grade[df_grade$boro != "Missing",]

df_food <- df_rest[df_rest$cuisine_description == "American" | df_rest$cuisine_description == "Barbecue" | df_rest$cuisine_description == "Chinese" |df_rest$cuisine_description == "French" |  df_rest$cuisine_description == "German" | df_rest$cuisine_description == "Greek" | df_rest$cuisine_description == "Indian" | df_rest$cuisine_description == "Italian" | df_rest$cuisine_description == "Middle Eastern" | df_rest$cuisine_description == "Scandinavian" | df_rest$cuisine_description == "Spanish" | df_rest$cuisine_description == "Thai",]

#Dodged Bar chart of 10 food places and mean scores by boro
ggplot(na.omit(df_food), aes(x =cuisine_description, y = score, fill = grade)) +
  stat_summary(fun.y="mean", geom="col", position = "stack") +
  facet_wrap(~boro) +
  labs(title= "Mean Inspection Scores for Popular Cuisine Types\n in the Five New York City Boroughs", x = "Cuisine Description", y= "Inspection Score\n (Mean)") +
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        axis.title.y = element_text(angle=0,vjust = 0.5),
        plot.title = element_text(hjust = 0.5))

