##Tidy Tuesday Week #32
###Example modeled off of the example from:
#https://github.com/rfordatascience/tidytuesday/tree/master/data/2018-11-06

### Introduction----------------------------------------------------------------
#Load Required Packages
library(tidyverse)

#Import Turnout Dataset
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_32/us_wind.csv")

#View Data and Structure
View(df)
str(df)
summary(df)


#Subset Data for Visualization
#Select Subset of variables
#14:16 are turbine characteristics
#Turbine Characteristics Confidence
df1 <- df[,c(14:16, 22)]

#Select 250 random observations
df1 <- df1[sample(1:nrow(df1), 250,
                  replace=FALSE),]

#Remove all observations with "-9999"
df1[df1 == -9999] <- NA
df1 <- df1[complete.cases(df1), ]


#To create a basic scatterplot matrix just requires using the pairs function
pairs(df1[1:3]) #only quantitative variables

#Modified scatterplot matricies
require("RColorBrewer")
display.brewer.pal(4,"Pastel1") #display colorpalette

#Put Histograms on the diagonal (from "pairs" Help)
#Creating new function
panel.hist  <- function(x,...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0,1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}

#Re-run pairs using the panel.hist function
pairs(df1[1:3],
      panel = panel.smooth,
      main = "Scatterplot Maxtris for Turbine Characteristics \n Tidy Tuesday Week #32",
      diag.panel = panel.hist,
      pch = 16, 
      col = brewer.pal(4,"Pastel1")[df1$t_img_srce],
      oma=c(4,4,7,20))

legend(locator(), legend = levels(df1$t_img_srce), fill= brewer.pal(4,"Pastel1"))
