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

### Create hierarchical clustering using a Dendrogram --------------------------
#Select Subset of variables
# 8 = Project Name
#12:21 are turbine characteristics
df1 <- df[,c(8,12:19)]

#Select 100 random observations
df1 <- df1[sample(1:nrow(df1), 100,
                 replace=FALSE),]

#Calculate distance between observations
d <- dist(df1)
#Large values = large distance between cars (disimilar)
#Small value = small distance between cars (similar)

#Next we use a hierarchical clustering tool using this distance matrix
#Default euclidean distance
c <- hclust(d)
c

#plot the dendrogram for clusters
plot(c)

#Draw boxes around the clusters in the dendrogram
rect.hclust(c, k = 3, border = "blue")
rect.hclust(c, k = 5, border = "green4")

### k-means clustering --------------------------------------------------------
#Subset df1 to remove all non-numeric values
df2 <- df1[,c(4:9)]
#Calculate kmeans on df2 for 3 clusters
km <- kmeans(df2, 3)
km

#Graph the k-means clustering
require(cluster)
clusplot(df2,
         km$cluster,
         color = TRUE,
         #shade = TRUE,
         lines = 3, # Lines connecting centroids
         labels = 2) # Labels clusters and cases

#Calculate kmeans on df2 for 5 clusters
km1 <- kmeans(df2, 5)
km

#Graph the k-means clustering
require(cluster)
clusplot(df2,
         km1$cluster,
         color = TRUE,
         #shade = TRUE,
         lines = 3, # Lines connecting centroids
         labels = 2) # Labels clusters and cases
