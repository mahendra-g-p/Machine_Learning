library(stats)  ## for dist
library(NbClust)
library(cluster)
library(mclust)
library(amap)  ## for using Kmeans (notice the cap K)
library(factoextra) ## for cluster vis, silhouette, etc.
library(purrr)
library(philentropy)  ## for distance() which offers 46 metrics
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(stringr)
library(wordcloud)


setwd("/Users/mahendra_g_p/Downloads/Academic/Spring_2023/Machine_Learning/Project")
Record_3D_DF<-read.csv("Shipping_Details_Sub_Data_Clustering.csv")

head(Record_3D_DF)
str(Record_3D_DF)

## remove column 1
Record_3D_DF <- Record_3D_DF[ ,-c(1) ]
head(Record_3D_DF)

Record_3D_DF <- sample_n(Record_3D_DF,100)
Dist2<- dist(Record_3D_DF, method = "minkowski", p=2) #Euclidean

## Create a normalized version of Record_3D_DF
Record_3D_DF_Norm <- as.data.frame(apply(Record_3D_DF[,1:4 ], 2, ##2 represents operations column wise
                                         function(x) (x - min(x))/(max(x)-min(x))))

## Look at scaled distances
Dist_norm<- dist(Record_3D_DF_Norm, method = "minkowski", p=2) #Euclidean



## Using Man with Ward.D2..............................
dist_C <- stats::dist(Record_3D_DF_Norm, method="manhattan")
HClust_Ward_CosSim_N_3D <- hclust(dist_C, method="ward.D2")
plot(HClust_Ward_CosSim_N_3D, cex=.7, hang=-30,main = "Manhattan")
rect.hclust(HClust_Ward_CosSim_N_3D, k=2)



# compute cosine similarity
similarity_matrix <- tcrossprod(scale(Record_3D_DF_Norm, center = TRUE, scale = TRUE))

# perform hierarchical clustering using cosine similarity
hclust_results <- hclust(as.dist(1 - similarity_matrix), method = "ward.D2")

# plot the dendrogram
plot(hclust_results, main = "Hierarchical Clustering using Cosine Similarity")
rect.hclust(hclust_results, k=2)
