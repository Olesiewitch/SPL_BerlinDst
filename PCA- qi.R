---
title: "PCA"
author: "Wu Qi"
date: "3/3/2019"
---
#packages:
install_github("vqv/ggbiplot")
#error: "failed to set default locale"
#solution: system('defaults write org.R-project.R force.LANG en_US.UTF-8') -> restart R
library(ggbiplot)

packages = c("styler","devtools","factoextra","ggthemes","readr",
             "ggplot2","ggrepel","purrr") 
for (i in packages) {                          
    if(!require(i, character.only=TRUE))
    {install.packages(i, character.only=TRUE)}
    library(i, character.only=TRUE)
}

#principle component 

data.pca <- prcomp(IndexFull[,-(1:2)],center = TRUE,scale. = TRUE)
#centure: whether the variables should be shifted to be zero centered
#scale: whether the variables should be scaled to have unit variance before the analysis takes place
summary(data.pca)  # Importance of components 
str(data.pca)
#You obtain 12 principal components, each of these explains a percentage of the total variation in the dataset.

fviz_eig(data.pca)  # Extract and visualize the eigenvalues/variances of dimensions,Show the percentage of variances explained by each principal component.

fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(data.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

ggbiplot(data.pca)

#=================================Clustering====================================


options(digits=6) 
# Physical1; Physical2; Social; Economical; and enviromental Index as well as the respectively weighted Index

Index = read.csv("SPL_BerlinDst_Liv_Index_Calc/SPL_BerlinDst_Liv_Index.csv",
         sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)

#Full data set with normalised data as individual index
IndexFull = read.csv("SPL_BerlinDst_Liv_Index_Calc/IndexSoreData.csv",
                 sep = ";", dec = ",", row.names = 1, stringsAsFactors = FALSE)


ggplot(Index, aes(Phys1INDEX, Phys2INDEX), color = Index$District) + 
    theme_bw() + geom_point() + 
    theme(#panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# k mean clustering 
k = kmeans(Index[,-as.numeric(which(colnames(Index) == 'District'))], 
           centers = 2, # we set cluster number to be 2 first, to see if there would be an east-west Berlin cluster
           iter.max = 20, nstart = 12, 
           algorithm = "Hartigan-Wong",trace=FALSE)

kFull = kmeans(IndexFull[,-(1:2)], 
           centers = 2, iter.max = 20, nstart = 12, 
           algorithm = "Hartigan-Wong",trace=FALSE)
str(k)
str(kFull)

# plot the data points according to the first two principal components that explain the majority of the variance.

fviz_cluster(kFull, data = IndexFull[,-(1:2)], 
             geom = c("point", "text")
             #, label = as.vector(IndexFull$District)
             )  + geom_text_repel(aes(label=IndexFull$District),size=3.5) + 
             theme_bw()  + 
             theme(#panel.border = element_blank(),
             panel.grid.minor = element_blank(),
             panel.grid.major = element_blank())


#sensitivity analysis: if we change the weight of different index, how is the result gonna change
#calculate the new living Index according to principle component

#now we try to find the Determining Optimal Cluster numbers 
df = IndexFull[,-(1:2)]
wss = function(k) {
    kmeans(df, centers = k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values = 1:10

# extract wss for 2-15 clusters
wss_values = map_dbl(k.values, wss) #Apply a function to each element of a vector

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

fviz_nbclust(df, kmeans, method = "wss")




