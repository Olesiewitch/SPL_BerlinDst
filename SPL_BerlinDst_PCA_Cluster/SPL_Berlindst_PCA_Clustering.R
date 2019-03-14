#title: "cluster and PCA"
#author: "Wu Qi"
#date: "3/3/2019"
#========================= Set working directory================================

setwd("~/SPL_BerlinDst")

#=================================install packages==============================
install_github("vqv/ggbiplot")
# error: "failed to set default locale"
# solution: system('defaults write org.R-project.R force.LANG en_US.UTF-8'),
# and then restart R
install.packages("devtools")
install.packages("factoextra")
install.packages("ggthemes")
install.packages("readr")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("purrr")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("cluster")
install.packages("xlsx")

library(ggbiplot)
library(devtools)
library(factoextra)
library(ggthemes)
library(readr)
library(ggplot2)
library(ggrepel)
library(purrr)
library(ggpubr)
library(dplyr)
library(cluster)
library(xlsx)

#========================= Set working directory================================

setwd("~/SPL_BerlinDst")

#==============================Import the data==================================
options(digits=6) 

# Physical1; Physical2; Social; Economical; enviromental Index 
# and the respectively weighted Index

Index = read.csv(paste0("SPL_BerlinDst_Liv_Index_Calc/",
                        "SPL_BerlinDst_Liv_Index.csv"),
                 sep = ";", dec = ",", row.names = 1, 
                 stringsAsFactors = FALSE)
View(Index)
#Full data set with normalised data as individual index
IndexFull = read.csv("SPL_BerlinDst_Liv_Index_Calc/Index_Score_Data.csv",
                     sep = ";", dec = ",", row.names = 1, 
                     stringsAsFactors = FALSE)
View(IndexFull)

#=============================principle component===============================

data.pca = prcomp(select(IndexFull,ends_with('Scr')),
                  center = TRUE,
                  scale = TRUE)
#centure: whether the variables should be shifted to be zero centered
#scale: whether the variables should be scaled to have unit variance 
summary(data.pca)  # Importance of components 

# each component explains a percentage of the total variation in the dataset.

# Extract and visualize the eigenvalues/variances of dimensions,
# Show the percentage of variances explained by each principal component.
fviz_eig(data.pca)  + 
  theme_bw()  + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

# Graph of observations(individuals), use the first 2 Principle compoonent 
# to visualize the 'distance' between our 12 observisions
p1 = fviz_pca_ind(data.pca,
                  col.ind = "cos2", # Color by the quality of representation
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE) +     # Avoid text overlapping
                  geom_text_repel(aes(label=IndexFull$District),size=3.5)+ 
                  theme_bw()  + 
                  theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.border = element_blank())

# Graph of variables. Positive correlated variables point to the same side  
# Negative correlated variables point to opposite sides of the graph.
# chosed only 10 most important variables for visualization
p2 = fviz_pca_var(data.pca,
             col.var = "contrib", # Color by contributions to the PC
             select.var = list(contrib = 10), 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)+      # Avoid text overlapping
             theme_bw()  + 
             theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank())

#Graph of variables and observations with only the first 10 important variables
p3 = fviz_pca_biplot(data.pca, label="var",
                col.var = "contrib",
                select.var = list(contrib = 10),
                repel = TRUE,
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))  +
                geom_text_repel(aes(label=IndexFull$District),size=3.5)+ 
                theme_bw()  + 
                theme(panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank())

#Plot all graphs
ggarrange(p1,p2)
p3

# get a closer look at the diffrent level of contributions of individuals
fviz_contrib(data.pca, choice = "ind", axes = 1) + 
  coord_flip() + 
  geom_text_repel(aes(label=IndexFull$District))+ 
  theme_bw()  + 
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank())

#=========================prepare data for Clustering===========================

# select colomn of 5 original Index, i.e. not weighted 
# exclude Total Index, since it is just weighted sum of Index
CIndex = select(Index,ends_with('In'))[,-grep('TotalIn', 
                                      colnames(select(Index,ends_with('In'))))]

# select column of 4 weighted Index, which are weighted with 
# phys1W = 0.10, phys2W = 0.15,socW = 0.25, ecoW = 0.25, envW = 0.25
CPilar = select(Index,ends_with('Pl'))

# every column in Index
CInPi = Index[,-grep('District',colnames(Index))]

# select column from the full Index dataset, the score of each variable
# exclude the first 2 column with district names and number
CFull = select(IndexFull,ends_with('Scr'))  

CGropu = c(CIndex,CPilar,CInPi,CFull)

# have a look at the distance Matrix
get_dist(CIndex, method = "euclidean")
get_dist(CPilar, method = "euclidean")
get_dist(CInPi, method = "euclidean")
get_dist(CFull, method = "euclidean")

# visualization of distance Matrix
fviz_dist(get_dist(CFull), 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


#========================Optimal Cluster numbers================================

#Average Silhouette Method

AvgSil = function(k, df) {
    # Function computes the average Silhouette width 
    # The optimal number of clusters k is the one that maximizes 
    # the average silhouette over a range of possible values for k
    #
    # Arg: 
    # k is number of clusters; df is the dataframe that we apply kmean cluster
    #
    # Output: 
    # mean value of silhouette width for one specific k value
    km = kmeans(df, centers = k, nstart = 25)
    s = silhouette(km$cluster, dist(df)) 
    # silhouette : compute Silhouette Information from Clustering 
    # returns 3 columns : cluster, neighbor, sil_width
    mean(s[, 'sil_width'])  
}

k = c(2:10)  # for function 'silhouette' we need cluster number to be at least 2
df = CIndex
#df = CPilar
#df = CInPi
#df = CFull
AvgSil_values = sapply(k, FUN = AvgSil, df)
k[which(AvgSil_values == max(AvgSil_values))]  # optimal number of cluster is 2

fviz_nbclust(CIndex, kmeans, method = "silhouette") + theme(title = element_blank())

# Visualizing the Optimal Number of Clusters
f1 = fviz_nbclust(CIndex, kmeans, method = "silhouette")  # optimal cluster is 2
f2 = fviz_nbclust(CPilar, kmeans, method = "silhouette") # optimal cluster is 2
f3 = fviz_nbclust(CInPi, kmeans, method = "silhouette") # optimal cluster is 4
f4 = fviz_nbclust(CFull, kmeans, method = "silhouette") # optimal cluster is 2

ggarrange(f1,f2,f3,f4,labels = c('1','2','3','4')) 

#================================k mean clustering============================== 

kmCIndex = kmeans(CIndex, 
            centers = 2, # we set cluster number to be 2 first, 
            # to see if there would be an east-west Berlin cluster
            iter.max = 20, nstart = 12, 
            algorithm = "Hartigan-Wong",trace=FALSE)

kmCFull = kmeans(CFull, 
           centers = 2, 
           iter.max = 20, nstart = 12, 
           algorithm = "Hartigan-Wong",trace=FALSE)

kmCInPi = kmeans(CInPi, 
                 centers = 4, 
                 iter.max = 20, nstart = 12, 
                 algorithm = "Hartigan-Wong",trace=FALSE)

kmCPilar = kmeans(CPilar, 
                  centers = 2, 
                  iter.max = 20, nstart = 12, 
                  algorithm = "Hartigan-Wong",trace=FALSE)


cluster = cbind(Index$District, kmCIndex$cluster,kmCFull$cluster,
                kmCInPi$cluster,kmCPilar$cluster)
colnames(cluster) = c('dis','unweighted Index','Full Score',
                      'Index+ weighted Index','weigted Index')

#write.csv(cluster,'cluster.csv')

# plot the data points according to the first two principal components 
# that explain the majority of the variance

colnames = c('dis',
             'unweighted Index',
             'Full Score',
             'Index+ weighted Index',
             'weigted Index')

pc1 = fviz_cluster(kmCFull, data = CFull, 
             geom = c("point", "text")) + 
             geom_text_repel(aes(label=IndexFull$District),size=3.5) + 
             theme_bw()  + 
             theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank())+
             ggtitle("cluster based on full score")

pc2 = fviz_cluster(kmCIndex, data = CIndex, 
             geom = c("point", "text")) + 
             geom_text_repel(aes(label=IndexFull$District),size=3.5) + 
             theme_bw()  + 
             theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank())+
             ggtitle("cluster based on unweighted 5 Index")

pc3 = fviz_cluster(kmCInPi, data = CInPi, 
             geom = c("point", "text")) + 
             geom_text_repel(aes(label=IndexFull$District),size=3.5) + 
             theme_bw()  + 
             theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank()) +
             ggtitle("cluster based on total Index")

pc4 = fviz_cluster(kmCPilar, data = CPilar, 
             geom = c("point", "text")) + 
             geom_text_repel(aes(label=IndexFull$District),size=3.5) + 
             theme_bw()  + 
             theme(panel.grid.minor = element_blank(),
             panel.grid.major = element_blank(),
             panel.border = element_blank()) + 
             ggtitle("cluster based on Pilars(weighted Index)")

ggarrange(pc1,pc2,pc3,pc4)
pc1




