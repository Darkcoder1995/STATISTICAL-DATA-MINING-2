library(ISLR)
data("USArrests") 
df1 <- USArrests
head(df1)
nrow(df1)
pairs(df1)
library(philentropy)
dist1 <- dist(df1,method="euclidean")
hclust1 <- hclust(dist1,method="complete")
library(ggplot2)
library(ggdendro)
library(dplyr)
library(dendextend)
dend1 <- as.dendrogram(hclust1)
dend1 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
cutree1 <- cutree(dend1,3)
cutree1
clust1_1 <- names(which(cutree1==1))
clust1_2 <- names(which(cutree1==2))
clust1_3 <- names(which(cutree1==3))
library(RColorBrewer)
library(ape)
plot(as.phylo(hclust1), type = "phylogram", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree1],
     direction = "downwards", font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
is.na(df1)
m <- apply(df1,2,mean)
s <- apply(df1,2,sd)
z <- scale(df1,m,s)
dist3 <- dist(z,method="euclidean")
hclust3 <- hclust(dist3,method="complete")
dend3 <- as.dendrogram(hclust3)
dend3 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
cutree2 <- cutree(dend3,3)
cutree2
plot(as.phylo(hclust3), type = "phylogram", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree2],
     direction = "downwards", font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
clust2_1 <- names(which(cutree2==1))
clust2_2 <- names(which(cutree2==2))
clust2_3 <- names(which(cutree2==3))
clust2_1
clust2_2
clust2_3