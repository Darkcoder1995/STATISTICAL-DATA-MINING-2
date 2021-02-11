my_data <- read.delim("seeds_dataset.txt",header = TRUE)
head(my_data)
library(ggplot2)
library(ggdendro)
library(dplyr)
library(dendextend)
library(philentropy)
library(cluster)
library(RColorBrewer)
library(ape)
library(fossil)
m <- apply(my_data[,-8],2,mean)
s <- apply(my_data[,-8],2,sd)
z <- scale(my_data[,-8],m,s)
distance1 <- dist(z,method = "euclidean")
hclust1 <- hclust(distance1,method="single")
dend1 <- as.dendrogram(hclust1)
dend1 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
cutree1 <- cutree(dend1,3)
cutree1
plot(as.phylo(hclust1), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree1],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
hclust2 <- hclust(distance1,method="average")
dend2 <- as.dendrogram(hclust2)
dend2 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
cutree2 <- cutree(dend2,3)
cutree2
plot(as.phylo(hclust2), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree2],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
hclust3 <- hclust(distance1,method="complete")
dend3 <- as.dendrogram(hclust3)
dend3 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
cutree3 <- cutree(dend3,3)
cutree3
plot(as.phylo(hclust3), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree3],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
store1 <- c()
for (i in 2:10){
  ct <- cutree(hclust1,k=i)
  si <- silhouette(ct,dist = distance1)
  avg_width <- summary(si)$avg.width
  store1 <- c(store1,avg_width)
}
plot(store1)
store2 <- c()
for (i in 2:10){
  ct <- cutree(hclust2,k=i)
  si <- silhouette(ct,dist = distance1)
  avg_width <- summary(si)$avg.width
  store2 <- c(store2,avg_width)
}
plot(store2)
library(ggplot2)
store3 <- c()
for (i in 2:10){
  ct <- cutree(hclust3,k=i)
  si <- silhouette(ct,dist = distance1)
  avg_width <- summary(si)$avg.width
  store3 <- c(store3,avg_width)
}

plot(store3)
silhouette_score <- function(k){
  km <- kmeans(z, centers = k, nstart=10)
  ss <- silhouette(km$cluster, dist(z))
  mean(ss[, 3])
}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
library(factoextra)
fviz_nbclust(z, kmeans, method='silhouette')
ka2 <- kmeans(z,centers=3,nstart = 10)
rand.index(ka2$cluster,as.numeric(my_data[,8]))
adj.rand.index(ka2$cluster,as.numeric(my_data[,8]))
clusplot(z, ka2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
##K-MENOIDS
library(fpc)
kmed <- pamk(z)
kmed$nc
table(kmed$pamobject$clustering,my_data$Seed.Group)
layout(matrix(c(1,2),1,2))
plot(kmed$pamobject)
library(fpc)
kmed3 <- pamk(z,3)
table(kmed3$pamobject$clustering,my_data$Seed.Group)
layout(matrix(c(1,2),1,2))
plot(kmed3$pamobject)
library(bootcluster)
k.select(z,range=2:6,B=50,r=5,scheme_2=TRUE)
k.select(z,range=2:6,B=50,r=5,scheme_2=FALSE)
gap_kmeans <- clusGap(z,kmeans,nstart=20,K.max=10,B=100)
plot(gap_kmeans,main="Gap Statistics: K-Means")
gap_kmedoid <- clusGap(z,pam,K.max=10,B=100)
plot(gap_kmedoid,main="Gap Statistics: K-Medoid")