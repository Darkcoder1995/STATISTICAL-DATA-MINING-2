df1 <- read.csv("Ch10Ex11.csv", header = FALSE)
head(df1)
nrow(df1)
ncol(df1)
distance1 <- dist(cor(df1))
library(philentropy)
library(ggplot2)
library(ggdendro)
library(dplyr)
library(dendextend)
hclust1 <- hclust(distance1,method="single")
dend1 <- as.dendrogram(hclust1)
dend1 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
library(corrplot)
corrplot(cor(df1),method='color',order="hclust",hclust.method = 'single',tl.col = 'black', tl.cex = 0.7)
hclust2 <- hclust(distance1,method="complete")
dend2 <- as.dendrogram(hclust2)
dend2 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
corrplot(cor(df1),method='color',order="hclust",hclust.method = 'complete',tl.col = 'black', tl.cex = 0.7)
hclust3 <- hclust(distance1,method="average")
dend3 <- as.dendrogram(hclust3)
dend3 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
corrplot(cor(df1),method='color',order="hclust",hclust.method = 'average',tl.col = 'black', tl.cex = 0.7)
hclust4 <- hclust(distance1,method="centroid")
dend4 <- as.dendrogram(hclust4)
dend4 %>% set("branches_k_color") %>% 
  plot(main = "Default colors") %>%
  axis(side = 2,col = "#F38630",labels = TRUE) %>%
  mtext(col = "#A38630")
corrplot(cor(df1),method='color',order="hclust",hclust.method = 'centroid',tl.col = 'black', tl.cex = 0.7)
cutree1 <- cutree(dend1,2)
cutree1
library(RColorBrewer)
library(ape)
plot(as.phylo(hclust1), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree1],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
cutree2 <- cutree(dend2,2)
cutree2
plot(as.phylo(hclust2), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree2],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
cutree3 <- cutree(dend3,2)
cutree3
plot(as.phylo(hclust3), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree3],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
cutree4 <- cutree(dend4,2)
cutree4
plot(as.phylo(hclust4), type = "fan", cex = 0.6,
     tip.color = brewer.pal(3, 'Accent')[cutree4],
     font = 2,
     edge.color = 'steelblue', edge.width = 2, edge.lty = 2)
library(factoextra)
pcomp_df <- prcomp(t(df1))
fviz_eig(pcomp_df)
fviz_pca_ind(pcomp_df,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_var(pcomp_df,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(pcomp_df, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969" )
individual_coord <- pcomp_df$x[, 1:2]
k2 <- kmeans(individual_coord, centers = 2,nstart=10)
library(cluster)
clusplot(individual_coord, k2$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
total <- apply(pcomp_df$x, 1, sum)
top <- order(abs(total), decreasing = TRUE)
top[1:10]