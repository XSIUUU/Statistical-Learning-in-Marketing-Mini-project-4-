rm(list = ls(pattern = "con"))

library(plot3D)
install.packages("scatterplot3d")
library(scatterplot3d)
library(readxl)
library(MASS)
library(dplyr)

####

#S5065623 SLIM Project 4!!!

####

spread.df <- segment_5065623
str(spread.df)

# Overview of the essentials
summary(spread.df)
x <- spread.df$Taste[1:50]
y <- spread.df$Spreadability[1:50]
z <- spread.df$Appearance[1:50]
plot(x, xlab = "Respondent", ylab = "Taste", ylim = c(1,7), col = "red", pch = 16, main = "Taste")
plot(x, y, xlab = "Taste", ylab = "spreadability", 
     xlim = c(1,7), ylim = c(1,7), col = "red", pch = 16, main = "Taste & spreadability")

scatterplot3d(x, y, z, xlab = "Taste", ylab = "spreadability", zlab = "Appearance",
              xlim = c(1,7), ylim = c(1,7), zlim = c(1,7), color = "red", pch = 16, main = "Taste, spreadability, 
              & Appearance")


#### HCLUST


# distances using numeric columns

# first standardize the variables!

spread2.df <-spread.df
spread2.df[,6:12] <- data.frame(scale(spread.df[,6:12]))

d <- dist(spread2.df[,c(6:12)])     # dist function calculates Euclidean distance
as.matrix(d)[1:10, 1:10]

# now the real hclust() work
library(cluster)                  # daisy works with mixed data types, in case all variables are continuous, results are the same as with the dist function that is giving the 
# euclidean distance
spread2.dist <- daisy(spread2.df[,c(6:12)])
# inspect some of the results
as.matrix(spread2.dist)[1:10, 1:10]


# Hierarchical clustering of WARD
spread2e.hc <- hclust(spread2.dist, method="ward.D2")
plot(spread2e.hc)
spreadaggloe <- cbind(as.data.frame(spread2e.hc[1]),as.data.frame(spread2e.hc[2]))
spreadaggloe      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)
spreadaggloe[1:15,]      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)
spreadaggloe[485:499,]      # positive values of merge1 and merge2 refer to earlier formed clusters (in that specific row number), negative values to singletons (true observations)



# Construct the scree plots for the different solutions

spreadscreee <- sort(spreadaggloe[485:499,c(3)], decreasing = TRUE)
plot(spreadscreee, type="l", col="red", lwd=5, xlab="clusters", ylab="Cluster distance", main="Scree Plot", xaxt="n")
axis(1, at=seq(1,18,by = 1))

# We continue with the WARD method (fifth type)

# see hclust's proposal for 6 groups
plot(spread2e.hc)
rect.hclust(spread2e.hc, k=6, border="red")

# see hclust's proposal for 5 groups
plot(spread2e.hc)
rect.hclust(spread2e.hc, k=5, border="red")

# see hclust's proposal for 4 groups
plot(spread2e.hc)
rect.hclust(spread2e.hc, k=4, border="red")

# see hclust's proposal for 3 groups
plot(spread2e.hc)
rect.hclust(spread2e.hc, k=3, border="red")

# see hclust's proposal for 2 groups
plot(spread2e.hc)
rect.hclust(spread2e.hc, k=2, border="red")

###########

# HC of WARD ----

###########
# actually get 3 groups
spread2e.hc.segment <- cutree(spread2e.hc, k=3)     # membership vector for 3 groups
table(spread2e.hc.segment)
# what did hclust come up with? Use the standardized variables!
seg.summ <- function(data , groups) {aggregate(data , list(groups), function(x) mean(as.numeric(x)))}
#在这个地方aggregate()是用来处理用于对数据集进行分组和汇总。
#list可以根据groups的n个分组将数据集分为n个向量
#function(x)是匿名函数,匿名函数可以在需要时即定义并使用。在这里等于将list后的每个向量都走一遍.

seg.summ(spread2.df[,c(6:12)], spread2e.hc.segment)
spread2e3.hc.means <- seg.summ(spread2.df[,c(6:12)], spread2e.hc.segment)


# actually get 4 groups
spread2e.hc.4segment <- cutree(spread2e.hc, k=4)     # membership vector for 6 groups
table(spread2e.hc.4segment)
# what did hclust come up with? Use the standardized variables!
seg.summ(spread2.df[,c(6:12)], spread2e.hc.4segment)
spread2e4.hc.means <- seg.summ(spread2.df[,c(6:12)], spread2e.hc.4segment)

# PCA virtualization

library(RColorBrewer)

spread2e.hc.4segment <- cutree(spread2e.hc, k=4) 
pca <- prcomp(spread2.df[,c(6:12)])
pca_scores <- predict(pca, spread2.df[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = spread2e.hc.4segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "(ward)4 Cluster Plot with PCA Dimensions")

spread2e.hc.segment <- cutree(spread2e.hc, k=3) 
pca <- prcomp(spread2.df[,c(6:12)])
pca_scores <- predict(pca, spread2.df[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = spread2e.hc.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "(ward)3 Cluster Plot with PCA Dimensions")


###########

#Kmean ----

###########
# 3-cluster solution
set.seed(96743)
spread2.k3 <- kmeans(spread2.df[,c(6:12)], centers=3)
table(spread2.k3$cluster)
# inspect it
seg.summ(spread2.df[,c(6:12)], spread2.k3$cluster) ##调用前面,每簇平均值的公式
spread.k3.mean <- seg.summ(spread2.df[,c(6:12)], spread2.k3$cluster)
# plot one of the variables
boxplot(spread2.df$Taste ~ spread2.k3$cluster, ylab="Taste", xlab="Cluster")
# plot the result
spread2.k3.segment <- spread2.k3$cluster
pca <- prcomp(spread2.df[,c(6:12)])
pca_scores <- predict(pca, spread2.df[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = spread2.k3.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "(kmean) 3 Cluster Plot with PCA Dimensions")

# 4-cluster solution
set.seed(12335)
spread2.k4 <- kmeans(spread2.df[,c(6:12)], centers=4)
table(spread2.k4$cluster)
# inspect it
seg.summ(spread2.df[,c(6:12)], spread2.k4$cluster)
spread.k4.mean <- seg.summ(spread2.df[,c(6:12)], spread2.k4$cluster)
# plot one of the variables
boxplot(spread2.df$Taste ~ spread2.k4$cluster, ylab="Taste", xlab="Cluster")
# plot the result
spread2.k4.segment <- spread2.k4$cluster
pca <- prcomp(spread2.df[,c(6:12)])
pca_scores <- predict(pca, spread2.df[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = spread2.k4.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "(kmean) 4 Cluster Plot with PCA Dimensions")


# compare 3/4 cluster

mean(as.matrix(abs(spread2e3.hc.means - spread.k3.mean))) # diff of 3 cluster
mean(as.matrix(abs(spread2e4.hc.means - spread.k4.mean))) # diff of 4 cluster


#############

#Combine method----

#############
# Do K-means with the outcomes of the hierarchical cluster as starting values

# do it with 3 clusters
kmeanstart <- spread2e3.hc.means[,c(2:8)]
spread2.k3 <- kmeans(spread2.df[,c(6:12)],kmeanstart)
table(spread2.k3$cluster)

# inspect it
seg.summ(spread2.df[,c(6:12)], spread2.k3$cluster)

# plot of the variables
boxplot(spread2.df$Taste ~ spread2.k3$cluster, ylab="Taste", xlab="Cluster")
boxplot(spread2.df$Fattiness ~ spread2.k3$cluster, ylab="Fattiness", xlab="Cluster")
boxplot(spread2.df$Salt ~ spread2.k3$cluster, ylab="Salt", xlab="Cluster")
boxplot(spread2.df$Spreadability ~ spread2.k3$cluster, ylab="Spreadability", xlab="Cluster")
boxplot(spread2.df$Appearance ~ spread2.k3$cluster, ylab="Appearance", xlab="Cluster")
boxplot(spread2.df$Recycling ~ spread2.k3$cluster, ylab="Recycling", xlab="Cluster")
boxplot(spread2.df$Bio.content ~ spread2.k3$cluster, ylab="Bio.content", xlab="Cluster")

# plot the result
spread2.k3.segment <- spread2.k3$cluster
pca <- prcomp(spread2.df[,c(6:12)])
pca_scores <- predict(pca, spread2.df[,c(6:12)])
plot(pca_scores[, 1], pca_scores[, 2], col = spread2.k3.segment, pch = 16, cex = 2,
     xlab = "PC1", ylab = "PC2", main = "Cluster Plot with PCA Dimensions")

# Test for significance
clusmember <- as.factor(spread2.k3.segment)
spread2eaovbase <- cbind(clusmember,spread2.df[,c(3,6:12)])

## sig of taste
taste.aov <- aov(Taste ~ clusmember, data = spread2eaovbase)
summary(taste.aov)
TukeyHSD(taste.aov)

## sig of fattiness
fattiness.aov <- aov(Fattiness ~ clusmember, data = spread2eaovbase)
summary(fattiness.aov)
TukeyHSD(fattiness.aov)

## sig of salt
salt.aov <- aov(Salt ~ clusmember, data = spread2eaovbase)
summary(salt.aov)
TukeyHSD(salt.aov)

## sig of spreadability
spreadability.aov <- aov(Spreadability ~ clusmember, data = spread2eaovbase)
summary(spreadability.aov)
TukeyHSD(spreadability.aov)

## sig of appearance
appearance.aov <- aov(Appearance ~ clusmember, data = spread2eaovbase)
summary(appearance.aov)
TukeyHSD(appearance.aov)

## sig of recycling
recycling.aov <- aov(Recycling ~ clusmember, data = spread2eaovbase)
summary(recycling.aov)
TukeyHSD(recycling.aov)

## sig of bio
bio.aov <- aov(Bio.content ~ clusmember, data = spread2eaovbase)
summary(bio.aov)
TukeyHSD(bio.aov)


# Age distribution in different cluster
age.aov <- aov(Age ~ clusmember, data = spread2eaovbase)
summary(age.aov)
TukeyHSD(age.aov)


# How about the categorical variables? Education for instance.


gender.freq <-table(clusmember, spread2.df[,c(2)])
gender.freq
gender.chisq <- chisq.test(gender.freq)
gender.chisq
education.freq <-table(clusmember, spread2.df[,c(4)])
education.freq
education.chisq <- chisq.test(education.freq)
education.chisq
area.freq <-table(clusmember, spread2.df[,c(5)])
area.freq
area.chisq <- chisq.test(area.freq)
area.chisq




