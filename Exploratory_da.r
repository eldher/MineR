# laticce
library(nlme)
library(lattice)

xyplot(weight ~ Time | Diet, BodyWeight)
llines(BodyWeight)


library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)

library(ggplot2)

qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))


# GGplot ------------------------------------

qplotlog()

g <- ggplot(maacs, aes(var1,var2), geom = c("point", "smooth"), method = "lm")

summary(g)

# with regression line
g <- g + geom_point() + geom_smooth(method = "lm")
# with facets
g + geom_point() + facet_grid(. ~ bmicat) + geom_smooth(method = "lm")
# ggplot plots labels automatically

# assign color and transparency
g + geom_point(aes(color = bmicat), size = 4, alpha = 1/2)

# for globally variables use theme()
theme(legend.position = "none")

theme_bw(base_family = "Times")

# customize labels
labs()

# A more difficult question
## calculate deciles of the data 
cutpoints <- quantile(maac$logno2_new, seq(0,1,length = 4), na.rm na.rm = TRUE)

## cut the data at the deciles and create a new factor variable
maacs$dec <- cut(maacs$logno2_new,cutpoints)

## see the level
levels(maacs$no2dec)

## setup ggplot with dataframe

g <- ggplot(maacs,aes(logpm25, NocturnalSympt))

g + geom_point(alpha = 1/3)
  + facet_wrap(bmicat ~ no2dec, nrow = 2, ncol = 4)
  + geom_smooth(method = "lm", se = FALSE, col="steelblue")
  + theme_bw(base_family = "Avenir", base_size = 10)
  + labs(x = expression("log "*PM[2.5]))
  + labs(y = "Nocturnal Symptoms")
  + labs(title = "Maacs Cohort")


# K means clustering
set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12,mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12,mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)

# plot clustered data with centroids
par(mar = rep(0.2,4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

# heatmap
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1,2), mar=c(2,4, 0.1, 0.1))
image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[,order(kmeansObj2$cluster)], yaxt = "n")

# Principal Component Analysis

set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

par(mar = rep(0.2, 4))
heatmap(dataMatrix)

# Now adding a pattern to the data
set.seed(678910)
for(i in 1:40){
  #flip a coin
  coinFlip <- rbinom(1,size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0, 3), each = 5)
  }
}

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

heatmap(dataMatrix)

# Patterns in rows and columns

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, xlab = "Row Mean", ylab= "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

dev.off()

# SVD and PCA are equivalent, the former just standarizing the data before plot.

# SVD

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1,3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])

plot(svd1$u[,1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[,1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular Value", pch = 19)
plot(svd1$d^2/sum(svd1$d), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

# Comparison with SVD and PCA
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[,1],svd1$v[,1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0,1))


install.packages("bioconductor")

# Imputing 
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data

svd1 <- svd(scale(dataMatrixOrdered))
svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1,2))
plot(svd1$v[,1], pch = 19)
plot(svd2$v[,1], pch = 19)


x <- rnorm(10000)
y <- rnorm(10000)
plot(x,y)
smoothScatter(x,y)
