library(data.table)
data <- iris[1:3]

algos <- list("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")

results <- 
  lapply(algos, function(algo) kmeans(data, centers = 2, nstart=5, algorithm = algo))

# clusters es una lista de 4 objetos resultados de kmeans

# Hartingan-Wong"
results[[1]]$centers[1,]
results[[1]]$betweenss
results[[1]]$withinss
results[[1]]$totss
results[[1]]$tot.withinss

#Lloyd"
results[[2]]$centers

# calculo del centro total
global_center <- colMeans(data) 
sumOfSquares <- apply(data,1, function(params)
  (params[1] - global_center[1])^2 + (params[2] - global_center[2])^2 + (params[3] - global_center[3])^2)  
totalss <- sum(sumOfSquares)


calcularIntraClase <- function(n){

  DT <- cbind(data, clust = results[[n]]$cluster)
  DT <- as.data.table(DT)
  
  print(paste0("-----",algos[[n]]))

  data_c1 <- DT[clust==1 ,c(1:3), with = FALSE]  # tabla con solo datos, sin el ID cluster. Cluster 1
  cluster_1 <- apply(data_c1, 1, function(params) (params - results[[1]]$centers[1,])^2)
  s1 <- sum(cluster_1)

  
  data_c2 <- DT[clust==2 ,c(1:3), with = FALSE] # tabla con solo datos, sin el ID cluster. Cluster 2
  cluster_2 <- apply(data_c2, 1, function(params) (params - results[[2]]$centers[1,])^2)

  s2 <- sum(cluster_2)
  print(paste0("Cluster1: ",s1," cluster2: ",s2," Suma: ",s1+s2))
}

exec <- lapply(c(1:4), calcularIntraClase)
