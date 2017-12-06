library(data.table)
data <- iris[1:3]


algos <- list("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")

results <- 
  lapply(algos, function(algo) kmeans(data, centers = 2, nstart=5, algorithm = algo))


# clusters es una lista de 4 objetos resultados de kmeans
# los centros los ves asi

# Hartingan-Wong"
results[[1]]$centers

#Lloyd"
results[[2]]$centers

# etc.....


# Y los clusters a los cuales pertenecen 

# Hartingan-Wong"
results[[1]]$cluster

#Lloyd"
results[[2]]$cluster



calcularIntraClase <- function(n){

 #n=1
  DT <- cbind(data, clust = results[[n]]$cluster)
  DT <- as.data.table(DT)
  
  # 
  # results[[1]]$centers[1,][2] 2da componente del centro del primer cluster, del primer algoritmo
  # results[[1]]$centers[2,] centro del segundo cluster con el primer algoritmo
  
  print(paste0("-----",algos[[n]]))
  

  data_c1 <- DT[clust==1 ,c(1,2,3), with = FALSE]  # tabla con solo datos, sin el ID cluster. Cluster 1
  
  
  cluster_1 <- apply(data_c1, 1, function(params) 
                    (params[1] - results[[1]]$centers[1,][1])^2 + 
                    (params[2] - results[[1]]$centers[1,][2])^2 +
                    (params[2] - results[[1]]$centers[1,][3])^2)
  
  print(mean(cluster_1))
  
  
  data_c2 <- DT[clust==2 ,c(1,2,3), with = FALSE] # tabla con solo datos, sin el ID cluster. Cluster 2
  
  
  cluster_2 <- apply(data_c2, 1, function(params) 
    (params[1] - results[[2]]$centers[1,][1])^2 + 
    (params[2] - results[[2]]$centers[1,][2])^2 +
    (params[2] - results[[2]]$centers[1,][3])^2)
  
  print(mean(cluster_2))
  
}

sapply(c(1:4), calcularIntraClase)
  
  
