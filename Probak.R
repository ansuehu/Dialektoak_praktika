
# setwd("/home/andoni/Downloads/Praktika Euskara Dialektoak-20241128T082756Z-001/Praktika Euskara Dialektoak") 
library(cluster)

dist_matrizea <- read.csv("./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/Matriz-distancias del ESB-Diatech z-24-04-02.csv", row.names=1)
datubasea <- read.csv("./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/ESB-datu-basea-taula.csv", row.names=1)

dist_matrizea <- data.matrix(dist_matrizea)
d <- as.dist(dist_matrizea)
#mds.coor <- cmdscale(d)
fanny_emaitza <- fanny(d, 10,  diss = TRUE, memb.exp = 1.5)
fanny_emaitza

as.list(fanny_emaitza$membership)
clusters <- as.list(fanny_emaitza$clustering)


datubase_matrix <- as.matrix(datubasea)

datubasea['X0']

colnames(membership) <- paste0('dimention_', 1L:145)
clusters['dimention_5']
#plot(fanny_emaitza)

# write.csv(fanny_emaitza$membership, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_membership_3.csv", row.names = FALSE)

# # Exportar los clusters asignados
# write.csv(fanny_emaitza$clustering, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_clusters_3.csv", row.names = FALSE)

kalkulatu_estabilitatea <- function(datubasea, membership, galdera, cluster) {
  c <- cluster[[1]]
  elementuak <- cluster[[2]]
  n <- length(elementuak)
  
  batura <- 0
  pisuen_batura <- 0
  
  for (i in 1:n) {
    for (j in (i+1):n) {
      if (is.na(datubasea[galdera, i]) || is.na(datubasea[galdera, j])) {
        distantzia <- 1
      } else {
        distantzia <- bilbao_d(strsplit(datubasea[galdera, i], ",")[[1]], strsplit(datubasea[galdera, j], ",")[[1]])
      }
      
      pisuak <- membership[i, c] * membership[j, c]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  return(batura * 2 / pisuen_batura)
}

kalkulatu_bariabilitatea <- function(datubasea, membership, galdera, cluster1, cluster2) {
  c1 <- cluster1[[1]]
  elementuak1 <- cluster1[[2]]
  n1 <- length(elementuak1)
  
  c2 <- cluster2[[1]]
  elementuak2 <- cluster2[[2]]
  n2 <- length(elementuak2)
  
  batura <- 0
  pisuen_batura <- 0
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      if (is.na(datubasea[galdera, i]) || is.na(datubasea[galdera, j])) {
        distantzia <- 1
      } else {
        distantzia <- bilbao_d(strsplit(datubasea[galdera, i], ",")[[1]], strsplit(datubasea[galdera, j], ",")[[1]])
      }
      
      pisuak <- membership[i, c1] * membership[j, c2]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  return(batura / pisuen_batura)
}

kalkulatu_diferentziazioa <- function(datubasea, membership, galdera, cluster1, cluster2) {
  estabilitatea1 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster1)
  estabilitatea2 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster2)
  bariabilitatea1_2 <- kalkulatu_bariabilitatea(datubasea, membership, galdera, cluster1, cluster2)
  
  return(bariabilitatea1_2 / max(estabilitatea1, estabilitatea2, 1e-6))
}
