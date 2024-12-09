# setwd("/home/andoni/Downloads/Praktika Euskara Dialektoak-20241128T082756Z-001/Praktika Euskara Dialektoak") 
library(cluster)
#install.packages('hash')
library(hash)

dist_matrizea <- read.csv("./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/Matriz-distancias del ESB-Diatech z-24-04-02.csv", row.names=1)
datubasea <- read.csv("./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/ESB-datu-basea-taula.csv", row.names=1)

# datubasea
dist_matrizea <- data.matrix(dist_matrizea)
d <- as.dist(dist_matrizea)
#mds.coor <- cmdscale(d)
fanny_emaitza <- fanny(d, 10,  diss = TRUE, memb.exp = 1.5)
fanny_emaitza
#write.csv(fanny_emaitza$membership, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_membership_3.csv", row.names = FALSE)

# # Exportar los clusters asignados
# write.csv(fanny_emaitza$clustering, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_clusters_3.csv", row.names = FALSE)

membership <- as.matrix(fanny_emaitza$membership, row.names=1)
clusters <- as.list(fanny_emaitza$clustering)

datubase_matrix <- as.matrix(datubasea)

bilbao_d <- function(items1, items2) {
    bat <- sum(items1 %in% items2) + sum(items2 %in% items1)
    result <- 1 - (bat / (length(items1) + length(items2)))
    return(result)
}

kalkulatu_estabilitatea <- function(datubasea, membership, galdera, cluster) {
    c <- cluster[[1]]
    elementuak <- cluster[[2]]
    n <- length(elementuak)

    batura <- 0
    pisuen_batura <- 0

    for (i in 1:n) {
        for (j in (i+1):n) {
            if (j >n){
                break
            }
            p_i = elementuak[i]
            p_j = elementuak[j]
            if (datubasea[galdera, p_i]=="" || datubasea[galdera, p_j]=="") {
                distantzia <- 1
            } else {
                distantzia <- bilbao_d(strsplit(datubasea[galdera, p_i], ",")[[1]], strsplit(datubasea[galdera, p_j], ",")[[1]])
            }
            pisuak <- membership[p_i, c] * membership[p_j, c]
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
  
    for (i in elementuak1) {
        for (j in elementuak2) {
        if (datubasea[galdera, i]=="" || datubasea[galdera, j]=="") {
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

c1 <- 1
c2 <- 2
indices1 <- which(clusters == c1)
indices2 <- which(clusters == c2)

cluster1 <- list(c1, indices1)
cluster2 <- list(c2, indices2)

kalkulatu_estabilitatea(datubase_matrix, membership, 4, cluster1)

kalkulatu_bariabilitatea(datubase_matrix, membership, 4, cluster1, cluster2)

kalkulatu_diferentziazioa(datubase_matrix, membership, 4, cluster1, cluster2)

get_most_relevant_items <- function(datubasea, clusters, membership, c1, c2){
    indices1 <- which(clusters == c1)
    indices2 <- which(clusters == c2)

    cluster1 <- list(c1, indices1)
    cluster2 <- list(c2, indices2)
    
    diferentziazioak <- numeric(length(datubasea))

    for (galdera in 1:dim(datubasea)[1]) {
        diferentziazioak[galdera] <- kalkulatu_diferentziazioa(datubasea, membership, galdera, cluster1, cluster2)
    }
    sorted_indices <- order(diferentziazioak, decreasing = TRUE)

    top3_items <- sorted_indices[1:3]

    cluster1_items <- list()
    for (i in 1:3){
        cluster1_items[[i]] <- datubase_matrix[top3_items[i], indices1]
    }
    cluster2_items <- list()
    for (i in 1:3){
        cluster2_items[[i]] <- datubase_matrix[top3_items[i], indices2]
    }

    return(c(cluster1_items, cluster2_items))
}

top_items <- get_most_relevant_items(datubase_matrix, membership, 4, 5)

top_items[[1]]

cluster1_items
cluster2_items[[1]][2]

names(which(clusters == c1))

cbind(cluster1_items, cluster2_items)[1]
