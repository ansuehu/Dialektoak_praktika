library(cluster)

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
    for (j in (i + 1):n) {
      if (j > n) {
        break
      }
      p_i <- elementuak[i]
      p_j <- elementuak[j]
      
      if (is.na(datubasea[galdera, p_i]) || is.na(datubasea[galdera, p_j]) || 
          datubasea[galdera, p_i] == "" || datubasea[galdera, p_j] == "") {
        distantzia <- 1
      } else {
        distantzia <- bilbao_d(strsplit(datubasea[galdera, p_i], ",")[[1]], strsplit(datubasea[galdera, p_j], ",")[[1]])
      }
      
      pisuak <- membership[p_i, c] * membership[p_j, c]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  if (pisuen_batura == 0) return(NA)
  return(batura * 2 / pisuen_batura)
}


kalkulatu_bariabilitatea <- function(datubasea, membership, galdera, cluster1, cluster2) {
  c1 <- cluster1[[1]]
  elementuak1 <- cluster1[[2]]
  
  c2 <- cluster2[[1]]
  elementuak2 <- cluster2[[2]]
  
  batura <- 0
  pisuen_batura <- 0
  
  for (i in elementuak1) {
    for (j in elementuak2) {
      if (is.na(datubasea[galdera, i]) || is.na(datubasea[galdera, j]) || 
          datubasea[galdera, i] == "" || datubasea[galdera, j] == "") {
        distantzia <- 1
      } else {
        distantzia <- bilbao_d(strsplit(datubasea[galdera, i], ",")[[1]], 
                               strsplit(datubasea[galdera, j], ",")[[1]])
      }
      
      pisuak <- membership[i, c1] * membership[j, c2]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  return(ifelse(pisuen_batura > 0, batura / pisuen_batura, NA))
}

kalkulatu_diferentziazioa <- function(datubasea, membership, galdera, cluster1, cluster2) {
  estabilitatea1 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster1)
  estabilitatea2 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster2)
  
  if (is.na(estabilitatea1) || is.na(estabilitatea2)) return(NA)
  
  bariabilitatea1_2 <- kalkulatu_bariabilitatea(datubasea, membership, galdera, cluster1, cluster2)
  return(bariabilitatea1_2 / max(estabilitatea1, estabilitatea2, 1e-6))
}

get_most_relevant_items <- function(datubasea, questions, clusteringResult, c1, c2, n){
  clusters <- clusteringResult$clustering
  membership <- clusteringResult$membership
  indices1 <- which(clusters == c1)
  indices2 <- which(clusters == c2)
  
  cluster1 <- list(c1, indices1)
  cluster2 <- list(c2, indices2)
  
  diferentziazioak <- numeric(length(datubasea))
  
  for (galdera in 1:dim(datubasea)[1]) {
    diferentziazioak[galdera] <- kalkulatu_diferentziazioa(datubasea, membership, galdera, cluster1, cluster2)
  }
  sorted_indices <- order(diferentziazioak, decreasing = TRUE)
  
  top3_items <- sorted_indices[1:n]
  
  top_questions <- questions[top3_items]
  
  cluster1_items <- datubasea[top3_items, indices1]
  cluster2_items <- datubasea[top3_items, indices2]
  output <- list("top_questions" = top_questions, "cluster1_items" = cluster1_items, "cluster2_items" = cluster2_items)
  
  return(output)
}

data <- read.csv('/Volumes/MacOS 1TB/Repos/Dialektoak_praktika-1/Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/Matriz-distancias del ESB-Diatech z-24-04-02.csv', stringsAsFactors = FALSE)
questions <- read.csv('/Volumes/MacOS 1TB/Repos/Dialektoak_praktika-1/Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/question.csv', stringsAsFactors = FALSE, sep = ";")
answers <- as.matrix(read.csv('/Volumes/MacOS 1TB/Repos/Dialektoak_praktika-1/Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/ESB-datu-basea-taula.csv', stringsAsFactors = FALSE, row.names=1))
datubasea <- answers

questions <- questions[4][[1]]
numericData <- data
numericData <- numericData[sapply(numericData, is.numeric)] # Filter numeric columns
numericData[is.na(numericData)] <- 0 # Replace NA with 0


distMatrix <- dist(numericData) # Compute distance matrix
clusteringResult <- fanny(distMatrix, k = 20, memb.exp = 1.2)
clusterLabels <- unique(clusteringResult$clustering)

c1 = 2
c2 = 4

datubasea
names <- c(names(data))[2:length(names)]


a <- get_most_relevant_items(datubasea, questions, clusteringResult, c1, c2, 10)
xnames <- c(colnames(a$cluster1_items))
xnames

ind <- as.integer(sub('.', '', xnames))
colnames(a$cluster1_items) <- names[ind]
a$cluster1_items
ind+1
