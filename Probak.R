setwd("/home/andoni/Downloads/Praktika Euskara Dialektoak-20241128T082756Z-001/Praktika Euskara Dialektoak")
library(cluster)

datuak <- read.csv("./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/Matriz-distancias del ESB-Diatech z-24-04-02.csv", row.names=1)

datuak

datuak <- data.matrix(datuak)
d <- as.dist(datuak)
#mds.coor <- cmdscale(d)
fanny_emaitza <- fanny(d, 3)
?fanny
summary(fanny_emaitza)
#plot(fanny_emaitza)

write.csv(fanny_emaitza$membership, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_membership_3.csv", row.names = FALSE)

# Exportar los clusters asignados
write.csv(fanny_emaitza$clustering, "./Datu-basea_ euskalkien ezaugarri adierazgarriak eta bereizgarriak/fanny_clusters_3.csv", row.names = FALSE)
