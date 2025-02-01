library(shiny)
library(shinyWidgets)
library(shinyjs)
library(cluster)
library(shinycssloaders)
library(stringdist)
library(ggplot2)
library(parallel)

bilbao_d <- function(items1, items2) {
  bat <- sum(items1 %in% items2) + sum(items2 %in% items1)
  result <- 1 - (bat / (length(items1) + length(items2)))
  return(result)
}

# levenshtein_d <- function(items1, items2) {
#   distances <- outer(items1, items2, Vectorize(function(a, b) stringdist(a, b, method = "lv")))
#   
#   max_length <- max(c(nchar(unlist(items1)), nchar(unlist(items2))), na.rm = TRUE)
#   if (max_length == 0) return(1)  
#   
#   total_distance <- sum(distances, na.rm = TRUE)
#   normalized_distance <- total_distance / (length(items1) * length(items2) * max_length)
#   
#   return(normalized_distance)
# }

levenshtein_d <- function(items1, items2) {
  # Calculate Levenshtein distances using stringdistmatrix 
  distances <- stringdistmatrix(items1, items2, method = "lv") 
  
  # Calculate maximum string length
  max_length <- max(nchar(c(items1, items2)), na.rm = TRUE)
  if (max_length == 0) return(1)
  
  # Calculate normalized distance
  normalized_distance <- sum(distances, na.rm = TRUE) / (length(items1) * length(items2) * max_length)
  
  return(normalized_distance)
}



# kalkulatu_estabilitatea <- function(answers, membership, galdera, cluster, distantzia_funtzioa) {
#   c <- cluster[[1]]
#   elementuak <- cluster[[2]]
#   n <- length(elementuak)
#   
#   batura <- 0
#   pisuen_batura <- 0
#   
#   for (i in 1:n) {
#     for (j in (i + 1):n) {
#       if (j > n) {
#         break
#       }
#       p_i <- elementuak[i]
#       p_j <- elementuak[j]
#       
#       if (is.na(answers[galdera, p_i]) || is.na(answers[galdera, p_j]) || 
#           answers[galdera, p_i] == "" || answers[galdera, p_j] == "") {
#         distantzia <- 1
#       } else {
#         distantzia <- distantzia_funtzioa(strsplit(answers[galdera, p_i], ",")[[1]], strsplit(answers[galdera, p_j], ",")[[1]])
#       }
#       
#       pisuak <- membership[p_i, c] * membership[p_j, c]
#       pisuen_batura <- pisuen_batura + pisuak
#       batura <- batura + distantzia * pisuak
#     }
#   }
#   
#   if (pisuen_batura == 0) return(NA)
#   return(batura * 2 / pisuen_batura)
# }

kalkulatu_estabilitatea <- function(answers, membership, galdera, cluster, distantzia_funtzioa) {
  c <- cluster[[1]]
  elementuak <- cluster[[2]]
  n <- length(elementuak)
  
  # Extract relevant answers and membership values
  answers_subset <- answers[galdera, elementuak, drop = FALSE]
  membership_subset <- membership[elementuak, c]
  
  # Create a distance matrix using outer() and vectorized distance function
  distance_matrix <- outer(answers_subset, answers_subset, Vectorize(function(x, y) {
    if (is.na(x) || is.na(y) || x == "" || y == "") {
      return(1)
    } else {
      return(distantzia_funtzioa(strsplit(x, ",")[[1]], strsplit(y, ",")[[1]]))
    }
  }))
  
  # Create a matrix of weights
  weights_matrix <- outer(membership_subset, membership_subset, "*")
  
  # Calculate the weighted sum of distances
  weighted_sum <- sum(distance_matrix * weights_matrix, na.rm = TRUE)
  
  # Calculate the sum of weights
  sum_weights <- sum(weights_matrix, na.rm = TRUE)
  
  # Calculate the stability
  if (sum_weights == 0) {
    return(NA)
  } else {
    return(2 * weighted_sum / sum_weights)
  }
}


# kalkulatu_bariabilitatea <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
#   c1 <- cluster1[[1]]
#   elementuak1 <- cluster1[[2]]
#   
#   c2 <- cluster2[[1]]
#   elementuak2 <- cluster2[[2]]
#   
#   batura <- 0
#   pisuen_batura <- 0
#   
#   for (i in elementuak1) {
#     for (j in elementuak2) {
#       if (is.na(answers[galdera, i]) || is.na(answers[galdera, j]) || 
#           answers[galdera, i] == "" || answers[galdera, j] == "") {
#         distantzia <- 1
#       } else {
#         distantzia <- distantzia_funtzioa(strsplit(answers[galdera, i], ",")[[1]], 
#                                strsplit(answers[galdera, j], ",")[[1]])
#       }
#       
#       pisuak <- membership[i, c1] * membership[j, c2]
#       pisuen_batura <- pisuen_batura + pisuak
#       batura <- batura + distantzia * pisuak
#     }
#   }
#   
#   return(ifelse(pisuen_batura > 0, batura / pisuen_batura, NA))
# }

kalkulatu_bariabilitatea <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  c1 <- cluster1[[1]]
  elementuak1 <- cluster1[[2]]
  c2 <- cluster2[[1]]
  elementuak2 <- cluster2[[2]]
  
  # Extract relevant answers and membership values
  answers_subset1 <- answers[galdera, elementuak1, drop = FALSE]
  answers_subset2 <- answers[galdera, elementuak2, drop = FALSE]
  membership_subset1 <- membership[elementuak1, c1]
  membership_subset2 <- membership[elementuak2, c2]
  
  # Create a distance matrix using outer() and vectorized distance function
  distance_matrix <- outer(answers_subset1, answers_subset2, Vectorize(function(x, y) {
    if (is.na(x) || is.na(y) || x == "" || y == "") {
      return(1)
    } else {
      return(distantzia_funtzioa(strsplit(x, ",")[[1]], strsplit(y, ",")[[1]]))
    }
  }))
  
  # Create a matrix of weights
  weights_matrix <- outer(membership_subset1, membership_subset2, "*")
  
  # Calculate the weighted sum of distances
  weighted_sum <- sum(distance_matrix * weights_matrix, na.rm = TRUE)
  
  # Calculate the sum of weights
  sum_weights <- sum(weights_matrix, na.rm = TRUE)
  
  # Calculate the variability
  if (sum_weights == 0) {
    return(NA)
  } else {
    return(weighted_sum / sum_weights)
  }
}

kalkulatu_diferentziazioa <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  estabilitatea1 <- kalkulatu_estabilitatea(answers, membership, galdera, cluster1, distantzia_funtzioa)
  estabilitatea2 <- kalkulatu_estabilitatea(answers, membership, galdera, cluster2, distantzia_funtzioa)
  
  if (is.na(estabilitatea1) || is.na(estabilitatea2)) return(NA)
  

  bariabilitatea1_2 <- kalkulatu_bariabilitatea(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa)

  diferentziazioa = bariabilitatea1_2 / max(estabilitatea1, estabilitatea2, 0.01)
  return (data.frame(galdera = galdera, diferentziazioa = diferentziazioa, estabilitatea_1 = estabilitatea1, estabilitatea_2= estabilitatea2, bariabilitatea1_2= bariabilitatea1_2))
}

get_most_relevant_items <- function(answers, questions, clusteringResult, c1, c2, n, distantzia_funtzioa, set_progress){
  clusters <- clusteringResult$clustering
  membership <- clusteringResult$membership
  names <- rownames(clusteringResult$membership)

  indices1 <- which(clusters == c1)
  indices2 <- which(clusters == c2)

  cluster1 <- list(c1, indices1)
  cluster2 <- list(c2, indices2)

  diferentziazioak <- data.frame()

  k <- dim(answers)[1]

  for (galdera in 1:k) {

    uneko_dif <- kalkulatu_diferentziazioa(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa)
    diferentziazioak <- rbind(diferentziazioak, uneko_dif)
    set_progress(galdera/k)
  }

  diferentziazioak <- diferentziazioak[order(-diferentziazioak$diferentziazioa), ]

  topn_items <- head(diferentziazioak, n)

  cluster1_items <- answers[topn_items$galdera, indices1]
  cluster2_items <- answers[topn_items$galdera, indices2]

  top_questions <- questions[topn_items$galdera]
  topn_items$galdera <- top_questions

  colnames(cluster1_items) <- names(indices1)
  colnames(cluster2_items) <- names(indices2)

  output <- list("top_questions" = topn_items, "cluster1_items" = cluster1_items, "cluster2_items" = cluster2_items)

  return(output)
}

get_most_relevant_items_optimized <- function(answers, questions, clusteringResult, c1, c2, n, distantzia_funtzioa, set_progress) {
  clusters <- clusteringResult$clustering
  membership <- clusteringResult$membership
  names <- rownames(clusteringResult$membership)
  
  indices1 <- which(clusters == c1)
  indices2 <- which(clusters == c2)
  
  cluster1 <- list(c1, indices1)
  cluster2 <- list(c2, indices2)
  
  # Function to calculate differentiation for a single question
  calculate_differentiation_for_question <- function(galdera) {
    kalkulatu_diferentziazioa(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa)
  }
  
  # Create cluster for parallel processing
  no_cores <- detectCores() - 1  # Use all but one core
  cl <- makeCluster(no_cores) 
  
  # Export necessary objects to each cluster
  clusterExport(cl, 
                varlist = c("answers", "membership", "cluster1", "cluster2", "distantzia_funtzioa", 
                            "kalkulatu_diferentziazioa"), 
                envir = environment()) 
  
  # Calculate differentiations in parallel
  diferentziazioak_list <- parLapply(cl, 1:dim(answers)[1], calculate_differentiation_for_question) 
  
  # Stop the cluster
  stopCluster(cl)
  
  # Combine results
  diferentziazioak <- do.call(rbind, diferentziazioak_list) 
  
  # Remaining steps as in the original function
  diferentziazioak <- diferentziazioak[order(-diferentziazioak$diferentziazioa), ]
  topn_items <- head(diferentziazioak, n)
  
  cluster1_items <- answers[topn_items$galdera, indices1]
  cluster2_items <- answers[topn_items$galdera, indices2]
  
  top_questions <- questions[topn_items$galdera]
  topn_items$galdera <- top_questions
  
  colnames(cluster1_items) <- names(indices1)
  colnames(cluster2_items) <- names(indices2)
  
  output <- list("top_questions" = topn_items, "cluster1_items" = cluster1_items, "cluster2_items" = cluster2_items)
  
  return(output)
}


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Diatech Fuzzy Clustering Demo"),
  div(
    id = "mainPanel",
    sidebarLayout(
      sidebarPanel(
        fileInput("fileInput", "Import Distance Matrix", accept = ".csv"),
        textOutput("fileInputError"),  # Error message placeholder
        fileInput("questionsFile", "Import Questions", accept = ".csv"),
        textOutput("questionsFileError"),  # Error message placeholder
        fileInput("answersFile", "Import Answers", accept = ".csv"),
        textOutput("answersFileError"),  # Error message placeholder
        fileInput("locationFile", "Import Location", accept = ".csv"),
        textOutput("locationFileError"),  # Error message placeholder
        numericInput("number_of_clusters", "Number of Clusters:", value = 20, min = 2, step = 1),
        numericInput("exponential", "Exponential Parameter:", value = 1.2, min = 1, step = 0.1),
        actionButton("performClustering", "Perform Clustering")
      ),
      mainPanel(
        tableOutput("table"),
        verbatimTextOutput("result")
      )
    )
  ),
  hidden(
    div(
      id = "clusteringPanel",
      # selectInput("cluster1", "Select Cluster 1:", choices = 1, label = NULL),
      selectInput("cluster1", "Select Cluster 1:", 
                  choices = c("All Clusters" = 0, 1), 
                  label = NULL),
      textOutput("cluster1Size"), 
      # selectInput("cluster2", "Select Cluster 2:", choices = 1, label = NULL),
      selectInput("cluster2", "Select Cluster 2:", 
                  choices = c("All Clusters" = 0, 1), 
                  label = NULL),
      textOutput("cluster2Size"), 
      selectInput("distanceFunction", "Select Distance:", choices = c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d")),
      numericInput("n_items", "Number of Relevant items:", value = 5, min = 1, step = 1), 
      actionButton("compareClusters", "Compare Clusters"),
      actionButton("backToMainPanel", "Back")
    )
  ),
  hidden(
    div(
      id = "comparisonPanel",
      h3("Relevant Questions", style = "padding: 10px;"),
      DT::dataTableOutput("topQuestionsTable"),
      h3("Cluster Items for Selected Question", style = "padding: 10px;"),
      div(
        style = "padding: 10px;",
        h4(textOutput("cluster1Label")), 
        tableOutput("cluster1Table"),
        h4(textOutput("cluster2Label")), 
        tableOutput("cluster2Table")
        
      ),
      div(
        style = "flex: 1; padding: 10px;",
        plotOutput("itemFrequenciesPlot"),
        downloadButton("downloadPlot", "Download Plot") # Add download button
        
      ),
      actionButton("backToClusteringPanel", "Back"),
      verbatimTextOutput("progress_output")
      
    )
  )
)

server <- function(input, output, session) {
  
  dist_matrix <- reactiveVal()
  questions <- reactiveVal()
  answers <- reactiveVal()
  location <- reactiveVal()
  clusters <- reactiveVal()
  relevantItems <- reactiveVal()
  
  observeEvent(input$fileInput, {
    tryCatch({
      req(input$fileInput)
      file <- input$fileInput$datapath
      dist_matrix(read.csv(file, row.names = 1))
      output$fileInputError <- renderText({ "" })  # Clear error message
      cleaned <<- FALSE
      showNotification("Distance matrix imported successfully!", type = "message")
    }, error = function(e) {
      output$fileInputError <- renderText({ paste("Error importing Distance Matrix:", e$message) })
      showNotification("Failed to import Distance Matrix!", type = "error")
    })
  })
  
  observeEvent(input$questionsFile, {
    tryCatch({
      req(input$questionsFile)
      file <- input$questionsFile$datapath
      questions(read.csv(file, stringsAsFactors = FALSE, sep = ";")[4][[1]])
      output$questionsFileError <- renderText({ "" })  # Clear error message
      showNotification("Questions imported successfully!", type = "message")
    }, error = function(e) {
      output$questionsFileError <- renderText({ paste("Error importing Questions:", e$message) })
      showNotification("Failed to import Questions!", type = "error")
    })
  })
  
  observeEvent(input$answersFile, {
    tryCatch({
      req(input$answersFile)
      file <- input$answersFile$datapath
      answers(read.csv(file, stringsAsFactors = FALSE, row.names = 1))
      output$answersFileError <- renderText({ "" })  # Clear error message
      showNotification("Answers imported successfully!", type = "message")
    }, error = function(e) {
      output$answersFileError <- renderText({ paste("Error importing Answers:", e$message) })
      showNotification("Failed to import Answers!", type = "error")
    })
  })
  
  observeEvent(input$locationFile, {
    tryCatch({
      req(input$locationFile)
      file <- input$locationFile$datapath
      location(read.csv(file, sep= ';', header = FALSE, col.names = c("ID", "Town", 'X1', 'X2')))
      
      output$locationFileError <- renderText({ "" })  # Clear error message
      showNotification("Locations imported successfully!", type = "message")
    }, error = function(e) {
      output$locationFileError <- renderText({ paste("Error importing Locations:", e$message) })
      showNotification("Failed to import Locations!", type = "error")
    })
  })
  
  # Perform clustering using FANNY
  observeEvent(input$performClustering, {
    tryCatch({
      req(dist_matrix(), location())
      distmatrix <- dist_matrix()
      locations <- location()
      
      if (!cleaned){
        
        clean_names <- function(name) {
          sub("-(\\d+)$", "", name)
        }
        
        rownames(distmatrix) <- sapply(rownames(distmatrix), clean_names)
        colnames(distmatrix) <- sapply(rownames(distmatrix), clean_names)
        
        correct_order <- as.data.frame(locations)$Town 
       
        # Reorder the rows and columns of the distance matrix
        distmatrix <- distmatrix[correct_order, correct_order]
        
        dist_matrix(distmatrix)
        cleaned <<- TRUE
      }
      
      clusteringResult <- fanny(distmatrix, k = input$number_of_clusters, memb.exp = input$exponential)
      
      clusters(clusteringResult)
      
      distances <- c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d")
      clusterLabels <- unique(clusteringResult$clustering)
      updateSelectInput(session, "cluster1", choices = clusterLabels)
      updateSelectInput(session, "cluster2", choices = clusterLabels)
      
      clusterSizes <- table(clusteringResult$clustering) 
      
      clusterLabels <- c("All Clusters" = 0, paste0(names(clusterSizes), " (Number of towns: ", clusterSizes, ")"))
      
      # Update selectInput with cluster labels and sizes
      updateSelectInput(session, "cluster1", 
                        choices = paste0(names(clusterSizes), " (Number of towns: ", clusterSizes, ")"),
                        selected = NULL)
      updateSelectInput(session, "cluster2", 
                        choices = paste0(names(clusterSizes), " (Number of towns: ", clusterSizes, ")"),
                        selected = NULL)
      
      shinyjs::hide("mainPanel")
      shinyjs::show("clusteringPanel")
    }, error = function(e) {
    showNotification(paste("Error performing clustering: Please check your input data", e$message), type = "error")
    })
  })

  
  get_cluster_number <- function(selected_option) {
    if (!is.null(selected_option) && selected_option != "All Clusters") {
      return(as.numeric(strsplit(selected_option, " ")[[1]][1]))
    } else {
      return(NULL) 
    }
  }
  
  # Compare clusters
  observeEvent(input$compareClusters, {
    
    tryCatch({
      req(input$cluster1, input$cluster2, input$distanceFunction)
      
      shinyjs::hide("clusteringPanel")
      shinyjs::show("comparisonPanel")
      
      selected_distance_function <- reactive({
        switch(input$distanceFunction,
               bilbao_d = bilbao_d,
               levenshtein_d = levenshtein_d)
      })
      
      
      if (input$cluster1 == "All Clusters") {
        indices1 <- setdiff(1:nrow(clusteringResult$membership), 
                            which(clusteringResult$clustering == get_cluster_number(input$cluster2)))
      } else if (input$cluster2 == "All Clusters") {
        indices1 <- which(clusteringResult$clustering == get_cluster_number(input$cluster1))
        indices2 <- setdiff(1:nrow(clusteringResult$membership), indices1)
      } else {
        indices1 <- which(clusteringResult$clustering == get_cluster_number(input$cluster1))
        indices2 <- which(clusteringResult$clustering == get_cluster_number(input$cluster2))
      }
      
      cluster1 <- list(get_cluster_number(input$cluster1), indices1)
      cluster2 <- list(get_cluster_number(input$cluster2), indices2)
      
      start.time <- Sys.time()
      
      
      
      withProgress(message = 'Processing data...', value = 0, {

          relevantItems(get_most_relevant_items_optimized(
            answers = answers(),
            questions = questions(),
            clusteringResult = clusters(),
            c1 = cluster1[[1]], 
            c2 = cluster2[[1]], 
            n = input$n_items,
            distantzia_funtzioa = selected_distance_function(),
            set_progress = function(value) { 
              
            }
          ))
      })
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      # print(time.taken)
      
      output$topQuestionsTable <- DT::renderDataTable({
        req(relevantItems())
        
        data.frame(Questions = relevantItems()$top_questions$galdera,
                   Diferentiation = relevantItems()$top_questions$diferentziazioa,
                   Stability1 = relevantItems()$top_questions$estabilitatea_1,
                   Stability2 = relevantItems()$top_questions$estabilitatea_2,
                   Variability = relevantItems()$top_questions$bariabilitatea1_2
        )
      }, selection = "single", options = list(pageLength = input$n_items, dom = 't'))
    
    }, error = function(e) {
      showNotification(paste("Error comparing clusters: Check data or clustering result.", e$message), type = "error")
    })
  })
  
  plot_obj <- reactiveVal(NULL)
  
  observeEvent(input$topQuestionsTable_rows_selected, {
    tryCatch({
      req(input$topQuestionsTable_rows_selected, relevantItems())
      selectedIndex <- input$topQuestionsTable_rows_selected

      output$cluster1Table <- renderTable({
        relevantItems()$cluster1_items[selectedIndex, , drop = FALSE]
      })

      output$cluster2Table <- renderTable({
        relevantItems()$cluster2_items[selectedIndex, , drop = FALSE]
      })
      
      # Update cluster labels
      output$cluster1Label <- renderText({ paste0("Cluster ", get_cluster_number(input$cluster1)) })
      output$cluster2Label <- renderText({ paste0("Cluster ", get_cluster_number(input$cluster2)) })
      
      cluster1_items <- unlist(relevantItems()$cluster1_items[selectedIndex, ])
      cluster2_items <- unlist(relevantItems()$cluster2_items[selectedIndex, ])
      
      # Separate items within each town (assuming towns are separated by commas)
      cluster1_items <- unlist(strsplit(cluster1_items, ","))
      cluster2_items <- unlist(strsplit(cluster2_items, ","))
      

      cluster1_data <- as.data.frame(table(cluster1_items))
      cluster2_data <- as.data.frame(table(cluster2_items))
      

      # Add cluster identifier
      cluster1_data$Cluster <- paste0("Cluster ", get_cluster_number(input$cluster1)) 
      cluster2_data$Cluster <- paste0("Cluster ", get_cluster_number(input$cluster2)) 
      
      
      colnames(cluster1_data) <- c("Item", "Frequency", "Cluster")
      colnames(cluster2_data) <- c("Item", "Frequency", "Cluster")
      
      # Combine data
      plot_data <- rbind(cluster1_data, cluster2_data)

      # Create the plot
      plot_obj(ggplot(plot_data, aes(x = Item, y = Frequency, fill = Cluster)) +
                 geom_bar(stat = "identity", position = "dodge") +
                 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                 labs(title = "Item Frequencies by Cluster"))
      
        }, error = function(e) {
    showNotification(e$message, type = "error")
    })
  })
  
  output$itemFrequenciesPlot <- renderPlot({
    req(plot_obj())
    plot_obj() 
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "item_frequencies_plot.png", 
    content = function(file) {
      ggsave(file, plot = plot_obj(), device = "png")
    }
  )
  
  observeEvent(input$backToClusteringPanel, {
    shinyjs::hide("comparisonPanel")
    shinyjs::show("clusteringPanel")
    
    output$topQuestionsTable <- DT::renderDataTable({ NULL })
    output$cluster1Table <- renderTable({ NULL })
    output$cluster2Table <- renderTable({ NULL })
  })
  
  observeEvent(input$backToMainPanel, {
    shinyjs::hide("clusteringPanel")
    shinyjs::show("mainPanel")
  })
}

shinyApp(ui = ui, server = server)
