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

levenshtein_distance <- function(str1, str2) {
  len1 <- nchar(str1)
  len2 <- nchar(str2)
  
  # Create the distance matrix
  dist_matrix <- matrix(0, nrow = len1 + 1, ncol = len2 + 1)
  
  # Initialize the first row and column
  for (i in 1:(len1 + 1)) dist_matrix[i, 1] <- i - 1
  for (j in 1:(len2 + 1)) dist_matrix[1, j] <- j - 1
  
  # Fill the matrix
  for (i in 2:(len1 + 1)) {
    for (j in 2:(len2 + 1)) {
      cost <- ifelse(substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1), 0, 1)
      dist_matrix[i, j] <- min(
        dist_matrix[i - 1, j] + 1,  # Deletion
        dist_matrix[i, j - 1] + 1,  # Insertion
        dist_matrix[i - 1, j - 1] + cost  # Substitution
      )
    }
  }
  
  return(dist_matrix[len1 + 1, len2 + 1])
}

levenshtein_d <- function(items1, items2) {
  distances <- matrix(0, nrow = length(items1), ncol = length(items2))
  
  # Calculate the pairwise Levenshtein distances
  for (i in 1:length(items1)) {
    for (j in 1:length(items2)) {
      distances[i, j] <- levenshtein_distance(items1[i], items2[j])
    }
  }
  
  # Normalize the distance as in the original function
  max_length <- max(nchar(c(items1, items2)), na.rm = TRUE)
  if (max_length == 0) return(1)
  
  normalized_distance <- sum(distances, na.rm = TRUE) / (length(items1) * length(items2) * max_length)
  
  return(normalized_distance)
}

kalkulatu_estabilitatea <- function(answers, membership, galdera, cluster, distantzia_funtzioa) {
  c <- cluster$cluster
  elementuak <- cluster$indices
  n <- length(elementuak)
  
  answers_subset <- answers[galdera, elementuak, drop = FALSE]
  membership_subset <- membership[elementuak, c]
  
  distance_matrix <- outer(answers_subset, answers_subset, Vectorize(function(x, y) {
    if (is.na(x) || is.na(y) || x == "" || y == "") {
      return(1)
    } else {
      return(distantzia_funtzioa(strsplit(x, ",")[[1]], strsplit(y, ",")[[1]]))
    }
  }))
  
  weights_matrix <- outer(membership_subset, membership_subset, "*")
  
  weighted_sum <- sum(distance_matrix * weights_matrix, na.rm = TRUE)
  
  sum_weights <- sum(weights_matrix, na.rm = TRUE)
  
  if (sum_weights == 0) {
    return(NA)
  } else {
    return(2 * weighted_sum / sum_weights)
  }
}

kalkulatu_bariabilitatea <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  c1 <- cluster1$cluster
  elementuak1 <- cluster1$indices
  c2 <- cluster2$cluster
  elementuak2 <- cluster2$indices

  answers_subset1 <- answers[galdera, elementuak1, drop = FALSE]
  answers_subset2 <- answers[galdera, elementuak2, drop = FALSE]
  membership_subset1 <- membership[elementuak1, c1]
  membership_subset2 <- membership[elementuak2, c2]
  
  distance_matrix <- outer(answers_subset1, answers_subset2, Vectorize(function(x, y) {
    if (is.na(x) || is.na(y) || x == "" || y == "") {
      return(1)
    } else {
      return(distantzia_funtzioa(strsplit(x, ",")[[1]], strsplit(y, ",")[[1]]))
    }
  }))
  
  weights_matrix <- outer(membership_subset1, membership_subset2, "*")
  
  weighted_sum <- sum(distance_matrix * weights_matrix, na.rm = TRUE)
  
  sum_weights <- sum(weights_matrix, na.rm = TRUE)
  
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
  return (data.frame(galdera = galdera, diferentziazioa = round(diferentziazioa, 2), estabilitatea_1 = round(estabilitatea1, 2), estabilitatea_2= round(estabilitatea2, 2), bariabilitatea1_2= round(bariabilitatea1_2, 2)))
}

get_most_relevant_items_optimized <- function(answers, questions, clusteringResult, cluster1, cluster2, distantzia_funtzioa, set_progress) {
  clusters <- clusteringResult$clustering
  membership <- clusteringResult$membership
  names <- rownames(clusteringResult$membership)
  
  indices1 <- cluster1$indices
  indices2 <- cluster2$indices

  calculate_differentiation_for_question <- function(galdera) {
    kalkulatu_diferentziazioa(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa)
  }
  
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores) 
  
  clusterExport(cl, 
                varlist = c("answers", "membership", "cluster1", "cluster2", "distantzia_funtzioa", 
                            "kalkulatu_diferentziazioa"), 
                envir = environment()) 
  
  diferentziazioak_list <- parLapply(cl, 1:dim(answers)[1], calculate_differentiation_for_question) 
  
  stopCluster(cl)
  
  diferentziazioak <- do.call(rbind, diferentziazioak_list) 
  
  topn_items <- diferentziazioak[order(-diferentziazioak$diferentziazioa), ]
  
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
    style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px;",
    fluidRow(
      column(
        width = 5,
        div(
          style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
          h3("Input Data", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px; margin-top: 0;"),
          
          div(
            style = "margin-top: 15px;",
            fileInput("fileInput", "Import Distance Matrix", accept = ".csv"),
            textOutput("fileInputError")
          ),
          
          div(
            style = "margin-top: 15px;",
            fileInput("questionsFile", "Import Questions", accept = ".csv"),
            textOutput("questionsFileError")
          ),
          
          div(
            style = "margin-top: 15px;",
            fileInput("answersFile", "Import Answers", accept = ".csv"),
            textOutput("answersFileError")
          ),
          
          div(
            style = "margin-top: 15px;",
            fileInput("locationFile", "Import Location", accept = ".csv"),
            textOutput("locationFileError")
          )
        )
      ),
    
      column(
        width = 3,
        div(
          style = "background-color: #ffffff; padding: 15px; margin-left: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
          h3("Clustering Parameters", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 8px;"),
          
          div(
            style = "margin-top: 15px;",
            numericInput("number_of_clusters", "Number of Clusters:", value = 20, min = 2, step = 1)
          ),
          
          div(
            style = "margin-top: 15px;",
            numericInput("exponential", "Exponential Parameter:", value = 1.2, min = 1, step = 0.1)
          ),
          
          div(
            style = "margin-top: 25px;",
            actionButton("performClustering", "Perform Clustering", 
                         class = "btn-primary", style = "width: 100%;")
          )
        )
      ),
      # Add new column for app overview and stats visualization
      column(
        width = 4,
        div(
          style = "background-color: #ffffff; padding: 15px; margin-left: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
          h3("Dashboard", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 8px;"),
          div(
            style = "margin-top: 25px;",
            h4("Data Summary", style = "color: #6c757d;"),
            uiOutput("dataSummary")
          )
        )
      )
    )
  ),

  hidden(
    div(
      id = "clusteringPanel",
      style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px;",
      fluidRow(
        column(
          width = 5,
          div(
            style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
            h3("Cluster Selection", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;"),
            div(
              style = "padding: 10px 0;",
              selectInput("cluster1", "Select Cluster 1:", 
                          choices = c("All Clusters" = 0, 1), 
                          label = NULL),
              textOutput("cluster1Size"), 
              
              selectInput("cluster2", "Select Cluster 2:", 
                          choices = c("All Clusters" = 0, 1), 
                          label = NULL),
              textOutput("cluster2Size"),
              
              selectInput("distanceFunction", "Select Distance:", 
                          choices = c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d"),
                          width = "100%"),
              
              div(
                style = "margin-top: 20px;",
                actionButton("compareClusters", "Compare Clusters", 
                             class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
                downloadButton("downloadClustering", "Download Clustering Results", 
                               style = "width: 100%; margin-bottom: 20px;"),
                actionButton("backToMainPanel", "Back to Main Panel", 
                             class = "btn-secondary", style = "width: 100%;")
              )
            )
          )
        ),
        column(
          width = 7,
          div(
            style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
            h3("Cluster Towns", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;"),
            fluidRow(
              column(
                width = 6,
                div(
                  style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-right: 10px; height: 100%;",
                  h4(textOutput("townsTableLabel1"), style = "color: #007bff; margin-bottom: 15px;"),
                  div(
                    style = "max-height: 500px; overflow-y: auto; overflow-x: auto;",
                    tableOutput("townsTable1")
                  )
                )
              ),
              column(
                width = 6,
                div(
                  style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-left: 10px; height: 100%;",
                  h4(textOutput("townsTableLabel2"), style = "color: #007bff; margin-bottom: 15px;"),
                  div(
                    style = "max-height: 500px; overflow-y: auto; overflow-x: auto;",
                    tableOutput("townsTable2")
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  hidden(
    div(
      id = "comparisonPanel",
      fluidRow(
        column(
          width = 12,
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
            fluidRow(
              column(
                width = 6,
                sliderInput("n_items", "Number of Relevant Items:",
                            min = 0, max = 100, # Set appropriate min/max values
                            value = 10, step = 1, # Define step size
                            width = "100%")
              ),
              column(
                width = 6,
                div(
                  style = "text-align: right; padding-top: 25px;",
                  actionButton("backToClusteringPanel", "Back", 
                               class = "btn-secondary",
                               style = "margin-right: 10px;"),
                  downloadButton("downloadPlot", "Download Plot", 
                                 class = "btn-primary"),
                  downloadButton("downloadTable", "Download Table",
                                 class = "btn-primary", style = "margin-left: 10px;")
                )
              )
            )
          )
        )
      ),
      
      div(
        style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 20px;",
        h3("Relevant Questions", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;"),
        div(
          style = "padding: 10px 0;",
          DT::dataTableOutput("topQuestionsTable")
        )
      ),
      
      div(
        style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); margin-bottom: 20px;",
        h3("Cluster Items for Selected Question", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;"),
        fluidRow(
          column(
            width = 6,
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-right: 10px; height: 100%; overflow-x: auto;",
              h4(textOutput("cluster1Label"), style = "color: #007bff; margin-bottom: 15px;"), 
              div(style = "max-height: 300px; overflow-y: auto;",
                  tableOutput("cluster1Table")
              )
            )
          ),
          column(
            width = 6,
            div(
              style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-left: 10px; height: 100%; overflow-x: auto;",
              h4(textOutput("cluster2Label"), style = "color: #007bff; margin-bottom: 15px;"), 
              div(style = "max-height: 300px; overflow-y: auto;",
                  tableOutput("cluster2Table")
              )
            )
          )
        )
      ),
      
      div(
        style = "background-color: #ffffff; padding: 15px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);",
        h3("Item Frequency Visualization", style = "color: #495057; border-bottom: 1px solid #dee2e6; padding-bottom: 10px;"),
        div(
          style = "padding: 10px 0;",
          plotOutput("itemFrequenciesPlot", height = "400px")
        )
      ),
      
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
      output$fileInputError <- renderText({ "" })
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
      output$questionsFileError <- renderText({ "" })
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
      output$answersFileError <- renderText({ "" })
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
      
      output$locationFileError <- renderText({ "" })
      showNotification("Locations imported successfully!", type = "message")
    }, error = function(e) {
      output$locationFileError <- renderText({ paste("Error importing Locations:", e$message) })
      showNotification("Failed to import Locations!", type = "error")
    })
  })
  
  output$locationMapPreview <- renderPlot({
    req(location())
    locations_data <- location()
    
    # Extract valid location points and handle potential issues
    valid_locations <- locations_data[!is.na(locations_data$X1) & !is.na(locations_data$X2) & 
                                        is.numeric(locations_data$X1) & is.numeric(locations_data$X2),]
    
    # Convert character coordinates to numeric if needed
    if(is.character(locations_data$X1)) {
      cat("Converting X1 from character to numeric\n")
      locations_data$X1 <- as.numeric(gsub(",", ".", locations_data$X1))
    }
    if(is.character(locations_data$X2)) {
      cat("Converting X2 from character to numeric\n")
      locations_data$X2 <- as.numeric(gsub(",", ".", locations_data$X2))
      valid_locations <- locations_data[!is.na(locations_data$X1) & !is.na(locations_data$X2),]
    }
    
    if(nrow(valid_locations) == 0) {
      # If no valid locations, show empty plot with message
      plot(0, 0, type = "n", axes = TRUE, xlab = "Longitude", ylab = "Latitude", 
           main = "No valid location data found - check your data format")
      return()
    }
    
    # Create a basic scatter plot that should always work
    plot(valid_locations$X2, valid_locations$X1, 
         pch = 19, col = "blue", cex = 1.2,
         xlab = "Longitude", ylab = "Latitude", 
         main = paste("Town Locations (", nrow(valid_locations), " valid points)"))
    
    # Add town names if there aren't too many points
    if(nrow(valid_locations) < 25) {
      text(valid_locations$X2, valid_locations$X1, 
           labels = valid_locations$Town, pos = 3, cex = 0.7)
    }
    
    # Draw a box around the plot
    box()
  })
  
  
  # Add data summary output
  output$dataSummary <- renderUI({
    summary_info <- list()
    
    if (!is.null(dist_matrix())) {
      dist_size <- dim(dist_matrix())
      summary_info <- c(summary_info, paste0("Distance Matrix: ", dist_size[1], "×", dist_size[2]))
    }
    
    if (!is.null(questions())) {
      summary_info <- c(summary_info, paste0("Questions: ", length(questions())))
    }
    
    if (!is.null(answers())) {
      answers_dim <- dim(answers())
      summary_info <- c(summary_info, paste0("Answers: ", answers_dim[1], " questions × ", answers_dim[2], " towns"))
    }
    
    if (!is.null(location())) {
      summary_info <- c(summary_info, paste0("Locations: ", nrow(location()), " towns"))
    }
    
    if (!is.null(clusters())) {
      n_clusters <- length(unique(clusters()$clustering))
      summary_info <- c(summary_info, paste0("Current Clusters: ", n_clusters))
    }
    
    tagList(
      if (length(summary_info) > 0) {
        tags$table(
          class = "table table-sm",
          style = "font-size: 0.9rem;",
          lapply(1:length(summary_info), function(i) {
            tags$tr(
              tags$td(style = "font-weight: bold;", names(summary_info)[i] %||% paste0("Data ", i, ":")),
              tags$td(summary_info[i])
            )
          })
        )
      } else {
        div(
          style = "text-align: center; color: #6c757d; padding: 20px;",
          "No data loaded yet"
        )
      }
    )
  })
  
  # Add this inside the server function
  output$downloadClustering <- downloadHandler(
    filename = function() {
      paste("clustering-results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(clusters())
      clusteringResult <- clusters()
      
      # Create a data frame with town names and their cluster assignments
      result_df <- data.frame(
        Town = rownames(clusteringResult$membership),
        Cluster = clusteringResult$clustering
      )
      
      # Write the data frame to a CSV file
      write.csv(result_df, file, row.names = FALSE)
    }
  )
  
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
       
        distmatrix <- distmatrix[correct_order, correct_order]
        
        dist_matrix(distmatrix)
        cleaned <<- TRUE
      }
      
      clusteringResult <- fanny(distmatrix, k = input$number_of_clusters, memb.exp = input$exponential)
      
      clusters(clusteringResult)
      
      distances <- c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d")
      clusterLabels <- unique(clusteringResult$clustering)
      
      clusterSizes <- table(clusteringResult$clustering) 
      
      updateSelectInput(session, "cluster1",
                        choices = paste0(names(clusterSizes), " (Number of towns: ", clusterSizes, ")"),
                        selected = 0)
      updateSelectInput(session, "cluster2",
                        choices = paste0(names(clusterSizes), " (Number of towns: ", clusterSizes, ")"),
                        selected = 0)
      shinyjs::hide("mainPanel")
      shinyjs::show("clusteringPanel")
    }, error = function(e) {
    showNotification(paste("Error performing clustering: Please check your input data", e$message), type = "error")
    })
  })

  
  get_cluster_number <- function(selected_option) {
    if (is.null(selected_option) || selected_option == "0" || selected_option == "All Clusters") { # Check for "0" and "All Clusters"
      return(0)
    } else {
      return(as.numeric(strsplit(selected_option, " ")[[1]][1]))
    }
  }
  
  observeEvent(input$compareClusters, {
    
    tryCatch({
      req(input$cluster1, input$cluster2, input$distanceFunction)
      
      shinyjs::hide("clusteringPanel")
      shinyjs::show("comparisonPanel")
      
      clusteringResult <- clusters()
      
      selected_distance_function <- reactive({
        switch(input$distanceFunction,
               bilbao_d = bilbao_d,
               levenshtein_d = levenshtein_d)
      })
      
      c1 <- get_cluster_number(input$cluster1)
      c2 <- get_cluster_number(input$cluster2)
      
      if (c1 == c2) {
        indices1 <- which(clusteringResult$clustering != c1)
        indices2 <- which(clusteringResult$clustering == c2)
      } else {
        indices1 <- which(clusteringResult$clustering == c1)
        indices2 <- which(clusteringResult$clustering == c2)
      }

      cluster1 <- list(cluster = c1, indices = indices1)
      cluster2 <- list(cluster = c2, indices = indices2)
      
      start.time <- Sys.time()
      
      withProgress(message = 'Processing data...', value = 0, {

          relevantItems(get_most_relevant_items_optimized(
            answers = answers(),
            questions = questions(),
            clusteringResult = clusters(),
            cluster1 = cluster1, 
            cluster2 = cluster2, 
            distantzia_funtzioa = selected_distance_function(),
            set_progress = function(value) { 
              
            }
          ))
      })
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      
      top_questions <- head(relevantItems()$top_questions, 10)
      
      output$topQuestionsTable <- DT::renderDataTable({
        req(relevantItems())
        
        data.frame(Questions = top_questions$galdera,
                   Diferentiation = top_questions$diferentziazioa,
                   Stability1 = top_questions$estabilitatea_1,
                   Stability2 = top_questions$estabilitatea_2,
                   Variability = top_questions$bariabilitatea1_2
        )
      }, selection = "single", options = list(pageLength = 10, dom = 't'))
    
    }, error = function(e) {
      showNotification(paste("Error comparing clusters: Check data or clustering result.", e$message), type = "error")
    })
  })
  
  observeEvent(input$cluster1, {
    req(input$cluster1)  # Ensure the input is available
    
    # Get the selected cluster number
    c1 <- get_cluster_number(input$cluster1)
    
    # Fetch the list of towns for the selected cluster
    clusteringResult <- clusters()  # Get clustering result
    
    if (c1 == 0) {
      # If "All Clusters" is selected, display all towns
      selected_towns <- rownames(clusteringResult$membership)
    } else {
      # Get the towns belonging to the selected cluster
      selected_towns <- rownames(clusteringResult$membership)[clusteringResult$clustering == c1]
    }
    
    output$townsTableLabel1 <- renderText({ paste0("Towns of Cluster ", get_cluster_number(input$cluster1)) })
    # Render the table with the selected towns
    output$townsTable1 <- renderTable({
      data.frame(Towns = selected_towns)
    })
  })
  
  observeEvent(input$cluster2, {
    req(input$cluster2)  # Ensure the input is available
    
    # Get the selected cluster number
    c2 <- get_cluster_number(input$cluster2)
    
    # Fetch the list of towns for the selected cluster
    clusteringResult <- clusters()  # Get clustering result
    
    if (c2 == 0) {
      # If "All Clusters" is selected, display all towns
      selected_towns <- rownames(clusteringResult$membership)
    } else {
      # Get the towns belonging to the selected cluster
      selected_towns <- rownames(clusteringResult$membership)[clusteringResult$clustering == c2]
    }
    
    output$townsTableLabel2 <- renderText({ paste0("Towns of Cluster ", get_cluster_number(input$cluster2)) })
    # Render the table with the selected towns
    output$townsTable2 <- renderTable({
      data.frame(Towns = selected_towns)
    })
  })
  
  
  observeEvent(input$n_items, {
    tryCatch({
      
      top_questions <- head(relevantItems()$top_questions, input$n_items)
      
      output$topQuestionsTable <- DT::renderDataTable({
        req(relevantItems())
        
        data.frame(Questions = top_questions$galdera,
                   Diferentiation = top_questions$diferentziazioa,
                   Stability1 = top_questions$estabilitatea_1,
                   Stability2 = top_questions$estabilitatea_2,
                   Variability = top_questions$bariabilitatea1_2
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
      
      if (get_cluster_number(input$cluster1) == get_cluster_number(input$cluster2)){
        output$cluster1Label <- renderText({ "All clusters" })
      } else {
        output$cluster1Label <- renderText({ paste0("Cluster ", get_cluster_number(input$cluster1)) })
      }
      output$cluster2Label <- renderText({ paste0("Cluster ", get_cluster_number(input$cluster2)) })
      
      cluster1_items <- unlist(relevantItems()$cluster1_items[selectedIndex, ])
      cluster2_items <- unlist(relevantItems()$cluster2_items[selectedIndex, ])
      
      cluster1_items <- unlist(strsplit(cluster1_items, ","))
      cluster2_items <- unlist(strsplit(cluster2_items, ","))
      

      cluster1_data <- as.data.frame(table(cluster1_items))
      cluster2_data <- as.data.frame(table(cluster2_items))
      
      if (get_cluster_number(input$cluster1) == get_cluster_number(input$cluster2)){
        cluster1_data$Cluster <- "All clusters"
      } else {
        cluster1_data$Cluster <- paste0("Cluster ", get_cluster_number(input$cluster1))
      }
      cluster2_data$Cluster <- paste0("Cluster ", get_cluster_number(input$cluster2))
      
      
      colnames(cluster1_data) <- c("Item", "Frequency", "Cluster")
      colnames(cluster2_data) <- c("Item", "Frequency", "Cluster")
      
      plot_data <- rbind(cluster1_data, cluster2_data)

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
  
  output$downloadTable <- downloadHandler(
    filename = function() {
      "relevant_questions_table.csv"
    },
    content = function(file) {
      req(relevantItems())
      top_questions <- head(relevantItems()$top_questions, input$n_items)
      
      # Get cluster numbers
      cluster1_num <- get_cluster_number(input$cluster1)
      cluster2_num <- get_cluster_number(input$cluster2)
      
      # Prepare data for download, including towns
      df_to_download <- data.frame(
        Questions = top_questions$galdera,
        Diferentiation = top_questions$diferentziazioa,
        Stability1 = top_questions$estabilitatea_1,
        Stability2 = top_questions$estabilitatea_2,
        Variability = top_questions$bariabilitatea1_2
      )
      
      # Handle column naming and ordering based on cluster selection
      if (cluster1_num == cluster2_num) {
        # When same cluster is selected, compare it with all other clusters
        selected_cluster_name <- paste0("Cluster_", cluster1_num)
        other_clusters_name <- "All_Other_Clusters"
        
        # Put selected cluster first, then all other clusters
        df_to_download[[selected_cluster_name]] <- sapply(1:nrow(top_questions), function(i) 
          paste(relevantItems()$cluster2_items[i,], collapse = ",")) 
        df_to_download[[paste0(other_clusters_name, "_Towns")]] <- sapply(1:nrow(top_questions), function(i) 
            paste(relevantItems()$cluster1_items[i,], collapse = ","))
      } else {
        # Different clusters selected - maintain order as shown in UI
        df_to_download[[paste0("Cluster_", cluster1_num, "_Towns")]] <- sapply(1:nrow(top_questions), function(i) 
          paste(relevantItems()$cluster1_items[i,], collapse = ","))
        df_to_download[[paste0("Cluster_", cluster2_num, "_Towns")]] <- sapply(1:nrow(top_questions), function(i) 
          paste(relevantItems()$cluster2_items[i,], collapse = ","))
      }
      
      write.csv(df_to_download, file, row.names = FALSE)
    })
  
  
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
