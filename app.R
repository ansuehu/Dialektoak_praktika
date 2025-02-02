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

levenshtein_d <- function(items1, items2) {
  distances <- stringdistmatrix(items1, items2, method = "lv") 
  
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
  return (data.frame(galdera = galdera, diferentziazioa = diferentziazioa, estabilitatea_1 = estabilitatea1, estabilitatea_2= estabilitatea2, bariabilitatea1_2= bariabilitatea1_2))
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
    sidebarLayout(
      sidebarPanel(
        fileInput("fileInput", "Import Distance Matrix", accept = ".csv"),
        textOutput("fileInputError"),
        fileInput("questionsFile", "Import Questions", accept = ".csv"),
        textOutput("questionsFileError"),
        fileInput("answersFile", "Import Answers", accept = ".csv"),
        textOutput("answersFileError"),
        fileInput("locationFile", "Import Location", accept = ".csv"),
        textOutput("locationFileError"), 
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
      selectInput("cluster1", "Select Cluster 1:", 
                  choices = c("All Clusters" = 0, 1), 
                  label = NULL),
      textOutput("cluster1Size"), 
      selectInput("cluster2", "Select Cluster 2:", 
                  choices = c("All Clusters" = 0, 1), 
                  label = NULL),
      textOutput("cluster2Size"), 
      selectInput("distanceFunction", "Select Distance:", choices = c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d")),
      actionButton("compareClusters", "Compare Clusters"),
      actionButton("backToMainPanel", "Back")
    )
  ),
  hidden(
    div(
      id = "comparisonPanel",
      numericInput("n_items", "Number of Relevant items:", value = 10, min = 0, step = 1),
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
        downloadButton("downloadPlot", "Download Plot")
        
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
