library(shiny)
library(shinyjs)
library(cluster)
#library(shinycssloaders)

bilbao_d <- function(items1, items2) {
  bat <- sum(items1 %in% items2) + sum(items2 %in% items1)
  result <- 1 - (bat / (length(items1) + length(items2)))
  return(result)
}

levenshtein_d <- function(items1, items2) {
  # Compute the sum of Levenshtein distances for all string pairs
  distances <- outer(items1, items2, Vectorize(function(a, b) stringdist(a, b, method = "lv")))
  
  # Normalize the total distance
  max_length <- max(c(nchar(unlist(items1)), nchar(unlist(items2))), na.rm = TRUE)
  if (max_length == 0) return(1)  # Avoid division by zero for empty inputs
  
  total_distance <- sum(distances, na.rm = TRUE)
  normalized_distance <- total_distance / (length(items1) * length(items2) * max_length)
  
  return(normalized_distance)
}

kalkulatu_estabilitatea <- function(datubasea, membership, galdera, cluster, distantzia_funtzioa) {
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
        distantzia <- distantzia_funtzioa(strsplit(datubasea[galdera, p_i], ",")[[1]], strsplit(datubasea[galdera, p_j], ",")[[1]])
      }
      
      pisuak <- membership[p_i, c] * membership[p_j, c]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  if (pisuen_batura == 0) return(NA)
  return(batura * 2 / pisuen_batura)
}


kalkulatu_bariabilitatea <- function(datubasea, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
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
        distantzia <- distantzia_funtzioa(strsplit(datubasea[galdera, i], ",")[[1]], 
                               strsplit(datubasea[galdera, j], ",")[[1]])
      }
      
      pisuak <- membership[i, c1] * membership[j, c2]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  return(ifelse(pisuen_batura > 0, batura / pisuen_batura, NA))
}

kalkulatu_diferentziazioa <- function(datubasea, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  estabilitatea1 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster1, distantzia_funtzioa)
  estabilitatea2 <- kalkulatu_estabilitatea(datubasea, membership, galdera, cluster2, distantzia_funtzioa)
  
  if (is.na(estabilitatea1) || is.na(estabilitatea2)) return(NA)
  
  bariabilitatea1_2 <- kalkulatu_bariabilitatea(datubasea, membership, galdera, cluster1, cluster2, distantzia_funtzioa)
  return(bariabilitatea1_2 / max(estabilitatea1, estabilitatea2, 1e-6))
}

get_most_relevant_items <- function(datubasea, questions, data, clusteringResult, c1, c2, n, distantzia_funtzioa){
  clusters <- clusteringResult$clustering
  membership <- clusteringResult$membership
  
  indices1 <- which(clusters == c1)
  indices2 <- which(clusters == c2)
  
  cluster1 <- list(c1, indices1)
  cluster2 <- list(c2, indices2)
  
  diferentziazioak <- numeric(length(datubasea))
  
  for (galdera in 1:dim(datubasea)[1]) {
    diferentziazioak[galdera] <- kalkulatu_diferentziazioa(datubasea, membership, galdera, cluster1, cluster2, distantzia_funtzioa)
  }
  
  
  
  sorted_indices <- order(diferentziazioak, decreasing = TRUE)
  
  
  
  top3_items <- sorted_indices[1:n]
  
  top_questions <- questions[top3_items]

  cluster1_items <- datubasea[top3_items, indices1]
  cluster2_items <- datubasea[top3_items, indices2]
  
  xnames <- c(colnames(cluster1_items))
  ind <- as.integer(sub('.', '', xnames))
  ind <- ind + 2
  colnames(cluster1_items) <- names(data)[ind]
  
  
  xnames <- c(colnames(cluster2_items))
  ind <- as.integer(sub('.', '', xnames))
  ind <- ind + 2 
  colnames(cluster2_items) <- names(data)[ind]
  
  output <- list("top_questions" = top_questions, "cluster1_items" = cluster1_items, "cluster2_items" = cluster2_items)
  
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
      selectInput("cluster1", "Select Cluster 1:", choices = 1),
      selectInput("cluster2", "Select Cluster 2:", choices = 1),
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
      withSpinner(DT::dataTableOutput("topQuestionsTable")),
      h3("Cluster Items for Selected Question", style = "padding: 10px;"),
      div(
        style = "padding: 10px;",
        h4("Cluster 1 Items"),
        tableOutput("cluster1Table"),
        h4("Cluster 2 Items", style = "margin-top: 20px;"),
        tableOutput("cluster2Table")
      ),
      actionButton("backToClusteringPanel", "Back")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store the data
  data <- reactiveVal()
  questions <- reactiveVal()
  answers <- reactiveVal()
  clusters <- reactiveVal()
  relevantItems <- reactiveVal()
  
  # File import with error handling
  observeEvent(input$fileInput, {
    tryCatch({
      req(input$fileInput)
      file <- input$fileInput$datapath
      data(read.csv(file, stringsAsFactors = FALSE))
      output$fileInputError <- renderText({ "" })  # Clear error message
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
      answers(as.matrix(read.csv(file, stringsAsFactors = FALSE, row.names = 1)))
      output$answersFileError <- renderText({ "" })  # Clear error message
      showNotification("Answers imported successfully!", type = "message")
    }, error = function(e) {
      output$answersFileError <- renderText({ paste("Error importing Answers:", e$message) })
      showNotification("Failed to import Answers!", type = "error")
    })
  })
  
  # Perform clustering using FANNY
  observeEvent(input$performClustering, {
    tryCatch({
      req(data())
      numericData <- data()
      numericData <- numericData[sapply(numericData, is.numeric)]
      numericData[is.na(numericData)] <- 0
      distMatrix <- dist(numericData)
      clusteringResult <- fanny(distMatrix, k = input$number_of_clusters, memb.exp = input$exponential)
      
      #write.csv(clusteringResult$, "./uneko_membership.csv", row.names = FALSE)
      #write.csv(clusteringResult$clustering, "./uneko_clusters_3.csv", row.names = FALSE)
      
      clusters(clusteringResult)
      
      distances <- c("Bilbao Distance" = "bilbao_d", "Levenshtein Distance" = "levenshtein_d")
      clusterLabels <- unique(clusteringResult$clustering)
      updateSelectInput(session, "cluster1", choices = clusterLabels)
      updateSelectInput(session, "cluster2", choices = clusterLabels)
      
      
      shinyjs::hide("mainPanel")
      shinyjs::show("clusteringPanel")
    }, error = function(e) {
    showNotification("Error performing clustering: Please check your input data.", type = "error")
    })
  })

  
  # Compare clusters
  observeEvent(input$compareClusters, {
    
    
    tryCatch({
      req(input$cluster1, input$cluster2, input$distanceFunction, questions(), answers())
      
      shinyjs::hide("clusteringPanel")
      shinyjs::show("comparisonPanel")
      
      selected_distance_function <- reactive({
        switch(input$distanceFunction,
               bilbao_d = bilbao_d,
               levenshtein_d = levenshtein_d)
      })
      
      relevantItems(get_most_relevant_items(
        datubasea = answers(),
        questions = questions(),
        data = data(),
        clusteringResult = clusters(),
        c1 = as.numeric(input$cluster1),
        c2 = as.numeric(input$cluster2),
        n = input$n_items,
        distantzia_funtzioa = selected_distance_function()
      ))
      
      
      output$topQuestionsTable <- DT::renderDataTable({
        req(relevantItems())
        data.frame(Questions = relevantItems()$top_questions)
      }, selection = "single", options = list(pageLength = input$n_items, dom = 't'))
    
    
    }, error = function(e) {
      showNotification("Error comparing clusters: Check data or clustering result.", type = "error")
    })
  })
  
  
  observeEvent(input$topQuestionsTable_rows_selected, {
    tryCatch({
      req(input$topQuestionsTable_rows_selected, relevantItems())
      
      selectedIndex <- input$topQuestionsTable_rows_selected
      
      # Render Cluster 1 Items
      output$cluster1Table <- renderTable({
        relevantItems()$cluster1_items[selectedIndex, , drop = FALSE]
      })
      
      # Render Cluster 2 Items
      output$cluster2Table <- renderTable({
        relevantItems()$cluster2_items[selectedIndex, , drop = FALSE]
      })
      
    }, error = function(e) {
    showNotification("Error displaying cluster items.", type = "error")
    })
  })
  
  # Navigate back to the clustering panel
  observeEvent(input$backToClusteringPanel, {
    shinyjs::hide("comparisonPanel")
    shinyjs::show("clusteringPanel")
    
    # Clear tables when navigating back
    relevantItems <- reactiveVal()
    output$topQuestionsTable <- DT::renderDataTable({ NULL })
    output$cluster1Table <- renderTable({ NULL })
    output$cluster2Table <- renderTable({ NULL })
  })
  
  # Navigate back to the main panel
  observeEvent(input$backToMainPanel, {
    shinyjs::hide("clusteringPanel")
    shinyjs::show("mainPanel")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
