library(shiny)
library(shinyWidgets)
library(shinyjs)
library(cluster)
library(shinycssloaders)
library(stringdist)

bilbao_d <- function(items1, items2) {
  bat <- sum(items1 %in% items2) + sum(items2 %in% items1)
  result <- 1 - (bat / (length(items1) + length(items2)))
  return(result)
}

levenshtein_d <- function(items1, items2) {
  distances <- outer(items1, items2, Vectorize(function(a, b) stringdist(a, b, method = "lv")))
  
  max_length <- max(c(nchar(unlist(items1)), nchar(unlist(items2))), na.rm = TRUE)
  if (max_length == 0) return(1)  
  
  total_distance <- sum(distances, na.rm = TRUE)
  normalized_distance <- total_distance / (length(items1) * length(items2) * max_length)
  
  return(normalized_distance)
}

kalkulatu_estabilitatea <- function(answers, membership, galdera, cluster, distantzia_funtzioa) {
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
      
      if (is.na(answers[galdera, p_i]) || is.na(answers[galdera, p_j]) || 
          answers[galdera, p_i] == "" || answers[galdera, p_j] == "") {
        distantzia <- 1
      } else {
        distantzia <- distantzia_funtzioa(strsplit(answers[galdera, p_i], ",")[[1]], strsplit(answers[galdera, p_j], ",")[[1]])
      }
      
      pisuak <- membership[p_i, c] * membership[p_j, c]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  if (pisuen_batura == 0) return(NA)
  return(batura * 2 / pisuen_batura)
}


kalkulatu_bariabilitatea <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  c1 <- cluster1[[1]]
  elementuak1 <- cluster1[[2]]
  
  c2 <- cluster2[[1]]
  elementuak2 <- cluster2[[2]]
  
  batura <- 0
  pisuen_batura <- 0
  
  for (i in elementuak1) {
    for (j in elementuak2) {
      if (is.na(answers[galdera, i]) || is.na(answers[galdera, j]) || 
          answers[galdera, i] == "" || answers[galdera, j] == "") {
        distantzia <- 1
      } else {
        distantzia <- distantzia_funtzioa(strsplit(answers[galdera, i], ",")[[1]], 
                               strsplit(answers[galdera, j], ",")[[1]])
      }
      
      pisuak <- membership[i, c1] * membership[j, c2]
      pisuen_batura <- pisuen_batura + pisuak
      batura <- batura + distantzia * pisuak
    }
  }
  
  return(ifelse(pisuen_batura > 0, batura / pisuen_batura, NA))
}

kalkulatu_diferentziazioa <- function(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa) {
  estabilitatea1 <- kalkulatu_estabilitatea(answers, membership, galdera, cluster1, distantzia_funtzioa)
  estabilitatea2 <- kalkulatu_estabilitatea(answers, membership, galdera, cluster2, distantzia_funtzioa)
  
  if (is.na(estabilitatea1) || is.na(estabilitatea2)) return(NA)
  

  bariabilitatea1_2 <- kalkulatu_bariabilitatea(answers, membership, galdera, cluster1, cluster2, distantzia_funtzioa)

  diferentziazioa = bariabilitatea1_2 / max(estabilitatea1, estabilitatea2, 0.1)
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
      DT::dataTableOutput("topQuestionsTable"),
      h3("Cluster Items for Selected Question", style = "padding: 10px;"),
      div(
        style = "padding: 10px;",
        h4("Cluster 1 Items"),
        tableOutput("cluster1Table"),
        h4("Cluster 2 Items", style = "margin-top: 20px;"),
        tableOutput("cluster2Table")
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
      
      
      shinyjs::hide("mainPanel")
      shinyjs::show("clusteringPanel")
    }, error = function(e) {
    showNotification(paste("Error performing clustering: Please check your input data.Error importing Locations:", e$message), type = "error")
    })
  })

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
      
      withProgress(message = 'Processing data...', value = 0, {
        relevantItems(get_most_relevant_items(
          answers = answers(),
          questions = questions(),
          clusteringResult = clusters(),
          c1 = as.numeric(input$cluster1),
          c2 = as.numeric(input$cluster2),
          n = input$n_items,
          distantzia_funtzioa = selected_distance_function(),
          set_progress = function(value) { # Pass setProgress function
            setProgress(value = value)
          }
        ))
      })
      
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
