library(shiny)
library(shinyjs)
library(cluster)
library(shinycssloaders)

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

get_most_relevant_items <- function(datubasea, clusteringResult, c1, c2){
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
  
  top3_items <- sorted_indices[1:3]
  
  top_questions <- questions[top3_items]
  
  cluster1_items <- datubasea[top3_items, indices1]
  cluster2_items <- datubasea[top3_items, indices2]
  output <- list("top_questions" = top_questions, "cluster1_items" = cluster1_items, "cluster2_items" = cluster2_items)
  
  return(output)
}

# Define UI for the app
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Diatech Fuzzy Clustering Demo"),
  div(
    id = "mainPanel",
    sidebarLayout(
      sidebarPanel(
        fileInput("fileInput", "Import Distance Matrix", accept = ".csv"),
        fileInput("questionsFile", "Import Questions", accept = ".csv"),
        fileInput("answersFile", "Import Answers", accept = ".csv"),
        numericInput("number_of_clusters", "Number of Clusters:", value = 6, min = 2, step = 1),
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
      selectInput("cluster1", "Select Cluster 1:", choices = NULL, multiple = FALSE),
      selectInput("cluster2", "Select Cluster 2:", choices = NULL, multiple = FALSE),
      actionButton("compareClusters", "Compare Clusters"),
      actionButton("backButton", "Back")
    )
  ),
  hidden(
    div(
      id = "comparisonPanel",
      h3("Relevant Questions (Pink Table)", style = "background-color: pink; padding: 10px;"),
      tableOutput("topQuestionsTable"),  # Pink table to display top questions
      actionButton("showItems", "Show Items", style = "background-color: yellow; color: black; margin: 10px;"),  # Yellow button
      h3("Cluster Items for Selected Question (Green Tables)", style = "background-color: lightgreen; padding: 10px;"),
      div(style = "display: flex; gap: 20px;",
          div(
            style = "flex: 1;",
            tableOutput("cluster1Table")  # Green table for Cluster 1
          ),
          div(
            style = "flex: 1;",
            tableOutput("cluster2Table")  # Green table for Cluster 2
          )
      ),
      actionButton("backToClusteringPanel", "Back")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values to store the data
  data <- reactiveVal()
  questions <- reactiveVal()
  answers <- reactiveVal()
  clusters <- reactiveVal()
  
  # Observe file inputs and read CSVs
  observeEvent(input$fileInput, {
    req(input$fileInput)
    file <- input$fileInput$datapath
    data(read.csv(file, stringsAsFactors = FALSE))
    # output$table <- renderTable({
    #   data()
    # })
  })
  
  observeEvent(input$questionsFile, {
    req(input$questionsFile)
    file <- input$questionsFile$datapath
    questions(read.csv(file, stringsAsFactors = FALSE, row.names = 1))
  })
  
  observeEvent(input$answersFile, {
    req(input$answersFile)
    file <- input$answersFile$datapath
    answers(as.matrix(read.csv(file, stringsAsFactors = FALSE, row.names=1)))
  })
  
  # Perform clustering using FANNY
  observeEvent(input$performClustering, {
    req(data())
    
    # Ensure numeric data for clustering
    numericData <- data()
    numericData <- numericData[sapply(numericData, is.numeric)] # Filter numeric columns
    numericData[is.na(numericData)] <- 0 # Replace NA with 0
    
    distMatrix <- dist(numericData) # Compute distance matrix
    clusteringResult <- fanny(distMatrix, k = input$number_of_clusters, memb.exp = input$exponential)
    clusters(clusteringResult)
    
    clusterLabels <- unique(clusteringResult$clustering)
    updateSelectInput(session, "cluster1", choices = clusterLabels)
    updateSelectInput(session, "cluster2", choices = clusterLabels)
    
    shinyjs::hide("mainPanel")
    shinyjs::show("clusteringPanel")
  })
  
  # Compare two clusters
  # observeEvent(input$compareClusters, {
  #   req(input$cluster1, input$cluster2, questions(), answers())
  # 
  #   relevant_items <- get_most_relevant_items(answers(), clusters(), as.numeric(input$cluster1), as.numeric(input$cluster2))
  #   
  #   
  #   output$comparisonDetails <- renderPrint({
  #     relevant_items
  #     
  #   })
  #   
  #   shinyjs::hide("clusteringPanel")
  #   shinyjs::show("comparisonPanel")
  # })
  
  observeEvent(input$compareClusters, {
    req(input$cluster1, input$cluster2, questions(), answers())
    relevant_items <- get_most_relevant_items(answers(), clusters(), as.numeric(input$cluster1), as.numeric(input$cluster2))
    
    output$topQuestionsTable <- renderTable({
      data.frame(Index = seq_along(relevant_items$top_questions),
                 Questions = relevant_items$top_questions)
    }, rownames = TRUE)
    
    selectedQuestion(NULL)  # Clear any previous selection
    shinyjs::hide("clusteringPanel")
    shinyjs::show("comparisonPanel")
  })
  
  observeEvent(input$showItems, {
    req(input$topQuestionsTable_rows_selected)  # Ensure a question is selected
    selectedQuestion(input$topQuestionsTable_rows_selected)  # Store selected question index
    
    relevant_items <- get_most_relevant_items(answers(), clusters(), as.numeric(input$cluster1), as.numeric(input$cluster2))
    selected_index <- selectedQuestion()
    
    output$cluster1Table <- renderTable({
      relevant_items$cluster1_items[selected_index, , drop = FALSE]
    })
    output$cluster2Table <- renderTable({
      relevant_items$cluster2_items[selected_index, , drop = FALSE]
    })
  })
  
  observeEvent(input$backToClusteringPanel, {
    shinyjs::hide("comparisonPanel")
    shinyjs::show("clusteringPanel")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
