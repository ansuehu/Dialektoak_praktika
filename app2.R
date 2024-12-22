library(shiny)
library(shinyjs)
library(cluster)

# Define UI for the app
ui <- fluidPage(
  useShinyjs(),
  titlePanel("CSV Importer and FANNY Clustering"),
  div(
    id = "mainPanel",
    sidebarLayout(
      sidebarPanel(
        fileInput("fileInput", "Choose Data CSV File", accept = ".csv"),
        fileInput("questionsFile", "Choose Questions CSV File", accept = ".csv"),
        fileInput("answersFile", "Choose Answers CSV File", accept = ".csv"),
        numericInput("number_of_clusters", "Number of Clusters:", value = 3, min = 2, step = 1),
        numericInput("exponential", "Exponential Parameter:", value = 2, min = 1, step = 0.1),
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
      verbatimTextOutput("comparisonDetails"),
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
    output$table <- renderTable({
      data()
    })
  })
  
  observeEvent(input$questionsFile, {
    req(input$questionsFile)
    file <- input$questionsFile$datapath
    questions(read.csv(file, stringsAsFactors = FALSE))
  })
  
  observeEvent(input$answersFile, {
    req(input$answersFile)
    file <- input$answersFile$datapath
    answers(read.csv(file, stringsAsFactors = FALSE))
  })
  
  # Perform clustering using FANNY
  observeEvent(input$performClustering, {
    req(data())
    distMatrix <- dist(data())
    clusteringResult <- fanny(distMatrix, k = input$number_of_clusters, memb.exp = input$exponential)
    clusters(clusteringResult)
    clusterLabels <- unique(clusteringResult$clustering)
    updateSelectInput(session, "cluster1", choices = clusterLabels)
    updateSelectInput(session, "cluster2", choices = clusterLabels)
    shinyjs::hide("mainPanel")
    shinyjs::show("clusteringPanel")
  })
  
  # Compare two clusters
  observeEvent(input$compareClusters, {
    req(input$cluster1, input$cluster2)
    cluster1 <- as.numeric(input$cluster1)
    cluster2 <- as.numeric(input$cluster2)
    req(clusters())
    clusteringResult <- clusters()
    
    # Example comparison function
    compareFunction <- function(cluster1, cluster2, clusteringResult) {
      list(
        Cluster1Size = sum(clusteringResult$clustering == cluster1),
        Cluster2Size = sum(clusteringResult$clustering == cluster2),
        Overlap = sum((clusteringResult$clustering == cluster1) & (clusteringResult$clustering == cluster2))
      )
    }
    
    comparisonResult <- compareFunction(cluster1, cluster2, clusteringResult)
    output$comparisonDetails <- renderPrint({
      comparisonResult
    })
    shinyjs::hide("clusteringPanel")
    shinyjs::show("comparisonPanel")
  })
  
  # Handle back button to return to the main panel
  observeEvent(input$backButton, {
    shinyjs::hide("clusteringPanel")
    shinyjs::show("mainPanel")
  })
  
  # Handle back button to return to clustering panel from comparison panel
  observeEvent(input$backToClusteringPanel, {
    shinyjs::hide("comparisonPanel")
    shinyjs::show("clusteringPanel")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
