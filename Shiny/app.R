library(shiny)
covidUS <- read.csv("covidUS.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hierarchical Clustering"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing linkage ----
      selectInput(inputId = "linkage",
                  label = "Choose a linkage for hierarchical clustering:",
                  choices = c("single", "complete", "average", "centroid")),
      
      # Input: Numeric entry for number of clusters ----
      numericInput(inputId = "k",
                   label = "Number of clusters:",
                   value = 7,
                   min = 2,
                   max = 30)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: HTML table with requested number of observations ----
      tableOutput("clusterTab")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Show the first "n" observations ----
  output$clusterTab <- renderTable({
    Delta <- dist(covidUS)
    hiera <- hclust(Delta, method=input$linkage)
    Cluster <- cutree(hiera, k=input$k)
    table(Cluster)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)