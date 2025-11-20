

library(ape)
library(shiny)
library(ggplot2)
library(plotly)
library(ggtree)

load("ggTreeObject")
tip_info <- read.csv("InvertDescriptions.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Metazoa Tree of Life"),
  sidebarLayout(
    sidebarPanel(
      h4("Info"),
      verbatimTextOutput("tipDetails")
    ),
    mainPanel(
      plotOutput("TreePlot", height = "600px", click = "plot_click")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # Reactive tree object
  make_tree <- reactive({
    TreePlot + ggplot2::xlim(NA, 10)
  })
  
  # Render tree plot
  output$TreePlot <- renderPlot({
    make_tree()
  })
  
  # Display tip info on click
  output$tipDetails <- renderPrint({
    click <- input$plot_click
    if (is.null(click)) return("Click a tip label to see details")
    
    # Extract tree data from TreePlot object
    tree_data <- TreePlot$data
    
    # Find nearest tip
    nearest <- nearPoints(tree_data, click, threshold = 50, maxpoints = 1)
    if (nrow(nearest) == 0) return("No tip label detected")
    
    label_clicked <- nearest$label
    info <- tip_info %>% filter(Clade == label_clicked)
    
    if (nrow(info) == 0) return(cat(paste0("Label:", label_clicked, "\n", "No description available.")))
    
    cat(paste0(info$Level, ": ", info$Clade, "\n", info$CommonName, "\n", info$Description))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
