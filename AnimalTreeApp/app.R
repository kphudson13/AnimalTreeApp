
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
      uiOutput("tipPanel")
    )
    ,
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
  
  output$tipPanel <- renderUI({
    click <- input$plot_click
    if (is.null(click)) return(tags$p("Click a tip label to see details"))
    
    tree_data <- TreePlot$data
    nearest <- nearPoints(tree_data, click, xvar = "x", yvar = "y", threshold = 50, maxpoints = 1)
    if (nrow(nearest) == 0) return(tags$p("No tip label detected"))
    
    label_clicked <- nearest$label
    info <- tip_info %>% filter(Clade == label_clicked)
    
    desc <- if (nrow(info) == 0) {
      paste0("Label: ", label_clicked, "\nNo description available.")
    } else {
      paste0(info$Level, ": ", info$Clade, "\n", info$CommonName, "\n", info$Description)
    }
    
    # Check file existence
    exts <- c(".jpeg", ".jpg", ".png")
    file_path <- NULL
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext))
      if (file.exists(candidate)) {
        file_path <- paste0("Photos/", label_clicked, ext)  # relative path for browser
        break
      }
    }
    
    tagList(
      tags$pre(desc),
      if (!is.null(file_path)) {
        tags$img(src = file_path, style = "width:100%; height:auto;")
      }
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
