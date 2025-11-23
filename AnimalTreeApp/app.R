
library(ape)
library(shiny)
library(ggplot2)
library(plotly)
library(ggtree)
library(bslib)

load("ggTreeObject")
tip_info <- read.csv("InvertDescriptions.csv")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .main-panel, .container-fluid {
        padding-bottom: 60px; /* adjust to match navbar height */
      }
    ")) # to fix issue where nav bar overlapped with main panel 
  ),
  theme = bs_theme(preset = "morph"),
  titlePanel("Metazoa Tree of Life"), #the title
  sidebarLayout(
    sidebarPanel(
      h4("Info"),
      uiOutput("tipPanel")),
    mainPanel(
      plotOutput("TreePlot", height = "600px", click = "plot_click")
    )
  ),
  # Footer navbar
  tags$head(
    tags$style(HTML("
    /* Always reserve space equal to footer height */
    body { margin-bottom: 40px; } /* adjust after testing */

    /* Full-width fixed footer with no outer padding that could shift content */
    .app-footer {
      position: fixed;
      bottom: 0;
      left: 0;
      right: 0;
      width: 100%;
      z-index: 1030;
      border-top: 1px solid #ddd;
      background-color: var(--bs-light);
    }

    /* Inner container controls actual content width and alignment */
    .app-footer .container {
      padding: 4px 4px;        /* your desired inner padding */
    }

    /* Ensure columns don't collapse or inherit odd spacing */
    .app-footer .row { margin-left: 0; margin-right: 0; }
    .app-footer .col { padding-left: 0; padding-right: 0; }

    /* Optional: make the main content scroll above the footer without overlap */
    .container-fluid, .main-panel {
      padding-bottom: 40px;      /* matches body margin-bottom */
    }
    
    .app-footer p {
    margin: 0;          /* remove extra paragraph spacing */
    line-height: 1.2;   /* tighten text line height */
    font-size: 0.9rem;  /* optional: slightly smaller text */
  }
  "))
  ),
  tags$div(
    class = "app-footer bg-light fixed-bottom",
    style = "border-top:1px solid #ddd;",
    div(class = "container-fluid py-2",
        div(class = "row align-items-center",
            div(class = "col-12 col-md-4",
                tags$p("2025 Tree Project / Kyle Hudson")),
            div(class = "col-12 col-md-4 text-md-center",
                tags$p(
                  tags$a("GitHub", href="https://github.com/kphudson13/AnimalTreeApp", target="_blank"),
                  " | ",
                  tags$a("Contact", href="mailto:hudson.k@ufl.edu"))),
            div(class = "col-12 col-md-4 text-md-end",
                tags$p(
                  "Hosted by ",
                  tags$a("Shiny", href="https://www.shinyapps.io/")))
        )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive tree object
  make_tree <- reactive({
    TreePlot + ggplot2::xlim(NA, 11)
  })
  
  # Render tree plot
  output$TreePlot <- renderPlot({
    make_tree()
  })
  
  output$tipPanel <- renderUI({
    click <- input$plot_click
    if (is.null(click)) {
      return(
        tagList(tags$p("Click a tip label to see details"),
                tags$img(src = "Photos/Metazoa.jpg", 
                         style = "width:100%; height:auto;") 
        )
      )
    } # what shows up before a click 
    
    tree_data <- TreePlot$data
    nearest <- nearPoints(tree_data, click, xvar = "x", yvar = "y", threshold = 50, maxpoints = 1)
    if (nrow(nearest) == 0) return(tags$p("No tip label detected")) # if you miss
    
    label_clicked <- nearest$label
    info <- tip_info %>% filter(Clade == label_clicked)
    
    desc <- if (nrow(info) == 0) {
      paste0("Label: ", label_clicked, "\nNo description available.")
    } else {
      paste0(info$Level, ": ", info$Clade, "\n", info$CommonName, "\n", info$Description)
    } # paste descriptive info from csv
    
    # Use ImageLink column if available
    img_link_text <- if (nrow(info) > 0 && !is.na(info$ImageLink)) info$ImageLink else NULL
    
    # Check image file existence
    exts <- c(".jpeg", ".jpg", ".png") # to support multiple file types
    file_path <- NULL
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext)) # theyre all in www so shiny finds them 
      if (file.exists(candidate)) {
        file_path <- paste0("Photos/", label_clicked, ext)  # relative path for browser
        break
      }
    }
    
    tagList(
      tags$pre(
        style = "white-space: pre-wrap; overflow-wrap: normal; word-break: normal;",
        desc
      ), # wrap the description within the side bar
      if (!is.null(file_path)) {
        tagList(
          tags$img(src = file_path, style = "width:100%; height:auto;"),
          tags$p(img_link_text)
        )
      })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
