
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
  # Intro section always visible
  fluidRow(
    column(
      width = 3,  # text takes most of the row
      h2("Metazoa Tree of Life"),  # use h2 for a nice title size
      h4("Click a label to see details")
    ),
    column(
      width = 9,  # smaller column for the image
      tags$img(
        src = "Photos/Intro.png",
        style = "width:100%; height:auto;;", # shrink image
        alt = "Metazoa overview image"
      )
    )
  ),
  
  
  # Main panel now full width
  fluidRow(
    column(
      width = 12,
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

server <- function(input, output, session) {
  
  make_tree <- reactive({
    TreePlot + ggplot2::xlim(NA, 11)
  })
  
  output$TreePlot <- renderPlot({
    make_tree()
  })
  
  observeEvent(input$plot_click, {
    tree_data <- TreePlot$data
    nearest <- nearPoints(tree_data, input$plot_click,
                          xvar = "x", yvar = "y",
                          threshold = 50, maxpoints = 1)
    if (nrow(nearest) == 0) return()
    
    label_clicked <- nearest$label
    info <- tip_info %>% filter(Clade == label_clicked)
    
    desc <- if (nrow(info) == 0) {
      paste0("Label: ", label_clicked, "\nNo description available.")
    } else {
      paste0(info$Level, ": ", info$Clade, "\n",
             info$CommonName, "\n", info$Description)
    }
    
    # Image fallback logic
    exts <- c(".jpeg", ".jpg", ".png")
    file_path <- NULL
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext))
      if (file.exists(candidate)) {
        file_path <- paste0("Photos/", label_clicked, ext)
        break
      }
    }
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        
        # Left side: title text
        span(paste("Details for", label_clicked)),
        
        # Right side: close button
        tags$button(
          type = "button",
          class = "btn-close",   # Bootstrap 5 close icon
          `data-bs-dismiss` = "modal",
          `aria-label` = "Close",
          style = "margin-left:20px;"  # small spacing so it doesn't touch the title
        )
      ),
      
      tagList(
        tags$pre(style = "white-space: pre-wrap;", desc),
        if (!is.null(file_path)) {
          tags$img(src = file_path,
                   style = "width:100%; height:auto;",
                   alt = paste("Image of", label_clicked))
        },
        if (nrow(info) > 0 && !is.na(info$ImageLink)) {
          tags$p(tags$a("More info", href = info$ImageLink, target = "_blank"))
        }
      ),
      
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
