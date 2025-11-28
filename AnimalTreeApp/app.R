
######################
# 
# developed Nov 2025 by Kyle Hudson
# kphudson@live.ca
# This app is designed as a teaching tool, the taxonomy is incomplete
# 
# Live laugh love
# -Kyle 
# 
######################

library(ape)
library(shiny)
library(ggplot2)
library(plotly)
library(ggtree)
library(bslib) # for the web app theme 

load("ggTreeObject")
tip_info <- read.csv("InvertDescriptions.csv")
tree <- read.nexus("tree.nex", tree.names = "tree")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('Photos/Background.png'); /* path inside www/Photos */
        background-size: cover;       /* scale to fill screen */
        background-repeat: no-repeat; /* don’t tile */
        background-attachment: fixed; /* stays fixed when scrolling */
      }
    "))
  ),
  
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
    style = "display:flex;", # this just centers the title better 
    column(
      width = 3,  # text takes less of the row
      h2("Metazoa Tree of Life", style = "margin-top:30px;"),  # use h2 for a nice title size
      h4("Click a label to see details")
    ),
    column(
      width = 9,  # larger column for the image
      tags$img(
        src = "Photos/Intro.png",
        style = "width:100%; height:auto;", # shrink image
        alt = "Metazoa overview image"
      )
    )
  ),
  
  # Main panel 
  fluidRow(
    tags$head(
      tags$style(HTML("
        .shiny-plot-output {
        background-color: transparent !important; /* Make plot container transparent */
      }
    "))
    ),
    column(
      width = 12, # full width
      plotOutput("TreePlot", 
                 height = paste0(30*length(tree$tip.label), "px"), # height is dynamic, based off number of tips
                 click = "plot_click"))
  ), 
  
  # Footer navbar
  tags$head(
    tags$style(HTML("
    body { margin-bottom: 40px; } /* reserve space equal to footer height */

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
      padding: 4px 4px;        /* inner padding */
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
  ")) # nested HTML to make the footer work with this theme 
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
                  actionLink("credits", "Credits"))  # clickable link
            )
            
        ))
  )
  
)

server <- function(input, output, session) {
  
  make_tree <- reactive({
    xmax <- max(TreePlot$data$x, na.rm = TRUE)
    TreePlot + ggplot2::xlim(NA, xmax + 1) # add buffer
  })
  
  output$TreePlot <- renderPlot({
    make_tree()
  }, bg = "transparent")
  
  observeEvent(input$plot_click, { # all for the main panel
    tree_data <- TreePlot$data
    nearest <- nearPoints(tree_data, input$plot_click,
                          xvar = "x", yvar = "y",
                          threshold = 40, maxpoints = 1)
    if (nrow(nearest) == 0) return() # stops app from crashing if you click randomly 
    
    label_clicked <- nearest$label # the label that was clicked
    info <- tip_info %>% filter(Clade == label_clicked) # save object for only that clade
    
    desc <- if (nrow(info) == 0) {
      paste0("Label: ", label_clicked) # back up for missing labels
    } else {
      paste0(info$CommonName, "\n", info$Description)
    } # save description to print 
    
    exts <- c(".jpeg", ".jpg", ".png") # Image fallback logic
    file_path <- NULL # reset just incase 
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext))
      if (file.exists(candidate)) { # this is only necessary if you're not sure every photo exists 
        file_path <- paste0("Photos/", label_clicked, ext) # store filepath for the desired photo
        break 
      }
    }
    
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        
        # Left side: title text
        span(
          if (nrow(info) == 0) {
            paste0("No description available") # back up for missing labels
          } else {
            paste0(info$Level, ": ", info$Clade) # ptherwise print title 
          }
        ),
        # Right side: close button
        tags$button(
          type = "button",
          class = "btn-close",   # Bootstrap 5 close icon
          `data-bs-dismiss` = "modal",
          `aria-label` = "Close",
          style = "margin-left:20px;"  # small spacing so it doesn't touch the title
        )
      ), # before this fits in title
      
      tagList(
        tags$pre(style = "white-space: pre-wrap;", desc
        ), # description
        if (!is.null(file_path)) {
          tags$img(src = file_path,
                   style = "width:100%; height:auto;",
                   alt = paste("Image of", label_clicked)) # fallback text
        }, # render image in www
        if (nrow(info) > 0 && !is.na(info$ImageLink)) {
          tags$p(info$ImageLink, style = "font-size:0.8rem") 
        } # image link, smaller font
      ), 
      
      easyClose = TRUE,
      footer = NULL
    )) 
    
  })
  
  observeEvent(input$credits, {
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        
        span("Credits"), # Left side: title text
        
        tags$button( 
          type = "button",
          class = "btn-close",   # Bootstrap 5 close icon
          `data-bs-dismiss` = "modal",
          `aria-label` = "Close",
          style = "margin-left:20px;"  # spacing so it doesn’t touch the title
        ) # Right side: close button
      ),
      
      tagList(
        tags$p("Hosted by ",
               tags$a("Shiny", href="https://www.shinyapps.io/", target="_blank")),
        tags$p("Built largely with ",
               tags$a("ggtree", href="https://bioconductor.org/packages/ggtree/", target="_blank")),
        tags$p("Taxonomy modified from ", 
               tags$a("Open Tree of Life", href="https://tree.opentreeoflife.org/about/open-tree-of-life", target="_blank")),
        tags$p("Thanks to Wikipedia for most of the pictures")
      ),
      
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
