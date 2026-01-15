###########################
# 
# developed Nov 2025-Jan 2026 by Kyle Hudson
# kphudson@live.ca
# This app is designed as a teaching tool, the taxonomy is incomplete
# 
# Live laugh love
# -Kyle 
# 
###########################

library(ape)
library(shiny)
library(ggplot2)
library(plotly)
library(ggtree)
library(bslib) # for the web app theme 
library(dplyr)

load("ggTreeObject") # built from other script 
taxa_info <- read.csv("InvertDescriptions.csv")
tree <- read.nexus("tree.nex", tree.names = "tree")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('Photos/Background.png'); /* path inside www/Photos */
        background-size: cover;       /* scale to fill screen */
        background-repeat: no-repeat; /* don't tile */
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
        alt = "Metazoa overview image" # in case loading fails
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
      
      # wrapper so absolute-position elements stay anchored to tree panel
      tags$div(
        style = "position:relative;",
        
        # Floating search box (NEW)
        tags$div(
          style = "
    position:absolute;
    top:20px;
    left:20px;
    z-index:1000;
    background: rgba(255,255,255,0.9);
    padding:10px;
    border-radius:8px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    width:220px;
  ",
          textInput(
            "taxon_search",
            label = NULL,
            placeholder = "Search taxon…"
          ),
          actionButton(
            "search_btn",
            "Search",
            width = "100%"
          )
        ),
        
        plotOutput(
          "TreePlot", 
          height = paste0(30 * length(tree$tip.label), "px"), # height is dynamic, based off number of tips
          click = "plot_click"
        )
      )
    )
  ), 
  
  # Footer navbar
  tags$head(
    tags$style(HTML("
    body { margin-bottom: 40px; } /* reserve space equal to footer height */
    /* Optional: make the main content scroll above the footer without overlap */
    .container-fluid, .main-panel {
      padding-bottom: 40px;      /* matches body margin-bottom */
    }
    
    .app-footer p {
      margin: 0;
      line-height: 1.2;
      font-size: 0.9rem;
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
                tags$p(actionLink("credits", "Credits")))
        ))
  ), 
  
  tags$head( # this is to press enter with the search bar
    tags$script(HTML("
    $(document).on('keyup', '#taxon_search', function(e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      setTimeout(function() { $('#search_btn').click(); }, 0);
    }
  });
  "))
  )
)

server <- function(input, output, session) {
  
  make_tree <- reactive({
    xmax <- max(TreePlot$data$x, na.rm = TRUE)
    TreePlot + ggplot2::xlim(NA, xmax + 1) # add buffer
  })
  
  output$TreePlot <- renderPlot({
    make_tree()
  }, 
  bg = "transparent")
  
  # Centralized modal logic 
  show_taxon_modal <- function(label_clicked) {
    
    info <- taxa_info %>% filter(Clade == label_clicked)
    
    desc <- if (nrow(info) == 0) { # descriptive text from csv
      paste0("Label: ", label_clicked)
    } else if (info$Anatomy == "") {
      paste0(info$CommonName, "\n", info$Description)
    } else {
      paste0(info$CommonName, "\n", info$Description,
             "\nAnatomy to know: ", info$Anatomy)
    }
    
    exts <- c(".jpeg", ".jpg", ".png") # Image extensions
    
    file_path <- NULL # reset the parking lot for image
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext))
      if (file.exists(candidate)) {
        file_path <- paste0("Photos/", label_clicked, ext)
      }
    }
    
    anat_path <- NULL # reset the parking lot for anatomy image
    for (n in 1:5) {
      for (ext in exts) {
        anat_candidate <- file.path("www", "Photos",
                                    paste0(label_clicked, "_anatomy", n, ext))
        if (file.exists(anat_candidate)) {
          anat_path <- c(anat_path,
                         paste0("Photos/", label_clicked, "_anatomy", n, ext))
        }
      }
    }
    
    col_width <- if (length(anat_path) > 0) 6 else 12 # split for anatomy pics
    img_style <- if (length(anat_path) == 0)
      "display:block; margin:auto; max-width:80%; height:auto;"
    else
      "max-width:100%; height:auto;"
    
    showModal(modalDialog( # main modal pop up 
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        span(if (nrow(info) == 0)
          "No description available"
          else
            paste0(info$Level, ": ", info$Clade)
        ),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "modal"
        )
      ),
      
      tagList(
        tags$pre(style = "white-space: pre-wrap;", desc),
        fluidRow(
          column(
            width = col_width,
            if (!is.null(file_path))
              tags$img(src = file_path, style = img_style),
            if (nrow(info) > 0 && !is.na(info$ImageLink))
              tags$p(info$ImageLink, style = "font-size:0.8rem;")
          ),
          if (length(anat_path) > 0)
            column(
              width = 6,
              lapply(anat_path, function(path) {
                tags$img(src = path,
                         style = "width:100%; height:auto; margin-bottom:10px;")
              })
            )
        )
      ),
      
      size = "l",
      easyClose = TRUE, # so you can click outside to close 
      footer = NULL # removes extra dismiss button
    ))
  }
  
  observeEvent(input$plot_click, { # main panel click 
    
    tree_data <- TreePlot$data
    nearest <- nearPoints(tree_data, input$plot_click,
                          xvar = "x", yvar = "y",
                          threshold = 40, maxpoints = 1)
    if (nrow(nearest) == 0) return() # stops app from crashing if you click randomly 
    
    show_taxon_modal(nearest$label)
  })
  
  suggestion_obs <- reactiveVal(list())
  suggestion_map <- reactiveVal(list())
  
  observeEvent(input$search_btn, {
    req(input$taxon_search)
    query <- trimws(input$taxon_search)
    if (query == "") return()
    
    # include both tip and node labels
    labels_all <- unique(trimws(c(tree$tip.label, tree$node.label)))
    labels_all <- labels_all[!is.na(labels_all) & labels_all != ""]
    labels_lower <- tolower(labels_all)
    query_lower  <- tolower(query)
    
    exact_match <- labels_all[labels_lower == query_lower]
    if (length(exact_match) == 1) {
      show_taxon_modal(exact_match)
      updateTextInput(session, "taxon_search", value = "")
      return()
    }
    
    partial_matches <- labels_all[grepl(query_lower, labels_lower, fixed = TRUE)]
    if (length(partial_matches) == 0) {
      dists <- adist(query_lower, labels_lower)
      closest <- order(dists)[1:min(5, length(dists))]
      suggestions <- labels_all[closest]
    } else {
      suggestions <- head(partial_matches, 5)
    }
    
    # destroy any old suggestion observers
    lapply(suggestion_obs(), function(obs) obs$destroy())
    suggestion_obs(list())
    
    # sanitize IDs and map back to labels
    make_id <- function(s) {
      id <- paste0("suggest_", gsub("[^A-Za-z0-9_]", "_", s))
      make.unique(id)
    }
    ids <- vapply(suggestions, make_id, FUN.VALUE = character(1))
    suggestion_map(setNames(as.list(suggestions), ids))
    
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        span("No exact match found"),
        tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal")
      ),
      tagList(
        tags$p(paste0("No taxon exactly matching \"", query, "\" was found.")),
        if (length(suggestions) > 0) {
          tagList(
            tags$p("Did you mean:"),
            tags$ul(lapply(ids, function(id) {
              tags$li(actionLink(inputId = id, label = suggestion_map()[[id]]))
            }))
          )
        } else {
          tags$p("No similar taxa found.")
        }
      ),
      easyClose = TRUE, 
      footer = NULL
    ))
    
    # register observers for sanitized IDs
    suggestion_obs(lapply(names(suggestion_map()), function(id) {
      observeEvent(input[[id]], {
        removeModal()
        show_taxon_modal(suggestion_map()[[id]])
      }, once = TRUE, 
      ignoreInit = TRUE)
    }))
    
    updateTextInput(session, "taxon_search", value = "")
  })
  
  observeEvent(input$credits, { # credit link click
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center; width:100%;",
        span("Credits"),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "modal"
        )
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