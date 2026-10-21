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
library(ggtree)
library(bslib) # for the web app theme 
library(dplyr)
library(ggiraph)

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
      margin-bottom: 40px; /* reserve space equal to footer height */
    }
    
    .ggiraph-toolbar,
    .btn-zoom { /* to remove plot buttons */
        display: none !important;
    }
    
    .app-footer p {
      margin: 0;
      line-height: 1.2;
      font-size: 0.9rem;
    }
    
                  "))
  ),
  
  tags$head( # block so enter button on keyboard works
    tags$script(HTML("
    $(document).on('keydown', '#taxon_search', function(e) {
      if (e.key === 'Enter') {
        e.preventDefault();
        $('#search_btn').click();
      }
    });
  "))
  ),
  
  theme = bs_theme(preset = "morph"),
  
  # Intro section always visible
  fluidRow(
    style = "display:flex;", # this just centers the title better 
    column(
      width = 3,  # text takes less of the row
      h2("Animal Tree of Life", style = "margin-top:30px;"),  # use h2 for a nice title size
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
        
        tags$div(
          style = "
    position:absolute;
    top:20px;
    left:20px;
    z-index:1000;
    display:flex;
    gap:8px;
    align-items:flex-start;
  ",
          
          # Search box
          tags$div(
            style = "
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
          
          # Dice box
          tags$div(
            style = "
      background: rgba(255,255,255,0.9);
      padding:10px;
      border-radius:8px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    ",
            actionButton("random_btn", "🎲")
          )
        ),
        
        girafeOutput(
          "TreePlot",
          height = "150%",
          width = "100%"
        )
      )
    )
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
)

server <- function(input, output, session) {
  
  make_tree <- reactive({
    xmax <- max(TreePlot$data$x, na.rm = TRUE)
    TreePlot +
      ggplot2::xlim(NA, xmax + 1) +
      ggplot2::theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8))
      )
  })
  
  output$TreePlot <- renderGirafe({
    girafe(
      ggobj = make_tree(),
      bg = "transparent",
      width_svg = 7,
      height_svg = 14,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_selection(
          type = "single",
          only_shiny = TRUE,
          css = "fill:white;stroke:black;"
        ),
        opts_tooltip(css = "background:rgba(255,255,255,0.8);"),
        opts_hover(css = "")
      )
    )
  })
  
  # Generates unique IDs for the navigation links created in each modal
  modal_nav_counter <- reactiveVal(0)
  
  # Find the tree node number associated with a displayed label
  label_to_node <- function(label) {
    tip_match <- which(tree$tip.label == label)
    
    node_match <- which(tree$node.label == label)
    node_match <- node_match + ape::Ntip(tree)
    
    c(tip_match, node_match)[1]
  }
  
  # Get the displayed label for a tree node number
  node_to_label <- function(node) {
    n_tips <- ape::Ntip(tree)
    
    if (node <= n_tips) {
      tree$tip.label[node]
    } else {
      tree$node.label[node - n_tips]
    }
  }
  
  # Return only the closest labeled parent clade
  get_higher_taxa <- function(label) {
    node <- label_to_node(label)
    if (is.na(node)) return(character(0))
    
    repeat {
      parent_row <- which(tree$edge[, 2] == node)
      
      # Reached the root of the tree
      if (length(parent_row) == 0) return(character(0))
      
      node <- tree$edge[parent_row[1], 1]
      parent_label <- node_to_label(node)
      
      # Return the first labeled ancestor only
      if (!is.na(parent_label) && parent_label != "") {
        return(parent_label)
      }
    }
  }
  
  # Return immediate labeled children only
  get_lower_taxa <- function(label) {
    node <- label_to_node(label)
    if (is.na(node)) return(character(0))
    
    child_nodes <- tree$edge[tree$edge[, 1] == node, 2]
    child_labels <- vapply(child_nodes, node_to_label, character(1))
    
    child_labels[!is.na(child_labels) & child_labels != ""]
  }
  
  show_taxon_modal <- function(label_clicked) {
    
    info <- taxa_info %>% filter(Clade == label_clicked)
    
    desc <- if (nrow(info) == 0) {
      paste0("Label: ", label_clicked)
    } else if (info$Anatomy == "") {
      paste0(info$CommonName, "\n", info$Description)
    } else {
      paste0(
        info$CommonName, "\n", info$Description,
        "\nAnatomy to know: ", info$Anatomy
      )
    }
    
    exts <- c(".jpeg", ".jpg", ".png") # Image extentions 
    
    file_path <- NULL
    for (ext in exts) {
      candidate <- file.path("www", "Photos", paste0(label_clicked, ext))
      if (file.exists(candidate)) {
        file_path <- paste0("Photos/", label_clicked, ext)
        break
      }
    }
    
    anat_path <- NULL
    for (n in 1:5) {
      for (ext in exts) {
        anat_candidate <- file.path(
          "www", "Photos",
          paste0(label_clicked, "_anatomy", n, ext)
        )
        
        if (file.exists(anat_candidate)) {
          anat_path <- c(
            anat_path,
            paste0("Photos/", label_clicked, "_anatomy", n, ext)
          )
        }
      }
    }
    
    # Find nearby taxa in the tree
    higher_taxa <- get_higher_taxa(label_clicked)
    lower_taxa <- get_lower_taxa(label_clicked)
    
    # Create unique action-link IDs for this particular modal
    modal_nav_counter(modal_nav_counter() + 1)
    modal_id <- modal_nav_counter()
    
    higher_ids <- paste0("higher_", modal_id, "_", seq_along(higher_taxa))
    lower_ids <- paste0("lower_", modal_id, "_", seq_along(lower_taxa))
    
    col_width <- if (length(anat_path) > 0) 6 else 12
    img_style <- if (length(anat_path) == 0) {
      "display:block; margin:auto; max-width:80%; height:auto;"
    } else {
      "max-width:100%; height:auto;"
    }
    
    showModal(modalDialog(
      title = div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        span(
          if (nrow(info) == 0) {
            "No description available"
          } else {
            paste0(info$Level, ": ", info$Clade)
          }
        ),
        tags$button(
          type = "button",
          class = "btn-close",
          `data-bs-dismiss` = "modal"
        )
      ),
      
      fluidRow(
        tags$div(
          style = "
    position:relative;
    width:100%;
  ",
          
          # Closest higher clade: outside the modal on the left
          if (length(higher_taxa) > 0) {
            tags$div(
              style = "
        position:absolute;
        right:calc(100% + 18px);
        top:50%;
        transform:translateY(-50%);
        width:150px;
        z-index:2000;
      ",
              lapply(seq_along(higher_taxa), function(i) {
                tags$div(
                  style = "
            background:rgba(255,255,255,0.97);
            border:1px solid #ccc;
            border-radius:10px;
            padding:12px;
            text-align:center;
            box-shadow:0 3px 10px rgba(0,0,0,0.22);
          ",
                  actionLink(
                    higher_ids[i],
                    higher_taxa[i],
                    style = "
              font-weight:bold;
              text-decoration:none;
              display:block;
            "
                  )
                )
              })
            )
          },
          
          # Central modal content: description, pictures, anatomy
          tags$div(
            tags$pre(style = "white-space:pre-wrap;", desc),
            
            fluidRow(
              column(
                width = col_width,
                if (!is.null(file_path)) {
                  tags$img(src = file_path, style = img_style)
                },
                if (nrow(info) > 0 && !is.na(info$ImageLink)) {
                  tags$p(info$ImageLink, style = "font-size:0.8rem;")
                }
              ),
              
              if (length(anat_path) > 0) {
                column(
                  width = 6,
                  lapply(anat_path, function(path) {
                    tags$img(
                      src = path,
                      style = "width:100%; height:auto; margin-bottom:10px;"
                    )
                  })
                )
              }
            )
          ),
          
          # Lower clades: outside the modal on the right
          if (length(lower_taxa) > 0) {
            tags$div(
              style = "
        position:absolute;
        left:calc(100% + 18px);
        top:50%;
        transform:translateY(-50%);
        width:150px;
        z-index:2000;
        display:flex;
        flex-direction:column;
        gap:10px;
      ",
              lapply(seq_along(lower_taxa), function(i) {
                tags$div(
                  style = "
            background:rgba(255,255,255,0.97);
            border:1px solid #ccc;
            border-radius:10px;
            padding:12px;
            text-align:center;
            box-shadow:0 3px 10px rgba(0,0,0,0.22);
          ",
                  actionLink(
                    lower_ids[i],
                    lower_taxa[i],
                    style = "
              font-weight:bold;
              text-decoration:none;
              display:block;
            "
                  )
                )
              })
            )
          }
        )
      ),
      
      size = "l",
      easyClose = TRUE,
      footer = NULL
    ))
    
    # Clicking a higher-taxon link opens that taxon's modal
    lapply(seq_along(higher_taxa), function(i) {
      observeEvent(input[[higher_ids[i]]], {
        removeModal() # This stops a scrolling bug
        show_taxon_modal(higher_taxa[i])
      }, once = TRUE, ignoreInit = TRUE)
    })
    
    # Clicking a lower-taxon link opens that taxon's modal
    lapply(seq_along(lower_taxa), function(i) {
      observeEvent(input[[lower_ids[i]]], {
        removeModal() # This stops a scrolling bug
        show_taxon_modal(lower_taxa[i])
      }, once = TRUE, ignoreInit = TRUE)
    })
  }
  
  
  observeEvent(input$TreePlot_selected, {
    label_clicked <- input$TreePlot_selected
    
    if (is.null(label_clicked) ||
        length(label_clicked) == 0 ||
        !nzchar(label_clicked)) {
      return()
    }
    
    show_taxon_modal(label_clicked)
    
    # Clear the selected label so it does not remain highlighted
    session$sendCustomMessage(
      type = "TreePlot_set",
      message = character(0)
    )
  }, 
  ignoreInit = TRUE)
  
  # Reactive storage for suggested search values
  suggestion_obs <- reactiveVal(list())
  suggestion_map <- reactiveVal(list())
  
  # For search bar
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
  
  # For random group button
  observeEvent(input$random_btn, {
    labels_all <- unique(trimws(c(tree$tip.label, tree$node.label)))
    labels_all <- labels_all[!is.na(labels_all) & labels_all != "" & 
                               labels_all %in% taxa_info$Clade]
    show_taxon_modal(sample(labels_all, 1))
  })
  
  # For credits modal
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