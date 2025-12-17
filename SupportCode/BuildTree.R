
library(TreeTools) # to add tip
library(ape)
library(rotl)
library(tidytree)
library(tidyverse)
library(ggtree) # for the tree

# install.packages("BiocManager")
# BiocManager::install("ggtree") # need these for ggtree 

Descriptions <- read.csv("AnimalTreeApp/InvertDescriptions.csv") # call in the groups we cant

ranks <- c("Superphylum", "Phylum", "Subphylum", "Class", "Subclass")
taxa <- tnrs_match_names(
  Descriptions$Clade[Descriptions$Level %in% ranks],
  context_name = "Animals") # to stop a weird error

# Drop pruned IDs before calling tol_induced_subtree
taxa_clean <- taxa[!grepl("barren", taxa$flags) & !taxa$approximate_match, ]

# Build tree only with valid IDs
tree <- tryCatch(
  tol_induced_subtree(ott_ids = taxa_clean$ott_id),
  error = function(e) {
    message("Subtree request failed: ", e$message)
    return(NULL)  # or handle gracefully
  }
)

# tree <- tol_induced_subtree(ott_ids = taxa$ott_id) # Build Tree

# write.nexus(as.phylo(tree), "SupportCode/RawTree.nex")

tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T) # remove ids from tip labels 

# rename things 
tree$node.label[tree$node.label == "mrcaott42ott49"] <- "Nephrozoa " # space after cause its a node
tree$node.label[tree$node.label == "mrcaott42ott658"] <- "Chordata "
tree$node.label[tree$node.label == "mrcaott56ott519"] <- "Lophotrochozoa "
tree$node.label[tree$node.label == "mrcaott431ott3524"] <- "Medusozoa "
tree$node.label[tree$node.label == "mrcaott49ott6612"] <- "Spiralia "
tree$node.label[tree$node.label == "mrcaott56ott1881"] <- "Mollusca "
tree$node.label[tree$node.label == "mrcaott42ott570365"] <- "Eumetazoa "
tree$node.label[tree$node.label == "Lophotrochozoa ott155737"] <- NA # named clades we dont want
tree$node.label[tree$node.label == "Pancrustacea ott985906"] <- NA
tree$node.label[tree$node.label == "Panarthropoda ott816442"] <- NA
tree$node.label[tree$node.label == "Ambulacraria ott6520512"] <- NA
tree$node.label[tree$node.label == "Mandibulata ott985907"] <- NA
tree$tip.label[tree$tip.label == "Ctenophora (phylum ncbi:10197)"] <- "Ctenophora" # no space for tips
tree$tip.label[tree$tip.label == "Vertebrata (subphylum in Deuterostomia)"] <- "Vertebrata"
tree$tip.label[tree$tip.label == "mrcaott150ott7012"] <- "Hexacorallia"
tree$tip.label[tree$tip.label == "Onychophora (phylum in Holozoa)"] <- "Onychophora"
tree$tip.label[tree$tip.label == "Appendicularia (class in Opisthokonta)"] <- "Larvacea"
# tree$tip.label[tree$tip.label == "Polychaeta (class in Lophotrochozoa)"] <- "Polychaeta"
# tree$tip.label[tree$tip.label == "Oligochaeta (subclass in Opisthokonta)"] <- "Oligochaeta"


#remove useless node labels
tree$node.label <- ifelse(grepl(" ", tree$node.label), str_extract(tree$node.label, "^[^ ]+"), "")
tree$node.label[tree$node.label == ""] <- NA 

# If the tree has no branch lengths, create them, this is just so we can collapse polytomies
if (is.null(tree$edge.length)) {
  tree$edge.length <- rep(1, nrow(tree$edge))  # give all edges length 1
}

pairs_to_collapse <- list(
  c("Ctenophora", "Calcarea"), # collapse spiralians 
  c("Platyhelminthes", "Rotifera"), # collapse spiralians 
  c("Chaetognatha", "Rotifera"), # collapse spiralians 
  c("Bivalvia", "Gastropoda"), # collapse mollusks
  c("Monoplacophora", "Gastropoda"), # collapse mollusks
  c("Polyplacophora", "Bivalvia"), # collapse mollusks
  c("Hexactinellida", "Calcarea"), # collapse sponges
  c("Vertebrata", "Cephalochordata"), # collapse chordates  
  c("Crinoidea", "Asteroidea"), # collapse echinoderms 
  c("Echinoidea", "Asteroidea") # collapse echinoderms 
)

# to collapse unwanted nodes 
for (pair in pairs_to_collapse) {
  node <- getMRCA(tree, pair)
  if (!is.na(node)) {
    edges_to_zero <- which(tree$edge[,1] == node)
    tree$edge.length[edges_to_zero] <- 0
  }
}

# to add a tip that was problematic 
tree <- AddTip(tree, where = getMRCA(tree, c("Hemichordata", "Bivalvia")),
               label = "Xenacoelomorpha",
               edgeLength = 1) # node label isnt working so see following code

# Assign the node label
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Xenacoelomorpha"), 1] - length(tree$tip.label)] <- "Bilateria"

# to add another tip that was problematic 
tree <- AddTip(tree, where = "Hirudinea",
               label = "Polychaeta",
               edgeLength = 1)

# Assign the node label
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Polychaeta"), 1] - length(tree$tip.label)] <- "Annelida"

# to add another tip that was problematic 
tree <- AddTip(tree, where = "Hirudinea",
               label = "Oligochaeta",
               edgeLength = 1)

# Assign the node label
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Oligochaeta"), 1] - length(tree$tip.label)] <- "Clitella"

tree <- di2multi(tree) # only works once some edges have 0 length 
write.nexus(tree, file = "AnimalTreeApp/tree.nex")

colnames(Descriptions)[colnames(Descriptions) == "Clade"] = "label" # i think this is necessary to match in descriptions

# reorder for the sake of the legend 
Descriptions$Level <- factor(Descriptions$Level, levels = c("Higher Clade", "Superphylum", "Phylum", "Subphylum", "Class", "Subclass"))

TreePlot <- ggtree(tree, branch.length="none", aes(color=Level), size = 1.5) %<+% Descriptions + # match descriptions to nodes
  geom_tiplab(fill="white", geom = "label", size = 5, fontface = 2) +
  geom_nodelab(subset = !is.na(node.label), 
               fill="white", geom = "label", size = 5, fontface = 2) +
  theme(legend.position = c(0.1,0.9),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)) # incase i decide to add a background

save(TreePlot, file = "AnimalTreeApp/ggTreeObject") # save it so the app can call it

