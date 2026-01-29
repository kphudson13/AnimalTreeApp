###########################
# 
# developed Nov 2025-Jan 2026 by Kyle Hudson
# kphudson@live.ca
# This code is to build the tree for the app. You do not need to run it locally 
# to run the app, but you do need the other files
# 
# Live laugh love
# -Kyle 
# 
###########################

library(TreeTools) # to add tip
library(ape)
library(rotl)
library(tidytree)
library(tidyverse)
library(ggtree) # for the tree

# Don't run this part anymore ---------------------------------------------

# install.packages("BiocManager")
# BiocManager::install("ggtree") # need these for ggtree

Descriptions <- read.csv("AnimalTreeApp/InvertDescriptions.csv") # call in the groups we cant

ranks <- c("Superphylum", "Phylum", "Subphylum", "Class", "Subclass", "Order", "Group")
taxa <- tnrs_match_names(
  Descriptions$Clade[Descriptions$Level %in% ranks],
  context_name = "Animals") # to stop a weird error

# Drop pruned IDs before calling tol_induced_subtree
taxa_clean <- taxa[!grepl("barren", taxa$flags) & !grepl("extinct", taxa$flags) & !taxa$approximate_match, ]

# Build tree only with valid IDs
tree <- tryCatch(
  tol_induced_subtree(ott_ids = taxa_clean$ott_id),
  error = function(e) {
    message("Subtree request failed: ", e$message)
    return(NULL)  # or handle gracefully
  }
)

tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T) # remove ids from tip labels

# rename things
tree$node.label[tree$node.label == "mrcaott42ott49"] <- "Nephrozoa " # space after cause its a node
tree$node.label[tree$node.label == "mrcaott42ott658"] <- "Chordata "
tree$node.label[tree$node.label == "mrcaott56ott519"] <- "Lophotrochozoa "
tree$node.label[tree$node.label == "mrcaott431ott3524"] <- "Medusozoa "
tree$node.label[tree$node.label == "mrcaott49ott6612"] <- "Spiralia "
tree$node.label[tree$node.label == "mrcaott56ott1881"] <- "Mollusca "
tree$node.label[tree$node.label == "mrcaott42ott570365"] <- "Eumetazoa "
tree$node.label[tree$node.label == "mrcaott343ott948"] <- "Arachnida "
tree$node.label[tree$node.label == "mrcaott348ott37291"] <- "Malacostraca "
tree$node.label[tree$node.label == "mrcaott150ott7012"] <- "Hexacorallia "
tree$node.label[tree$node.label == "Pterygota (subclass in Opisthokonta) ott1048707"] <- "Insecta "
tree$node.label[tree$node.label == "Pleocyemata ott736321"] <- "Decapoda "
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
tree$tip.label[tree$tip.label == "Brachyura (infraorder in Protostomia)"] <- "Brachyura"
tree$tip.label[tree$tip.label == "Antipatharia (order worms:22549)"] <- "Antipatharia"
tree$tip.label[tree$tip.label == "mrcaott4101ott21309"] <- "Rotifera"

# remove useless node labels
tree$node.label <- ifelse(grepl(" ", tree$node.label), str_extract(tree$node.label, "^[^ ]+"), "")
tree$node.label[tree$node.label == ""] <- NA

# # If the tree has no branch lengths, create them, this is just so we can collapse polytomies
# if (is.null(tree$edge.length)) {
#   tree$edge.length <- rep(1, nrow(tree$edge))  # give all edges length 1
# }

# Add problematic tips ----------------------------------------------------

tree <- AddTip(tree, where = getMRCA(tree, c("Enteropneusta", "Bivalvia")), label = "Xenacoelomorpha")
# Assign the node label cause nodelabel isnt working
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Xenacoelomorpha"), 1] - length(tree$tip.label)] <- "Bilateria"

tree <- AddTip(tree, where = getMRCA(tree, c("Trematoda", "Cestoda")), label = "Turbellia")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Turbellia"), 1] - length(tree$tip.label)] <- "Platyhelminthes"

tree <- AddTip(tree, where = "Hirudinea", label = "Polychaeta")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Polychaeta"), 1] - length(tree$tip.label)] <- "Annelida"

tree <- AddTip(tree, where = "Hirudinea", label = "Oligochaeta")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Oligochaeta"), 1] - length(tree$tip.label)] <- "Clitella"

# this one needs to be moved
tree <- drop.tip(tree, "Pycnogonida")
tree <- AddTip(tree, where = getMRCA(tree, c("Merostomata", "Araneae")), label = "Pycnogonida")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Pycnogonida"), 1] - length(tree$tip.label)] <- "Chelicerata"

tree <- AddTip(tree, where = getMRCA(tree, c("Merostomata", "Araneae")), label = "Trilobita")

tree <- drop.tip(tree, "Copepoda")
tree <- AddTip(tree, where = getMRCA(tree, c("Ostracoda", "Cirripedia")), label = "Copepoda")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Copepoda"), 1] - length(tree$tip.label)] <- "Crustacea"

tree <- drop.tip(tree, "Branchiopoda")
tree <- AddTip(tree, where = getMRCA(tree, c("Ostracoda", "Cirripedia")), label = "Branchiopoda")

tree <- AddTip(tree, where = "Cubozoa", label = "Staurozoa")

tree <- AddTip(tree, where = getMRCA(tree, c("Pennatulacea", "Helioporacea")), label = "Alcyonacea")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Alcyonacea"), 1] - length(tree$tip.label)] <- "Octocorallia"

tree <- AddTip(tree, where = "Nuda", label = "Tenticulata")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Tenticulata"), 1] - length(tree$tip.label)] <- "Ctenophora"

tree <- AddTip(tree, where = "Larvacea", label = "Ascidiacea")

tree <- AddTip(tree, where = "Salpida", label = "Pyrosomida")
tree$node.label[tree$edge[tree$edge[,2] == which(tree$tip.label == "Pyrosomida"), 1] - length(tree$tip.label)] <- "Thaliacea"

tree <- drop.tip(tree, "Clitella") # this shows up in tunicates for some reason

tree <- AddTip(tree, where = "Solifugae", label = "Acari")

# this one needs to be moved
tree <- drop.tip(tree, "Bryozoa")
tree <- AddTip(tree, where = "Brachiopoda", label = "Bryozoa")

# Collapse nodes ----------------------------------------------------------

# You could do this in all one big list, as I once did
# but trouble shooting becomes a pain in the ass

x_crustacea <- list(
  c("Ostracoda", "Branchiopoda"),         # collapse crustacea
  c("Ostracoda", "Pentastomida"),         # collapse crustacea
  c("Ostracoda", "Cirripedia"),           # collapse crustacea
  c("Stomatopoda", "Cirripedia"),         # collapse crustacea
  c("Isopoda", "Amphipoda"),              # collapse malacostraca
  c("Isopoda", "Euphausiacea"),           # collapse malacostraca
  c("Caridea", "Euphausiacea"),           # collapse malacostraca
  c("Achelata", "Astacidea"),             # collapse decapoda
  c("Brachyura", "Astacidea"),            # collapse decapoda
  c("Brachyura", "Anomura")               # collapse decapoda
)

for (pair in x_crustacea) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_cnidaria <- list(
  c("Cubozoa", "Staurozoa"),              # collapse medusozoa
  c("Scyphozoa", "Cubozoa"),              # collapse medusozoa
  c("Helioporacea", "Pennatulacea"),      # collapse octocorallia
  c("Zoantharia", "Antipatharia"),        # collapse hexacorralia
  c("Corallimorpharia", "Scleractinia"),  # collapse hexacorralia
  c("Zoantharia", "Corallimorpharia"),    # collapse hexacorralia
  c("Ceriantharia", "Actiniaria")         # collapse anothozoa
)

for (pair in x_cnidaria) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_hexapoda <- list(
  c("Diptera", "Lepidoptera"),            # collapse insecta
  c("Coleoptera", "Lepidoptera"),         # collapse insecta
  c("Coleoptera", "Hymenoptera"),         # collapse insecta
  c("Hemiptera", "Hymenoptera"),          # collapse insecta
  c("Hemiptera", "Blattodea"),            # collapse insecta
  c("Blattodea", "Orthoptera")            # collapse insecta
)

for (pair in x_hexapoda) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_chelicerata <- list(
  c("Merostomata", "Opiliones"),          # collapse chelicerata
  c("Trilobita", "Opiliones"),            # collapse chelicerata
  c("Solifugae", "Opiliones"),            # collapse arachnida
  c("Araneae", "Amblypygi"),              # collapse arachnida
  c("Araneae", "Scorpiones"),             # collapse arachnida
  c("Pseudoscorpiones", "Scorpiones"),    # collapse arachnida
  c("Solifugae", "Acari"),
  c("Uropygi", "Amblypygi")               # collapse arachnida
)

for (pair in x_chelicerata) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_lophotrochozoa <- list(
  c("Nemertea", "Brachiopoda"),           # collapse lophotrochozoa
  c("Nemertea", "Bivalvia"),              # collapse lophotrochozoa
  c("Brachiopoda", "Bryozoa"),
  c("Monoplacophora", "Bivalvia"),        # collapse mollusca
  c("Monoplacophora", "Coleoidea"),       # collapse mollusca
  c("Bivalvia", "Gastropoda")             # collapse mollusca
)

for (pair in x_lophotrochozoa) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_chordata <- list(
  c("Vertebrata", "Larvacea"),            # collapse chordata
  c("Enteropneusta", "Crinoidea"),        # collapse deuterostomes
  c("Echinoidea", "Holothuroidea"),       # collapse echinoderms
  c("Asteroidea", "Ophiuroidea"),         # collapse echinoderms
  c("Asteroidea", "Echinoidea")           # collapse echinoderms
)

for (pair in x_chordata) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_ecdysozoa <- list(
  c("Priapulida", "Nematoda"),            # collapse ecdysozoa
  c("Tardigrada", "Onychophora"),         # collapse ecdysozoa
  c("Tardigrada", "Trilobita"),           # collapse ecdysozoa
  c("Blattodea", "Anomura"),              # collapse arthropods
  c("Blattodea", "Diplopoda")             # collapse arthropods
)

for (pair in x_ecdysozoa) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_spiralia <- list(
  c("Cestoda", "Trematoda"),              # collapse platyhelminthes
  c("Rotifera", "Trematoda"),             # collapse spiralia
  c("Rotifera", "Polychaeta")             # collapse spiralia
)

for (pair in x_spiralia) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

x_misc <- list(
  c("Demospongiae", "Hexactinellida"),    # collapse porifera
  c("Ascidiacea", "Larvacea"),             # collapse tunicata
  c("Calcarea", "Hydrozoa"),              # collapse basal taxa
  c("Zoantharia", "Anomura")              # collapse eumetazoa
)

for (pair in x_misc) {
  tree <- CollapseNode(tree, getMRCA(tree, pair))
}

write.nexus(tree, file = "AnimalTreeApp/tree.nex")

# Plot and save tree ------------------------------------------------------

colnames(Descriptions)[colnames(Descriptions) == "Clade"] = "label" # i think this is necessary to match in descriptions

# reorder for the sake of the legend 
Descriptions$Level <- factor(Descriptions$Level, levels = c("Higher Clade", "Superphylum", "Phylum", "Subphylum", "Class", "Subclass", "Order", "Group"))

TreePlot <- ggtree(tree, branch.length="none", layout="ellipse", aes(color=Level), size = 1.5) %<+% Descriptions + # match descriptions to nodes
  geom_tiplab(fill="white", geom = "label", size = 5, fontface = 2) +
  geom_nodelab(subset = !is.na(node.label), 
               fill="white", geom = "label", size = 5, fontface = 2) +
  theme(legend.position = c(0.1,0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=16),
        legend.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background  = element_rect(fill = NA, colour = NA)) # incase i decide to add a background

save(TreePlot, file = "AnimalTreeApp/ggTreeObject") # save it so the app can call it

