
library(ape)
library(rotl)
library(tidytree)
library(tidyverse)
library(ggtree)

# install.packages("BiocManager")
# BiocManager::install("ggtree")


Descriptions <- read.csv("AnimalTreeApp/InvertDescriptions.csv")

taxa <- tnrs_match_names(Descriptions$Clade[Descriptions$Level == "Phylum" | 
                                              Descriptions$Level == "Subphylum" | 
                                              Descriptions$Level == "Class"], 
                         context_name = "Animals")

tree <- tol_induced_subtree(ott_ids = taxa$ott_id)
tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T)

tree$node.label[tree$node.label == "mrcaott42ott49"] <- "Nephrozoa "
tree$node.label[tree$node.label == "Lophotrochozoa ott155737"] <- "Spiralia "
tree$node.label[tree$node.label == "mrcaott56ott519"] <- "Lophotrochoza "

#remove useless node labels
tree$node.label <- ifelse(grepl(" ", tree$node.label),
                          str_extract(tree$node.label, "^[^ ]+"),
                          "")

TreePlot <- ggtree(tree, branch.length="none") +
  geom_tiplab(color="darkblue") +
  geom_nodelab(fill = "green", geom = "label")

save(TreePlot, file = "AnimalTreeApp/ggTreeObject")

