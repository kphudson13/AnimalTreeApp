
library(ape)
library(rotl)
library(tidytree)
library(tidyverse)
library(ggtree)


# install.packages("BiocManager")
# BiocManager::install("ggtree") # need these for ggtree 

Descriptions <- read.csv("AnimalTreeApp/InvertDescriptions.csv") # call in the groups we cant

taxa <- tnrs_match_names(Descriptions$Clade[Descriptions$Level == "Phylum" | 
                                              Descriptions$Level == "Subphylum" | 
                                              Descriptions$Level == "Class" ], 
                         context_name = "Animals") # Pull taxa

tree <- tol_induced_subtree(ott_ids = taxa$ott_id) # Build Tree

# write.nexus(as.phylo(tree), "SupportCode/RawTree.nex")

tree$tip.label <- strip_ott_ids(tree$tip.label, remove_underscores = T) # remove ids from tip labels 

# rename things 
tree$node.label[tree$node.label == "mrcaott42ott49"] <- "Nephrozoa "
tree$node.label[tree$node.label == "Lophotrochozoa ott155737"] <- "Spiralia "
tree$node.label[tree$node.label == "mrcaott56ott519"] <- "Lophotrochoza "
tree$tip.label[tree$tip.label == "Ctenophora (phylum ncbi:10197)"] <- "Ctenophora"

#remove useless node labels
tree$node.label <- ifelse(grepl(" ", tree$node.label), str_extract(tree$node.label, "^[^ ]+"), "")

colnames(Descriptions)[colnames(Descriptions) == "Clade"] = "label" # i think this is necessary 

# tree$tip.label

TreePlot <- ggtree(tree, branch.length="none", aes(color=Level)) %<+% Descriptions + # match descriptions to nodes
  geom_tiplab(fill="white", geom = "label") +
  geom_nodelab(fill="white", geom = "label") +
  theme(legend.position = c(0.1,0.8))

save(TreePlot, file = "AnimalTreeApp/ggTreeObject") # save it so the app can call it

