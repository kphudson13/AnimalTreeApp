
# Just a script to check which taxa have been cut out 

library(tidyverse)
library(ape)

taxainfo <- read.csv("AnimalTreeApp/InvertDescriptions.csv")
tree <- read.nexus("AnimalTreeApp/tree.nex", tree.names = "tree")

datalabels <- sort(taxainfo$Clade)
treelabels <- sort(c(tree$tip.label, tree$node.label))

setdiff(datalabels, treelabels) # check for taxa in table but not tree
setdiff(treelabels, datalabels) # check for taxa in tree but not table
