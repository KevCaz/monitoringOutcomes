############
#Intra and inter community connectivity for each node
##############
#Main coding Kevin Cazelles, additional Helen Wheeler

######### Packages
library(igraph)
# devtools::install_github("KevCaz/graphicsutils")
library(graphicsutils)
######
#Must be run after walktrapdendro4 due to dependencies
#####


setwd("~/Codes/Github/Figures/Figures_Wheeler2017")

nodes <- readRDS("nodes.Rds") 
links <- readRDS("links2.Rds")

######
# inter and intra links 

nodes$inter <- nodes$intra <- 0
for (i in 1:nrow(links)){
  idf <- which(nodes$id == links[i,1L])  #returns row no of node in nodes obj (from links)
  idt <- which(nodes$id == links[i,2L])  #same to link
  if (nodes$idgrp[idf] == nodes$idgrp[idt]){
    nodes$intra[idf] <- nodes$intra[idf]+links$weight[i]
    nodes$intra[idt] <- nodes$intra[idt]+links$weight[i] #need to add other direction
  } else {
    nodes$inter[idf] <- nodes$inter[idf]+links$weight[i]
    nodes$inter[idt] <- nodes$inter[idt]+links$weight[i]
  }
}

#Graphic of intra and inter community links by node
par(mar=c(4,4,2,2), las=1)
plot(nodes$intra, nodes$inter, pch=21, col=pal2, cex=3, bg=pal, xlab="cummulative link weight - intra group", ylab = "cummulative links weight - inter group")
abline(v = mean(nodes$intra), h = mean(nodes$inter), lty=2, col="grey50")
text(nodes$intra, nodes$inter,, labels=nodes$idnum)