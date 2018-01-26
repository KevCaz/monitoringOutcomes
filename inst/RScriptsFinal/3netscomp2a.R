#################
#Plotting of network of desired impacts from monitoring -comparison of three algorithms
#################
#Coding by Kevin Cazelles and Helen Wheeler

######### Packages
library(igraph)
library(plotrix) # draw.circle()
library(ape)
 devtools::install_github("KevCaz/graphicsutils")
library(graphicsutils)
library(igraph)


# source("R/walktrap_community2.R")

######### Import data
nodes <- read.csv("datafinal/Nodes_names.csv", header=T, as.is=T)
links <- read.csv("datafinal/Relationships_names.csv", header=T, as.is=T)
#links <- readRDS("links.Rds")


nodes$idnum <- as.numeric(gsub(nodes$id, pat="\\D", rep=""))

##################################
net <- graph.data.frame(links, nodes, directed=F)
gcon <- simplify(net, edge.attr.comb = list(weight = "sum", function(x)length(x)))

# walktrap community detection using weights
clus5 <- cluster_walktrap(gcon, weights=E(gcon)$weight)
nodes$idgrp <- membership(clus5)

coords <- layout.fruchterman.reingold(gcon)
# saveRDS(coords, "data/coords.Rds")
#coords <- readRDS("data/coords.Rds")
#########
#png("fignetden.png", units="in", res=300, width=8, heigh=6)
#layout(matrix(c(1,3),1), width = c(.85,1))
par(mar=c(1,1,1,1))
par(mfrow=c(1,3))
#########

##
pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
pal3 <- c("#d72968", "#a18037", "#5523b4", "#063632",1)
##
##
plot0(range(coords[,1])+.5*c(-1,1), range(coords[,2])+.5*c(-1,1), asp=1)
for (i in 1:nrow(links)) {
  idf <- which(nodes$id == links$from[i])
  idt <- which(nodes$id == links$to[i])
  #
  lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = .75*links$orig.wt[i])
}

for (i in 1:length(clus5)) {
  pts <- c(NULL, NULL)
  tmp <- clus5[[i]]
  for (j in 1:length(tmp)){
    id <- which(nodes$id == tmp[j])
    pts <- rbind(pts, c(coords[id,1L], coords[id,2L]))
  }
  encircle(pts, border=pal3[i], nb.pt=100, off.set=0.8, lwd=2)
}
points(coords[,1], coords[,2], pch=21, bg = pal, col=pal2, cex=4, lwd=2)
text(coords[,1], coords[,2], labels=nodes$idnum)

########
#spinglass community detection
clus6 <- cluster_spinglass(gcon, weights=E(gcon)$weight)
nodes$idgrp <- membership(clus6)

pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
pal3 <- c("#d72968","#a18037",  "#5523b4", "#063632",1)

plot0(range(coords[,1])+.5*c(-1,1), range(coords[,2])+.5*c(-1,1), asp=1)
for (i in 1:nrow(links)) {
  idf <- which(nodes$id == links$from[i])
  idt <- which(nodes$id == links$to[i])
  #
  lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = .75*links$orig.wt[i])
}

for (i in 1:length(clus5)) {
  pts <- c(NULL, NULL)
  tmp <- clus6[[i]]
  for (j in 1:length(tmp)){
    id <- which(nodes$id == tmp[j])
    pts <- rbind(pts, c(coords[id,1L], coords[id,2L]))
  }
  encircle(pts, border=pal3[i], nb.pt=100, off.set=0.8, lwd=2)
}
points(coords[,1], coords[,2], pch=21, bg = pal, col=pal2, cex=4, lwd=2)
text(coords[,1], coords[,2], labels=nodes$idnum)

###########
#multilevel community detection
clus7 <- multilevel.community(gcon, weights=E(gcon)$weight)
nodes$idgrp <- membership(clus7)

pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
pal3 <- c("#a18037",  "#d72968","#5523b4", "#063632",1)

plot0(range(coords[,1])+.5*c(-1,1), range(coords[,2])+.5*c(-1,1), asp=1)
for (i in 1:nrow(links)) {
  idf <- which(nodes$id == links$from[i])
  idt <- which(nodes$id == links$to[i])
  #
  lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = .75*links$orig.wt[i])
}

for (i in 1:length(clus5)) {
  pts <- c(NULL, NULL)
  tmp <- clus7[[i]]
  for (j in 1:length(tmp)){
    id <- which(nodes$id == tmp[j])
    pts <- rbind(pts, c(coords[id,1L], coords[id,2L]))
  }
  encircle(pts, border=pal3[i], nb.pt=100, off.set=0.8, lwd=2)
}
points(coords[,1], coords[,2], pch=21, bg = pal, col=pal2, cex=4, lwd=2)
text(coords[,1], coords[,2], labels=nodes$idnum)
