######### Packages
library(igraph)
library(plotrix) # draw.circle()
library(ape)
# devtools::install_github("KevCaz/graphicsutils")
library(graphicsutils)


# setwd("E:/WD Apps for Windows/Mar2017BUEXTFull/NEW/TAMANI/SOCSCI/NVIVO/Nodes/")#setwd("/Users/KevCaz/Desktop/Helen")
setwd("~/Codes/Github/Figures/Figures_Wheeler2017")
# source("R/walktrap_community2.R")

######### Import data
nodes <- read.csv("data/Nodes5_reduced5con2.csv", header=T, as.is=T)
links <- readRDS("dataReady/links.Rds")
nodes$idnum <- as.numeric(gsub(nodes$id, pat="\\D", rep=""))

##################################
net <- graph.data.frame(links, nodes, directed=F)
gcon <- simplify(net, edge.attr.comb = list(weight = "sum", function(x)length(x)))

# multilevel community detection using weights
clus5 <- cluster_walktrap(gcon, weights=E(gcon)$weight)
nodes$idgrp <- membership(clus5)

# coords <- layout.fruchterman.reingold(gcon)
# saveRDS(coords, "data/coords.Rds")
coords <- readRDS("data/coords.Rds")
#########
png("fig/fignetden.png", units="in", res=300, width=8, heigh=6)
layout(matrix(c(1,2),1), width = c(.85,1))
par(mar=c(1,1,1,1))
#########

##
pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
pal3 <- c("#d72968", "#a18037", "#5523b4", "#063632",1)
##
##
plot0(range(coords[,1])+.25*c(-1,1), range(coords[,2])+.25*c(-1,1), asp=1)
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
  encircle(pts, border=pal3[i], nb.pt=100, off.set=1.8, lwd=2)
}
points(coords[,1], coords[,2], pch=21, bg = pal, col=pal2, cex=4, lwd=2)
text(coords[,1], coords[,2], labels=nodes$idnum)

########

#for the dendrogram I would like to plot the labels (as used for the nodes rather than
#short names)
clus5d <- as.hclust(clus5)
clus5d$labels <- paste0(nodes$idnum, ": ", nodes$Node.name) #nodes$idnum
clus5p <- as.phylo(clus5d)

pal4 <- pal3[nodes$idgrp[clus5p$edge[,2]]]
pal4[is.na(pal4)] <- "grey50"
plot.phylo(clus5p, cex = .82, label.offset = 0.25, edge.color=pal4, edge.width=1.8)
# box2()
abline(v=1.8, col="grey15", lwd=.8)
dev.off()

saveRDS(nodes, "dataReady/nodes.Rds")
######
#
# nodes2 <- nodes[order(nodes$idnum),]
# nnd <- nrow(nodes2)
# plot0(c(0,1), c(0, nnd+1))
# for (i in 1:nnd){
#   text(0, nnd-i, labels=paste0(nodes2$idnum[i], ": ", nodes2$Node.name[i]), pos=4)
# }

## inter and intra links draft
# nodes$inter <- nodes$intra <- 0
# for (i in 1:nrow(links)){
#   idf <- which(nodes$id == links[i,1L])
#   idt <- which(nodes$id == links[i,2L])
#   if (nodes$idgrp[idf] == nodes$idgrp[idt]){
#     nodes$intra[idf] <- nodes$intra[idf]+links$orig.wt[i]
#   } else {
#     nodes$inter[idf] <- nodes$inter[idf]+links$orig.wt[i]
#   }
# }
#
# par(mar=c(4,4,2,2), las=1)
# plot(nodes$intra, nodes$inter, pch=21, col=pal2, cex=3, bg=pal, xlab="cummulative link weight - intra group", ylab = "cummulative links weight - inter group")
# abline(v = mean(nodes$intra), h = mean(nodes$inter), lty=2, col="grey50")
# text(nodes$intra, nodes$inter,, labels=nodes$idnum)
