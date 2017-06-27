######### Packages
library(igraph)
# devtools::install_github("KevCaz/graphicsutils")
library(graphicsutils)

nodes <- saveRDS(nodes, "dataReady/nodes.Rds")
######
#
# nodes2 <- nodes[order(nodes$idnum),]
# nnd <- nrow(nodes2)
# plot0(c(0,1), c(0, nnd+1))
# for (i in 1:nnd){
#   text(0, nnd-i, labels=paste0(nodes2$idnum[i], ": ", nodes2$Node.name[i]), pos=4)
# }

# inter and intra links draft
nodes$inter <- nodes$intra <- 0
for (i in 1:nrow(links)){
  idf <- which(nodes$id == links[i,1L])
  idt <- which(nodes$id == links[i,2L])
  if (nodes$idgrp[idf] == nodes$idgrp[idt]){
    nodes$intra[idf] <- nodes$intra[idf]+links$orig.wt[i]
    nodes$intra[idt] <- nodes$intra[idt]+links$orig.wt[i] #HW added to include other linking direction
  } else {
    nodes$inter[idf] <- nodes$inter[idf]+links$orig.wt[i]
    nodes$inter[idt] <- nodes$inter[idt]+links$orig.wt[i] #HW added to include other linking direction
  }
}

par(mar=c(4,4,2,2), las=1)
plot(nodes$intra, nodes$inter, pch=21, col=pal2, cex=3, bg=pal, xlab="cummulative link weight - intra group", ylab = "cummulative links weight - inter group")
abline(v = mean(nodes$intra), h = mean(nodes$inter), lty=2, col="grey50")
text(nodes$intra, nodes$inter,, labels=nodes$idnum)
