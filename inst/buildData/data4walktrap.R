######### Import data
nodes <- nodes2 <- read.csv("inst/datafinal/Nodes_names.csv", header=T, as.is=T)
links <- links2 <- read.csv("inst/datafinal/Relationships_names.csv", header=T, as.is=T)

# Additional code for computation of connectivity (files used in connectivity.R script)
save(nodes2, file = "data/nodes2.rda", compress = 'xz')
save(links2, file = "data/links2.rda", compress = 'xz')


nodes$idnum <- as.numeric(gsub(nodes$id, pat="\\D", rep=""))

##################################
net <- igraph::graph.data.frame(links, nodes, directed=F)
gcon <- igraph::simplify(net, edge.attr.comb = list(weight = "sum", function(x)length(x)))

# walktrap community detection using weights
clus5 <- igraph::cluster_walktrap(gcon, weights=igraph::E(gcon)$weight)
nodes$idgrp <- igraph::membership(clus5)

coords <- igraph::layout.fruchterman.reingold(gcon)

#########
png("fig/fignetden.png", units="in", res=300, width=8, heigh=6)
layout(matrix(c(1,2),1), width = c(.85,1))
par(mar=c(1,1,1,1))
#########

##Colour palette creation
pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
pal3 <- c("#d72968", "#a18037", "#5523b4", "#063632",1)
##
##
#Network plotting
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

#Dendrogram plotting
clus5d <- stats::as.hclust(clus5)
clus5d$labels <- paste0(nodes$idnum, ": ", nodes$Node.name) #nodes$idnum
clus5p <- ape::as.phylo(clus5d)

pal4 <- pal3[nodes$idgrp[clus5p$edge[,2]]]
pal4[is.na(pal4)] <- "grey50"
ape::plot.phylo(clus5p, cex = .82, label.offset = 0.25, edge.color=pal4, edge.width=1.8)


dev.off()


#########
links$xf <- links$yf <- links$yt <- links$yt <- NA
links$type_to <- links$type_from <- links$node_to_cex <- NA
for (i in 1:nrow(links)){
  idf <- which(nodes$id == links$from[i])
  idt <- which(nodes$id == links$to[i])
  ##
#  links$xf[i] <- nodes$x[idf]
#  links$yf[i] <- nodes$y[idf]
#  links$xt[i] <- nodes$x[idt]
#  links$yt[i] <- nodes$y[idt]
  links$type_from[i] <- nodes$Type[idf]
  links$type_to[i] <- nodes$Type[idt]
  links$node_to_cex[i] <- nodes$No.sources[idt]
}

nodes3 <- nodes
links3 <- links

save(links3, file = "data/links3.rda", compress = 'xz')
save(nodes3, file = "data/nodes3.rda", compress = 'xz')
