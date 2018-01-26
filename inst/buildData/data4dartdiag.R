nodes <- read.csv("inst/datafinal/Nodes_dartdiag.csv", header=T, as.is=T)
links <- read.csv("inst/datafinal/Relationships_dartdiag.csv", header=T, as.is=T)
#### to improve the visual, I've swapped two elements
nodes <- nodes[c(13,2:12,1,14:18),]


# Create copy of original weight
links <- cbind(links, links$weight)
names(links)[7L] <- "orig.wt"
##### igraph uses its weight as a resistance to movement, therefore weights are transformed
links$weight <- 1 - links$weight/(max(links$weight)+min(links$weight)/max(links$weight))
network <- igraph::graph.data.frame(links, nodes, directed=T)

# V(net)
# cluster_louvain(net, weights=links$weight)

clos <- igraph::closeness(network, mode="out")
## add closness to the df.
nodes$clos <- 1/clos

seqc <- rev(pi/2+seq(0, 2*pi, length.out=nrow(nodes)+1)[-1])

#### KC: Nodes coordinates
nodes$x <- (3+1*(nodes$clos/min(nodes$clos)))*cos(seqc)
nodes$y <- (3+1*(nodes$clos/min(nodes$clos)))*sin(seqc)
save(nodes, file = "data/nodes.rda", compress = 'xz')


#### KC: links coordinates and other info used when making the graph.
links$xf <- links$yf <- links$yt <- links$yt <- NA
links$type_to <- links$type_from <- links$node_to_cex <- NA
for (i in 1:nrow(links)){
  idf <- which(nodes$id == links$from[i])
  idt <- which(nodes$id == links$to[i])
  ##
  links$xf[i] <- nodes$x[idf]
  links$yf[i] <- nodes$y[idf]
  links$xt[i] <- nodes$x[idt]
  links$yt[i] <- nodes$y[idt]
  links$type_from[i] <- nodes$Type[idf]
  links$type_to[i] <- nodes$Type[idt]
  links$node_to_cex[i] <- nodes$No.sources[idt]
}
save(links, file = "data/links.rda", compress = 'xz')
