####################################################
######## Network x closness
####################################################


######### KC: Packages
library(igraph) # used to calculate the closness
library(plotrix)
######## KC: I've finally decided to use my package
## to install my package, you need the devtools packages
library(devtools)
## load it
## and now:
# install_github("KevCaz/graphicsutils")

# setwd("E:/WD Apps for Windows/Mar2017BUEXTFull/NEW/TAMANI/SOCSCI/NVIVO/Nodes/")#setwd("/Users/KevCaz/Desktop/Helen")
setwd("~/Codes/Github/Figures/Figures_Wheeler2017")


######### KC: Importing data
nodes <- read.csv("data/Nodes5_reduced5con2.csv", header=T, as.is=T)
links <- read.csv("data/Relationships_woassoc_reduced2.csv", header=T, as.is=T)
## to improve the visual, I've swapped two elements
nodes <- nodes[c(13,2:12,1,14:18),]


######### Create copy of original weight
links <- cbind(links, links$weight)
names(links)[7] <- "orig.wt"
#used a I graph uses its weigh as a resistance to movement
links$weight<- 1 - links$weight/(max(links$weight)+min(links$weight)/max(links$weight))

net <- graph.data.frame(links, nodes, directed=T)
saveRDS(net, "dataReady/network.Rds")
# V(net)
# cluster_louvain(net, weights=links$weight)

clos <- closeness(net, mode="out")
## add closness to the df.
nodes$clos <- 1/clos

seqc <- rev(pi/2+seq(0, 2*pi, length.out=nrow(nodes)+1)[-1])
######### KC: Nodes coordinates
nodes$x <- (3+1*(nodes$clos/min(nodes$clos)))*cos(seqc)
nodes$y <- (3+1*(nodes$clos/min(nodes$clos)))*sin(seqc)


######### KC: title attributes
title.angle <- 180*(seqc)/pi
id <- which(seqc>pi/2 & seqc<3*pi/2)
title.angle[id] <- title.angle[id]+180

######### KC: links coordinates and other info used when making the graph.
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
saveRDS(links, "dataReady/links.Rds")


######### KC: color palettes (I used the same color as on the other graphs)
pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(nodes$Type)]
pal2 <- sapply(pal, darken, 30)
links.pal <- c("#edac35", "#64c4eb", "#00ac89")[as.factor(links$Type)]




######### KC: Here the plot start
png("fig/diag3.png", width=8, height=10, units="in", res=300)

  layout(matrix(c(1,2), nrow=2), height=c(1, .2))
  par(mar=c(0,1,1,1), lend=2)

  ## plot0 is my 'empty plot' function
  plot0(c(-15, 15), asp=1)
  ## KC: I've used strwidth to calculate the plot it requires a new plot window
  ## to be open.
  title.coords <- cbind(
      x = (10.25+.5*strwidth(nodes$Node.name))*cos(seqc),
      y = (10.25+.5*strwidth(nodes$Node.name))*sin(seqc)
    )
  ## KC: Improving the readability of the figure (`circle` is my own circle function)
  dan <- .5*(seqc[1]-seqc[2])
  for (i in 1:(length(seqc)-1)) {
    circle(0, 0, 10, from=seqc[i+1]-dan, to=seqc[i]-dan,
      col = c(NA, "grey95")[1+i%%2],
      border=FALSE, pie=TRUE)
  }
  ## KC: I do a loop to draw the circles!
  radi <- seq(10, 4, by= -1)
  for (i in radi) circle(0, 0, i, lty=2, border="grey50")

  ## KC: LINKS and ARROWS' HEADS - Here it starts to be tricky I calculate the links and arrow coordinates
  ## so have the arrows arrive on the circle edges not on the centre of the circle.
  for (i in 1:nrow(links)){
    ag <- getAngle2d(links$xf[i], links$yf[i], links$xt[i], links$yt[i])
    hx <- cos(pi*ag/180)*.045*links$node_to_cex[i]
    hy <- sin(pi*ag/180)*.045*links$node_to_cex[i]
    lines(c(links$xf[i], links$xt[i]-hx), c(links$yf[i], links$yt[i]-hy), col = links.pal[i], lwd = .75*links$orig.wt[i])
    text(links$xt[i]-hx, links$yt[i]-hy, labels=intToUtf8(9658), srt=ag,
      cex=1+.1*links$orig.wt[i], col=links.pal[i])
    }

  ## KC: NODES
  points(nodes$x, nodes$y, cex=0.2*nodes$No.sources, pch=21, col = pal2, bg=pal, lwd=1.5)

  ## KC: LABELS
  for (i in 1:length(seqc)) {
    strwidth(nodes$Node.name[1])
    text(x=title.coords[i,1], y=title.coords[i,2], nodes$Node.name[i],
       srt=title.angle[i], cex=0.9, col="black") #pal2[i])
  }

## KC: Legend
par(mar=c(1,1,0,1), lend=2)
plot0()
sqs <- c(5,10,30)
sql <- c(1,5,10)
legend(-.7, 1, legend = paste0(sqs, " sources"), pt.cex=0.2*sqs, pch=21, y.intersp=2, x.intersp=2, bty="n")
sqs <- c(5,10,30)
legend(.2, 1, legend = paste0(sqs, " connections"), lwd = .75*sql, y.intersp=2, x.intersp=2, bty="n")

dev.off()
