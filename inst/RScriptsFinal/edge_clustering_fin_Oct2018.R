##########################
#Plots network using analysis of edges
##########################
#Main by Helen Wheeler, additional Kevin Cazelles
setwd("E:/WD Apps for Windows/Mar2017BUEXTFull/NEW/TAMANI/SOCSCI/ConsBio/FinalGitHub/")


#install.packages("linkcomm")
library(linkcomm)
library(igraph)
library(graphicsutils)
library(igraph)
#vignette(topic = "linkcomm", package = "linkcomm")

# data
nodes <- read.csv("inst/datafinal/Nodes_names.csv", header=T, as.is=T)
links <- read.csv("inst/datafinal/Relationships_namesedges.csv", header=T, as.is=T)

#simplified list of edges with weights
links2<-(as.data.frame(cbind(substring(links$to, 2),
         substring(links$from,2),links[,6])))
names(links2)<-c("to","from","weight")
links2$weight<-as.numeric(as.character(links$weight))

nodes2 <- nodes

save(links2, file = "data/links2.rda")
save(nodes2, file = "data/nodes2.rda")

#colour palette for plotting
pal3 <- c("#aa380b" ,"#0b58aa" ,"#f7e37e")
pal4 <- pal3 #sapply(pal3, lighten, 50)

#community analyses
lc <- getLinkCommunities(links2, directed=FALSE, removetrivial=FALSE)

#plotting with fruchterman reingold placement and pies showing node degree for weighted network
set.seed(2)
svg("fig/fig3b.svg", width=8, height=10)
plot(lc, type = "graph", vertex.radius=0.07,margin=-1, pal=pal4, vlabel.cex=1.4,
    ewidth = graph.feature(lc, type = "edges", indices = getEdgesIn(lc), features = 5, default = 1),
   layout = layout.fruchterman.reingold, col.nonclusters="grey", showall=TRUE)

 dev.off()
