##########################
#Plots network using analysis of edges
##########################
#Main by Helen Wheeler, additional Kevin Cazelles

setwd("~/Codes/Github/Figures/Figures_Wheeler2017")

#install.packages("linkcomm")
library(linkcomm)
library(igraph)
library(graphicsutils)
library(igraph)
#vignette(topic = "linkcomm", package = "linkcomm")

# data
nodes <- read.csv("inst/datafinal/Nodes_names.csv", header=T, as.is=T)
links <- read.csv("inst/datafinal/Relationships_names.csv", header=T, as.is=T)

#simplified list of edges with weights
links2<-(as.data.frame(cbind(substring(links$to, 2),
         substring(links$from,2),links[,6L])))
names(links2)<-c("to","from","weight")
links2$weight<-as.numeric(as.character(links$weight))

#colour palette for plotting
pal3 <- c("#aa380b" ,"#0b58aa" ,"#f7e37e")
pal4 <- sapply(pal3, lighten, 50)

#community analyses
lc <- linkcomm::getLinkCommunities(links2, directed=FALSE, removetrivial=FALSE, plot=FALSE)

#plotting with fruchterman reingold placement and pies showing node degree for weighted network
par(mfrow=c(1,1))
plot(lc, type = "graph", vertex.radius=0.07,margin=-1.5, pal=pal4,
    ewidth = linkcomm::graph.feature(lc, type = "edges", indices = linkcomm::getEdgesIn(lc), features = 5, default = 1),
   layout = igraph::layout.fruchterman.reingold, col.nonclusters="grey", showall = TRUE)
