#' @title Network of desired impacts from monitoring
#'
#' @description Fig S1: Plotting of network of desired impacts from monitoring -
#' comparison of three algorithms.
#'
#' @author Helen Wheeler, Kevin Cazelles
#'
#' @param nodes set of nodes.
#' @param links set of links.
#' @param vec_col1 a vector describing three colors.
#' @param vec_col2 a vector describing five colors.
#'
#' @importFrom graphics layout lines par points text
#' @importFrom graphicsutils darken encircle plot0
#' @export

netcomp2a <- function(links, nodes, vec_col1 = c("#edac35", "#64c4eb", "#00ac89"), 
    vec_col2 = c("#d72968", "#a18037", "#5523b4", "#063632", 1)) {
    
    nodes$idnum <- as.numeric(gsub(nodes$id, pattern = "\\D", replacement = ""))
    
    ################################## 
    net <- igraph::graph.data.frame(links, nodes, directed = F)
    gcon <- igraph::simplify(net, edge.attr.comb = list(weight = "sum", function(x) length(x)))
    
    # walktrap community detection using weights
    clus5 <- igraph::cluster_walktrap(gcon, weights = igraph::E(gcon)$weight)
    nodes$idgrp <- igraph::membership(clus5)
    
    coords <- igraph::layout.fruchterman.reingold(gcon)
    
    par(mar = c(1, 1, 1, 1), mfrow = c(1, 3))
    ## Colour palette creation
    pal <- vec_col1[as.factor(nodes$Type)]
    pal2 <- sapply(pal, darken, 30)
    pal3 <- vec_col2
    ## 
    plot0(range(coords[, 1]) + 0.5 * c(-1, 1), range(coords[, 2]) + 0.5 * c(-1, 1), 
        asp = 1)
    for (i in 1:nrow(links)) {
        idf <- which(nodes$id == links$from[i])
        idt <- which(nodes$id == links$to[i])
        # 
        lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = 0.75 * 
            links$orig.wt[i])
    }
    
    for (i in 1:length(clus5)) {
        pts <- c(NULL, NULL)
        tmp <- clus5[[i]]
        for (j in 1:length(tmp)) {
            id <- which(nodes$id == tmp[j])
            pts <- rbind(pts, c(coords[id, 1L], coords[id, 2L]))
        }
        encircle(pts, border = pal3[i], nb.pt = 100, off.set = 0.8, lwd = 2)
    }
    points(coords[, 1], coords[, 2], pch = 21, bg = pal, col = pal2, cex = 4, lwd = 2)
    text(coords[, 1], coords[, 2], labels = nodes$idnum)
    
    
    # spinglass community detection
    clus6 <- igraph::cluster_spinglass(gcon, weights = igraph::E(gcon)$weight)
    nodes$idgrp <- igraph::membership(clus6)
    
    plot0(range(coords[, 1]) + 0.5 * c(-1, 1), range(coords[, 2]) + 0.5 * c(-1, 1), 
        asp = 1)
    for (i in 1:nrow(links)) {
        idf <- which(nodes$id == links$from[i])
        idt <- which(nodes$id == links$to[i])
        # 
        lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = 0.75 * 
            links$orig.wt[i])
    }
    
    for (i in 1:length(clus5)) {
        pts <- c(NULL, NULL)
        tmp <- clus6[[i]]
        for (j in 1:length(tmp)) {
            id <- which(nodes$id == tmp[j])
            pts <- rbind(pts, c(coords[id, 1L], coords[id, 2L]))
        }
        encircle(pts, border = pal3[i], nb.pt = 100, off.set = 0.8, lwd = 2)
    }
    points(coords[, 1], coords[, 2], pch = 21, bg = pal, col = pal2, cex = 4, lwd = 2)
    text(coords[, 1], coords[, 2], labels = nodes$idnum)
    
    ########### multilevel community detection
    clus7 <- igraph::multilevel.community(gcon, weights = igraph::E(gcon)$weight)
    nodes$idgrp <- igraph::membership(clus7)
    
    plot0(range(coords[, 1]) + 0.5 * c(-1, 1), range(coords[, 2]) + 0.5 * c(-1, 1), 
        asp = 1)
    for (i in 1:nrow(links)) {
        idf <- which(nodes$id == links$from[i])
        idt <- which(nodes$id == links$to[i])
        # 
        lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", lwd = 0.75 * 
            links$orig.wt[i])
    }
    
    for (i in 1:length(clus5)) {
        pts <- c(NULL, NULL)
        tmp <- clus7[[i]]
        for (j in 1:length(tmp)) {
            id <- which(nodes$id == tmp[j])
            pts <- rbind(pts, c(coords[id, 1L], coords[id, 2L]))
        }
        encircle(pts, border = pal3[i], nb.pt = 100, off.set = 0.8, lwd = 2)
    }
    points(coords[, 1], coords[, 2], pch = 21, bg = pal, col = pal2, cex = 4, lwd = 2)
    text(coords[, 1], coords[, 2], labels = nodes$idnum)
    
}
