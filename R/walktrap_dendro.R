#' @title Community detection, and network and dendrogram plotting using
#' the walktrap algorithm
#'
#' @description Fig 2 a,b: Community detection, and network and dendrogram plotting using
#' the walktrap algorithm.
#'
#' @author Kevin Cazelles, Helen Wheeler
#'
#' @param nodes set of nodes.
#' @param links set of links.
#' @param vec_col1 a vector describing three colors.
#' @param vec_col2 a vector describing five colors.
#'
#' @importFrom graphics layout lines par points text
#' @importFrom graphicsutils darken encircle plot0
#' @export

walktrap_dendro <- function(links, nodes, vec_col1 = c("#f1bf5f", "#00ac89", "#27ade3"), 
    vec_col2 = c("#d72968", "#a18037", "#5523b4", "#063632", 1)) {
    
    nodes$idnum <- as.numeric(gsub(nodes$id, pattern = "\\D", replacement = ""))
    
    ################################## 
    net <- igraph::graph.data.frame(links, nodes, directed = F)
    gcon <- igraph::simplify(net, edge.attr.comb = list(weight = "sum", function(x) length(x)))
    
    # walktrap community detection using weights
    clus5 <- igraph::cluster_walktrap(gcon, weights = igraph::E(gcon)$weight)
    nodes$idgrp <- igraph::membership(clus5)
    
    coords <- igraph::layout.fruchterman.reingold(gcon)
    
    layout(matrix(c(1, 2), 1), widths = c(0.85, 1))
    par(mar = c(1, 1, 1, 1))
    
    ## Colour palette creation
    pal <- vec_col1[as.factor(nodes$Type)]
    pal2 <- sapply(pal, darken, 30)
    pal3 <- vec_col2
    ## Network plotting
    plot0(range(coords[, 1]) + 0.25 * c(-1, 1), range(coords[, 2]) + 0.25 * c(-1, 
        1), asp = 1)
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
        encircle(pts, border = pal3[i], nb.pt = 100, off.set = 1.8, lwd = 2)
    }
    points(coords[, 1], coords[, 2], pch = 21, bg = pal, col = pal2, cex = 4, lwd = 2)
    text(coords[, 1], coords[, 2], labels = nodes$idnum)
    
    # Dendrogram plotting
    clus5d <- stats::as.hclust(clus5)
    clus5d$labels <- paste0(nodes$idnum, ": ", nodes$Node.name)  #nodes$idnum
    clus5p <- ape::as.phylo(clus5d)
    
    par(mar = c(1, 1, 1, 0))
    
    pal4 <- pal3[nodes$idgrp[clus5p$edge[, 2]]]
    pal4[is.na(pal4)] <- "grey50"
    ape::plot.phylo(clus5p, cex = 0.82, label.offset = 0.25, edge.color = pal4, edge.width = 1.6)
    
}
