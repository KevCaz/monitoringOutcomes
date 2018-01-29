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
    #### 
    net <- igraph::graph.data.frame(links, nodes, directed = F)
    gcon <- igraph::simplify(net, edge.attr.comb = list(weight = "sum", function(x) length(x)))
    #### Different clustering algorithms
    ls_clus <- list()
    ls_clus[[1L]] <- igraph::cluster_walktrap(gcon, weights = igraph::E(gcon)$weight)
    ls_clus[[2L]] <- igraph::cluster_spinglass(gcon, weights = igraph::E(gcon)$weight)
    ls_clus[[3L]] <- igraph::multilevel.community(gcon, weights = igraph::E(gcon)$weight)
    
    ## Plotting
    par(mar = c(1, 1, 1, 1), mfrow = c(1, 3))
    #### 
    coords <- igraph::layout.fruchterman.reingold(gcon)
    #### Colour palette creation
    pal <- vec_col1[as.factor(nodes$Type)]
    pal2 <- sapply(pal, darken, 30)
    pal3 <- vec_col2
    
    for (k in 1:3) {
        ## 
        plot0(range(coords[, 1L]) + 0.5 * c(-1, 1), range(coords[, 2L]) + 0.5 * c(-1, 
            1), asp = 1)
        for (i in 1:nrow(links)) {
            idf <- which(nodes$id == links$from[i])
            idt <- which(nodes$id == links$to[i])
            # 
            lines(coords[c(idf, idt), 1L], coords[c(idf, idt), 2L], col = "grey50", 
                lwd = 0.75 * links$orig.wt[i])
        }
        
        for (i in 1:length(ls_clus[[k]])) {
            pts <- c(NULL, NULL)
            tmp <- ls_clus[[k]][[i]]
            for (j in 1:length(tmp)) {
                id <- which(nodes$id == tmp[j])
                pts <- rbind(pts, c(coords[id, 1L], coords[id, 2L]))
            }
            encircle(pts, border = pal3[i], nb.pt = 100, off.set = 0.9, lwd = 2)
        }
        points(coords[, 1L], coords[, 2L], pch = 21, bg = pal, col = pal2, cex = 3.6, 
            lwd = 1.8)
        text(coords[, 1L], coords[, 2L], labels = nodes$idnum)
    }
    
}