#' @title Edge communities and contribution of edges
#'
#' @description Figure 3. Network diagram showing edge communities and contribution of edges from each community to node themes.
#'
#' @author Helen Wheeler, Kevin Cazelles
#'
#' @param links set of links.
#' @param vec_names a vector of character strings that names the nodes.
#' @param vec_col a vector describing three colors.
#'
#' @importFrom graphicsutils lighten plot0
#' @importFrom graphics layout legend par
#'
#' @export

edge_clustering <- function(links, vec_names = NULL, vec_col = c("#993209", "#0c67c8", 
    "#f7e37e")) {
    # simplified list of edges with weights
    links_tmp <- as.data.frame(cbind(substring(links$to, 2), substring(links$from, 
        2), links[, 6L]))
    names(links_tmp) <- c("to", "from", "weight")
    links_tmp$weight <- as.numeric(as.character(links$weight))
    
    # colour palette for plotting
    pal4 <- sapply(vec_col, lighten, 40)
    
    tmp <- !is.null(names)
    if (tmp) 
        layout(matrix(2:1, 1), widths = c(1, 0.86))
    # community analyses
    par(mar = c(1, 0, 1, 1))
    lc <- linkcomm::getLinkCommunities(links_tmp, directed = FALSE, removetrivial = FALSE, 
        plot = FALSE)
    
    if (tmp) {
        plot0()
        legend("center", legend = vec_names, bty = "n", cex = 1.32)
    }
    
    # plotting with fruchterman reingold placement and pies showing node degree for
    # weighted network
    linkcomm:::plot.linkcomm(lc, type = "graph", vertex.radius = 0.07, margin = -1.5, 
        pal = pal4, ewidth = linkcomm::graph.feature(lc, type = "edges", indices = linkcomm::getEdgesIn(lc), 
            features = 5, default = 1), layout = igraph::layout.fruchterman.reingold, 
        col.nonclusters = "grey", showall = TRUE)
    
}
