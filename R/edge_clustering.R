#' @title Edge communities and contribution of edges
#'
#' @description Figure 3. Network diagram showing edge communities and contribution of edges from each community to node themes.
#'
#' @author Helen Wheeler, Kevin Cazelles
#'
#' @param links set of links.
#' @param vec_col a vector describing three colors.
#'
#' @importFrom graphicsutils lighten
#'
#' @export

edge_clustering <- function(links, vec_col = c("#aa380b", "#0b58aa", "#f7e37e")) {
    # simplified list of edges with weights
    links2 <- (as.data.frame(cbind(substring(links$to, 2), substring(links$from, 
        2), links[, 6L])))
    names(links2) <- c("to", "from", "weight")
    links2$weight <- as.numeric(as.character(links$weight))
    
    # colour palette for plotting
    pal3 <- vec_col
    pal4 <- sapply(pal3, lighten, 50)
    
    # community analyses
    lc <- linkcomm::getLinkCommunities(links2, directed = FALSE, removetrivial = FALSE, 
        plot = FALSE)
    
    # plotting with fruchterman reingold placement and pies showing node degree for
    # weighted network
    linkcomm:::plot.linkcomm(lc, type = "graph", vertex.radius = 0.07, margin = -1.5, 
        pal = pal4, ewidth = linkcomm::graph.feature(lc, type = "edges", indices = linkcomm::getEdgesIn(lc), 
            features = 5, default = 1), layout = igraph::layout.fruchterman.reingold, 
        col.nonclusters = "grey", showall = TRUE)
}
