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

    # colour palette for plotting
    pal <- sapply(vec_col, lighten, 10)

    tmp <- !is.null(names)
    if (tmp)
        layout(matrix(2:1, 1), widths = c(1, 0.8))
    # community analyses
    par(mar = c(1, 0, 1, 1))
    lc <- linkcomm::getLinkCommunities(links, directed = FALSE, removetrivial = FALSE,
        plot = FALSE)

    if (tmp) {
        plot0()
        legend("center", legend = vec_names, bty = "n", cex = 1.1)
    }

    # plotting with fruchterman reingold placement and pies showing node degree for
    # weighted network
    par(mar = c(0,1,0,0))
    linkcomm:::plot.linkcomm(lc, type = "graph", vertex.radius = 0.07,
      margin = -1, pal = pal, vlabel.cex = 1.25,
      ewidth = graph.feature(lc, type = "edges", indices = getEdgesIn(lc),
      features = 5, default = 1),
      layout = layout.fruchterman.reingold, col.nonclusters = "grey",
      showall = TRUE)

    # linkcomm:::plot.linkcomm(lc, type = "graph", vertex.radius = 0.07, margin = -1.5,
    #     pal = pal4, ewidth = linkcomm::graph.feature(lc, type = "edges", indices = linkcomm::getEdgesIn(lc),
    #         features = 5, default = 1), layout = igraph::layout.fruchterman.reingold,
    #     col.nonclusters = "grey", showall = TRUE)
}
