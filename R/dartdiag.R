#' @title Dart-styled diagram: Network, closness and more.
#'
#' @description Figure 1. Network diagram of the desirable impacts of monitoring
#' as identified by 29 stakeholders in arctic monitoring.
#'
#' @author Kevin Cazelles, Helen Wheeler
#'
#' @param nodes set of nodes.
#' @param links set of links.
#' @param vec_col a vector describing three colors.
#'
#' @importFrom graphics layout legend lines par points text strwidth
#' @importFrom graphicsutils circles darken getAngle2d plot0
#' @export
### HW: Small colour change
dartdiag <- function(nodes, links, vec_col = c("#f1bf5f", "#00ac89", "#27ade3")) {
    
    #### KC: color palettes (I used the same color as on the other graphs)
    pal <- vec_col[as.factor(nodes$Type)]
    pal2 <- sapply(pal, darken, 30)
    
    #### HW: small code change for colour coding (Type -> type_from)
    links.pal <- vec_col[as.factor(links$type_from)]
    
    #### KC: title attributes
    seqc <- rev(pi/2 + seq(0, 2 * pi, length.out = nrow(nodes) + 1)[-1])
    title.angle <- 180 * (seqc)/pi
    id <- which(seqc > pi/2 & seqc < 3 * pi/2)
    title.angle[id] <- title.angle[id] + 180
    
    layout(matrix(c(1, 2), nrow = 2), heights = c(1, 0.25))
    par(mar = c(0, 0, 0, 0), lend = 2)
    
    plot0(c(-16, 16))
    ## KC: I've used strwidth to calculate the plot it requires a new plot window to
    ## be open.
    title.coords <- cbind(x = (10.25 + 0.5 * strwidth(nodes$Node.name)) * cos(seqc), 
        y = (10.25 + 0.5 * strwidth(nodes$Node.name)) * sin(seqc))
    ## KC: Improving the readability of the figure (`circle` is my own circle
    ## function)
    dan <- 0.5 * (seqc[1] - seqc[2])
    for (i in 1:(length(seqc) - 1)) {
        circles(0, 0, 10, from = seqc[i + 1] - dan, to = seqc[i] - dan, col = c(NA, 
            "grey90")[1 + i%%2], border = FALSE, pie = TRUE)
    }
    ## KC: I do a loop to draw the circles!
    radi <- seq(10, 4, by = -1)
    for (i in radi) circles(0, 0, i, lty = 2, border = "grey50")
    
    ## KC: LINKS and ARROWS' HEADS - Here it starts to be tricky I calculate the links
    ## and arrow coordinates so have the arrows arrive on the circle edges not on the
    ## centre of the circle.
    for (i in 1:nrow(links)) {
        ag <- getAngle2d(links$xf[i], links$yf[i], links$xt[i], links$yt[i])
        hx <- cos(pi * ag/180) * 0.045 * links$node_to_cex[i]
        hy <- sin(pi * ag/180) * 0.045 * links$node_to_cex[i]
        lines(c(links$xf[i], links$xt[i] - hx), c(links$yf[i], links$yt[i] - hy), 
            col = links.pal[i], lwd = 0.75 * links$orig.wt[i])
        text(links$xt[i] - hx, links$yt[i] - hy, labels = intToUtf8(9658), srt = ag, 
            cex = 1 + 0.1 * links$orig.wt[i], col = links.pal[i])
    }
    
    ## KC: NODES
    points(nodes$x, nodes$y, cex = 0.2 * nodes$No.sources, pch = 21, col = pal2, 
        bg = pal, lwd = 1.8)
    
    ## KC: LABELS
    for (i in 1:length(seqc)) {
        strwidth(nodes$Node.name[1L])
        text(x = title.coords[i, 1], y = title.coords[i, 2L], nodes$Node.name[i], 
            srt = title.angle[i], cex = 0.9, col = "black")  #pal2[i])
    }
    
    ## KC: Legend
    par(mar = c(1, 1, 0, 1), lend = 2)
    plot0()
    sqs <- c(5, 10, 30)
    sql <- c(1, 5, 10)
    legend(-0.8, 1, legend = paste0(sqs, " sources"), pt.cex = 0.2 * sqs, pch = 21, 
        y.intersp = 2, x.intersp = 2, bty = "n")
    legend(-0.3, 1, legend = paste0(sql, " connection", c("", "", "s")), lwd = 0.75 * 
        sql, y.intersp = 2, x.intersp = 2, bty = "n")
    # HW added legend item for colours, and adjusted positioning for others
    legend(0.35, 1, legend = c("Information", "Process", "Combined"), pt.cex = 4, 
        pch = 19, col = vec_col[c(2, 3, 1)], y.intersp = 2, x.intersp = 2, bty = "n")
}
