# monitoringOutcomes (DOI: 2 be added)

## Overview

R functions for analysis of stakeholder perceptions of desirable monitoring outcomes.
The following scripts relate to the following figures.

- **Fig 1**: `dartdiag.R`
- **Fig 2a-b**: `walktrapdendro4.R`
- **Fig 2c**: `edge_clustering.R`
- **Fig 3**: `connectivity2a.R`
- **Fig S3**: `3netscomp2a.R`

Sets of links and nodes are required to creates figures. They are
included in the `data` folder; three version are furnished. See the `How to` section
to reproduce the figures of the manuscript.

Orientation of networks may differ on each run due to random placement of the
first node during plotting to avoid such behavior, use `set.seed()` with
a specific values (see below).


## An R package?

We have used a standard R package to:

1. efficiently assess the quality of our code (here we simply used `R CMD check`);

2. readily share our code (see installation section below).

We however acknowledge that despite the form of a standard R package, the code
presented is more a organized script than a package (*i.e.* functions included
are not general).

Note that one of the packages required, `graphicsutils`, is not available on
the CRAN. To install it, proceed as follows:

```R
install.packages("devtools")
devtools::install_github("inSileco/graphicsutils")
```



## How to

Before calling any function, load the package:

```R
devtools::load_all()
```

### Figure 1


```R
library(Cairo)
set.seed(1987)
vec_col <-  c("#f1bf5f", "#058544", "#27ade3")
data(nodes)
data(links)
## PNG
png("inst/fig/fig1e.png", width=8, height=10, units="in", res=300)
  dartdiag(nodes, links, vec_col)
dev.off()
## SVG
CairoSVG("inst/fig/fig1.svg", width=8, height=10)
  dartdiag(nodes, links, vec_col)
dev.off()
```

![](inst/fig/fig1.png)


### Figure 2 a and b

```R
set.seed(3466)
data(nodes2)
data(links2)
png("inst/fig/fig2ab.png", units="in", res=300, width=8.5, heigh=6)
  walktrap_dendro(links2, nodes2, vec_col)
dev.off()
## SVG
CairoSVG("inst/fig/fig2ab.svg", width=8.5, heigh=6)
  walktrap_dendro(links2, nodes2, vec_col)
dev.off()
```

![](inst/fig/fig2ab.png)


### Figure 2c

```R
set.seed(3466)
data(nodes3)
data(links3)
## png
png("inst/fig/fig2c.png", units="in", res=300, width=8.5, heigh=6)
  connectivity2a(links3, nodes3)
dev.off()
## svg
CairoSVG("inst/fig/fig2c.svg", width = 8.5, height = 6)
  connectivity2a(links3, nodes3, vec_col, cex_crl = 1.5)
dev.off()
```

![](inst/fig/fig2c.png)

### Figure 3

```R
set.seed(102)
data(links2)
data(nodes2)
vec_names <- sort(gsub(paste0(nodes2[,5L], "  ", nodes2[,2L]), pattern= "^n", replacement = ""))
png("inst/fig/fig3.png", units="in", res=300, width=8.5, heigh=6.5)
  edge_clustering(links2, vec_names)
dev.off()
## SVG
CairoSVG("inst/fig/fig3.svg",width=8.5, heigh=6.5)
  edge_clustering(links2, vec_names)
dev.off()
```



![](inst/fig/fig3.png)


### Figure S3

```R
set.seed(81932)
data(nodes2)
data(links2)
png("inst/fig/figS3.png", units="in", res=300, width=8, heigh=5)
  netcomp2a(links2, nodes2)
dev.off()
##
CairoSVG("inst/fig/figS3.svg", width=8, heigh=5)
  netcomp2a(links2, nodes2, vec_col)
dev.off()
```

![](inst/fig/figS3.png)
