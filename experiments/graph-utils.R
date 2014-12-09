library(ggplot2)
library(dplyr)
library(reshape)
library(ggthemes)
library(gridExtra)
library(scales)

#########
# Utils #
#########
save_plots <- function(file, ..., keep_legend=FALSE, pdfwidth=18, pdfheight=3.3){
    # Util
    g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)
    }
    # Treats and extracts legends
    graphs <- list(...)
    if (!keep_legend) {
        leg <- g_legend(graphs[[length(graphs)]])
        graphs <- lapply(graphs, function(g){g + theme(legend.position="none")})
        graphs <- c(graphs, list(leg))
        # Deals with legends
        lwidth <- sum(leg$width)
        ratio <- 1.0/length(graphs)
        widths <- unit(rep(ratio, length(graphs)-1), "npc")
        widths <- unit.c(widths, lwidth)
        # Creates graph
        args <- c(graphs, list(widths=widths, nrow=1))
    } else {
        args <- c(graphs, list(nrow=1))
    }
    gr <- do.call("arrangeGrob", args)
    # Files stuff
    unlink(file)
    ggsave(file, gr, width=pdfwidth, height=pdfheight, units="cm")
}

# OK!
prettify <- function(chart, ...) {
    chart <- chart + theme_few(base_size = 8)
    
    chart <- chart + scale_colour_few(...)
    chart <- chart + scale_fill_few(...)
    
    chart <- chart + theme(legend.title=element_blank(),
                           plot.margin=unit(c(0,0,0,0),"cm"),
                            legend.key.height=unit(8,"pt"))
          #axis.title.y=element_text(vjust=0.5),
          #plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))
}