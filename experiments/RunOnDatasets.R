library(foreign)
library(hexbin)
source("MakeViews.R", chdir = TRUE)

FEW_COLS = c(
    green="#008C48",
    blue="#185AA9",
    orange="#F47D23",
    red="#EE2E2F",
    purple="#662C91",
    maroon="#A21D21",
    magenta="#B43894"
)

############
# Plotting #
############
plot_group <- function(i, groups, group = NULL){
    columns <- groups[[1]][[i]]$columns
    subgroups <- groups[[2]][[i]]
    colors <- rep("#CCCCCC", nrow(data_file))
    
    the_groups <- if (is.null(group)) {
        1:length(subgroups)
    } else {
        c(group)
    }
    
    for (n in the_groups){
        gp <- subgroups[[n]]
        colors[gp$items] <- FEW_COLS[which(the_groups == n)]
    }
    
    parcoord(data_file[, columns], col = colors, var.label = TRUE)
}

prettify <- function(chart, ...) {
    chart <- chart + theme_few(base_size = 8)
    chart <- chart + theme(legend.key.height=unit(8,"pt"),
                           plot.margin=unit(c(0.1,0.5,0.1,0.5),"cm"))
}

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

#################
# Crime Dataset #
#################
data_file <- read.arff("~/Projects/TurboGroups/data/communities.arff")
data_file <- select(data_file, -community)

# Execution
target <- "ViolentCrimesPerPop"

views1 <- generate_views(data_file, target, 
                       nbins_target = 2,
                       q=100, size_view=3, size_beam_q=100, dup_factor=NULL, # 100-100-[-250] is optimal
                       min_freq = 0.01, k=50, size_beam_k=50, nbins=5, levels=3,
                       logfun = function(...) print(paste(...)),
                       outfun = function(...) print(paste(...))
)

views2 <- generate_views(data_file, target, 
                         nbins_target = 2,
                         q=100, size_view=3, size_beam_q=100, dup_factor=250, # 100-100-[-250] is optimal
                         min_freq = 0.01, k=50, size_beam_k=50, nbins=5, levels=3,
                         logfun = function(...) print(paste(...)),
                         outfun = function(...) print(paste(...))
)
#######################
# Plots for old Views #
#######################
# Plotting view 1 - POI 28
g1 <- ggplot(data_file, aes(x=racePctWhite * 100, y=PctVacantBoarded * 100, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint = 23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Pct.Race.White (%)") + 
    ylab("Pct.Vacant.Boarded (%)") +
    geom_rect(aes(xmin = -2, xmax = 18, ymin = 80, ymax = 102),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = 76, xmax = 102, ymin = -2, ymax = 22),
              fill="white",
              colour = "darkblue", 
              alpha=0.001,
              size = 0.3)

g1 <- prettify(g1)

# Plotting view 13 - POI 21 - 26
g2 <- ggplot(data_file, aes(x=PolicReqPerOffic * 100, y=PctFam2Par * 100, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint = 23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Police.Requests.Per.Officer") +
    ylab("Pct.Families.2.Parents (%)") +
    geom_rect(aes(xmin = 18, xmax = 42, ymin = -2, ymax = 20),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = -2, xmax = 100, ymin = 18, ymax = 36),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3)

g2 <- prettify(g2)

# Plotting view 95 - POI 50
g3 <- ggplot(data_file, aes(x=PctPolicMinor * 100, y=PctPolicWhite * 100, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint = 23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Pct.Police.Minority (%)") +
    ylab("Pct.Police.White (%)") +
    geom_rect(aes(xmin = -2, xmax = 22, ymin = -2, ymax = 100),
              fill="white",
              colour = "darkblue", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = 60, xmax = 80, ymin = -2, ymax = 100),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3)

g3 <- prettify(g3)

save_plots("ViolentCrimesExample1.pdf", g1, g2, g3, pdfwidth = 21, pdfheight=4.2)

################################
# Plots for deduplicated views #
################################
# Views 4 - POI 24, 41
g4 <- ggplot(data_file, aes(x=PctEmplManu * 100, y=PctEmplProfServ * 100, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint =23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Pct.Empl.Profes.Services (%)") + 
    ylab("Pct.Empl.Manual (%)") +
    geom_rect(aes(xmin = 33, xmax = 53, ymin = 26, ymax = 50),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = 20, xmax = 42, ymin = 31, ymax = 48),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3)

g4 <- prettify(g4)


# View 11 - POI 1 - 4
g5 <- ggplot(data_file, aes(x=pctWRetire * 100, y=PctUsePubTrans * 100, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint = 23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Pct.Retired (%)") + 
    ylab("Pct.Use.Public.Transports (%)") +
    geom_rect(aes(xmin = 27, xmax = 41, ymin = 78, ymax = 102),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = 6, xmax = 29, ymin = -2, ymax = 22),
              fill="white",
              colour = "darkblue", 
              alpha=0.001,
              size = 0.3)

g5 <- prettify(g5)

# View 8 - POI 37
g6 <- ggplot(data_file, aes(x=PctSameState85 * 100, y=PopDens, z=ViolentCrimesPerPop * 100)) +
    stat_summary_hex() +
    scale_fill_gradient2(midpoint = 23,
                         low = "darkblue", mid = "#EEEEEE", high = muted("red"),
                         name = "Violent Crimes (%)") +
    xlab("Pct.Recently.Moved (%)") + 
    ylab("Population.Density") +
    geom_rect(aes(xmin = 52, xmax = 74, ymin = 0.78, ymax = 1.02),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3) +
    geom_rect(aes(xmin = 63, xmax = 81, ymin = 0.94, ymax = 1.02),
              fill="white",
              colour = "darkred", 
              alpha=0.001,
              size = 0.3)

g6 <- prettify(g6)

save_plots("ViolentCrimesExample2.pdf", g4, g5, g6, pdfwidth = 21, pdfheight=4.2)


# library(foreign)
# o_data_file <- read.arff("/Users/thib/Data/Files/vibrations/meteo.arff")
# 
# vibra_cols <- sort(names(o_data_file))[51:113]
# pca <- prcomp(o_data_file[,vibra_cols],
#                  center = TRUE,
#                  scale. = TRUE) 
# vibra_cols_compressed <- predict(pca, o_data_file[,vibra_cols])[, 1:2]
# km_vibra_cols <- kmeans(vibra_cols_compressed, 16, nstart = 5)
# data_file_vibrid <- km_vibra_cols$cluster
# #rain <- rainbow(length(unique(vibra_cols_coded)))
# #plot(vibra_cols_compressed, col = rain[as.numeric(vibra_cols_coded)])
# 
# dimensions <- names(o_data_file)[!names(o_data_file) %in% vibra_cols]
# target <- "data_file_vibrid"
# data_file <- cbind(o_data_file[,dimensions], data_file_vibrid)
# names(data_file) <- c(dimensions, target)
# 
# groups <- generate_views(data_file, target)
# 
# plot_group <- function(i, group = NULL){
#     columns <- groups[[1]][[i]]$columns
#     subgroups <- groups[[2]][[i]]
#     
#     colors <- rep("#737373", nrow(data_file))
#     
#     the_groups <- if (is.null(group)) {
#         1:length(subgroups)
#     } else {
#         c(group)
#     }
#     
#     for (n in the_groups){
#         gp <- subgroups[[n]]
#         colors[gp$items] <- FEW_COLS[which(the_groups == n)]
#     }
#     parcoord(data_file[, columns], col = colors)
# }
# 
# plot_target <- function(i, group=1){
#     items <- groups[[2]][[i]][[group]]$items
#     non_items <- setdiff(1:nrow(vibra_cols_compressed), items)
#     plot(vibra_cols_compressed[non_items,], col = "#737373")
#     points(vibra_cols_compressed[items,], col = FEW_COLS[1])
# }
