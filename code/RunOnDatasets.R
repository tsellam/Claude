library(foreign)
source("MakeViews.R", chdir = TRUE)

#################
# Crime Dataset #
#################
data_file <- read.arff("~/Projects/TurboGroups/data/communities.arff")
data_file <- select(data_file, -community)

# Execution
target <- "ViolentCrimesPerPop"


views <- generate_views(data_file, target, 
                        nbins_target = 10,
                        q=100, size_view=3, size_beam_q=50, dup_factor=500,
                        min_freq = 0.05, k=5, size_beam_k=10, nbins=5, levels=3,
                        logfun = function(...) print(paste(...)),
                        outfun = function(...) print(paste(...))
)

# View names
for (v in views[[1]]){
    print(sort(v$columns))
}

# Plotting


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
