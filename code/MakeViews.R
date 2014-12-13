library(MASS)
library(dplyr)
source("StrongViews.R", chdir = TRUE)
source("SubgroupDiscovery.R", chdir = TRUE)

FEW_COLS = c(
  green="#008C48",
  blue="#185AA9",
  orange="#F47D23",
  red="#EE2E2F",
  purple="#662C91",
  maroon="#A21D21",
  magenta="#B43894"
)


write_results <- function(algo, POIs, outfun){
    if (is.null(outfun)) return()
    
    POIs <- unlist(POIs, recursive = FALSE)
    for (i in 1:length(POIs)){
        POI <- POIs[[i]]
        outfun(algo, i, POI$score, POI$description)
    }
}

generate_views <- function(data, target_col, nbins_target = 2,
                           q=250, size_view=5, size_beam_q=300, pessimistic=TRUE,
                           min_freq = 0.05, k = 1, size_beam_k = 10, nbins = 5, levels = 3,
                           just_POIs = FALSE, logfun = NULL, outfun = NULL){
    
    
    TIME <- proc.time()["elapsed"]
    ALGO <- paste0("Just_POIs_", just_POIs)
    
    # First, get views
    if (!just_POIs) {
        cat("Preprocessing for view selection\n")
        preproc_for_views <- preprocess(data, target_col, nbins_target)
        
        cat("Actual view selection\n")
        views <- search_approx(preproc_for_views, target_col,
                       q, size_view, size_beam_q, pessimistic)
    } else {
        views <- list(list(
            columns = names(data)[!names(data) == target],
            strength = NA
        ))
    }
    TIMEV <- proc.time()["elapsed"]
    if (!is.null(logfun)) 
        logfun(ALGO, "Views", TIMEV - TIME)
    
    # Then gets POIs
    cat("Preprocessing for POIs\n")
    preproc_for_POIs <- preprocess_for_sd(data, target_col, nbins_target)
    
    cat("Getting POIs\n")
    POIs <- lapply(views, function(view){
        
        cat("Doing view:", paste(view$columns), "\n")
        subgroup_discovery(preproc_for_POIs, view$columns, target,
                           min_freq, k, size_beam_k,  nbins, levels)

    })
    
    TIMEP <- proc.time()["elapsed"]
    if (!is.null(logfun)) 
        logfun(ALGO, "POIs", TIMEP - TIMEV)
    
    TIME2 <- proc.time()["elapsed"] - TIME
    if (!is.null(logfun)) logfun(ALGO, "Time", TIME2)
    
    if (!is.null(outfun)) write_results(ALGO, POIs, outfun)
    
    # Plots
    return(list(views, POIs))
}
# 
# # Experiments
# library(foreign)
# 
# data_file <- read.arff("~/Projects/TurboGroups/data/communities.arff")
# data_file <- select(data_file, -community)
# 
# target <- "ViolentCrimesPerPop"
# #print(system.time(generate_views(data_file, target, just_POIs = TRUE)))
# print(generate_views(data_file, target, 
#                      logfun = function(...) print(paste(...)),
#                      outfun = function(...) print(paste(...))
#                      )
#       )

# Experiments
library(foreign)
o_data_file <- read.arff("/Users/thib/Data/Files/vibrations/meteo.arff")

vibra_cols <- sort(names(o_data_file))[51:113]
pca <- prcomp(o_data_file[,vibra_cols],
                 center = TRUE,
                 scale. = TRUE) 
vibra_cols_compressed <- predict(pca, o_data_file[,vibra_cols])[, 1:2]
km_vibra_cols <- kmeans(vibra_cols_compressed, 16, nstart = 5)
data_file_vibrid <- km_vibra_cols$cluster
#rain <- rainbow(length(unique(vibra_cols_coded)))
#plot(vibra_cols_compressed, col = rain[as.numeric(vibra_cols_coded)])

dimensions <- names(o_data_file)[!names(o_data_file) %in% vibra_cols]
target <- "data_file_vibrid"
data_file <- cbind(o_data_file[,dimensions], data_file_vibrid)
names(data_file) <- c(dimensions, target)

groups <- generate_views(data_file, target)

plot_group <- function(i, group = NULL){
    columns <- groups[[1]][[i]]$columns
    subgroups <- groups[[2]][[i]]
    
    colors <- rep("#737373", nrow(data_file))
    
    the_groups <- if (is.null(group)) {
        1:length(subgroups)
    } else {
        c(group)
    }
    
    for (n in the_groups){
        gp <- subgroups[[n]]
        colors[gp$items] <- FEW_COLS[which(the_groups == n)]
    }
    parcoord(data_file[, columns], col = colors)
}

plot_target <- function(i, group=1){
    items <- groups[[2]][[i]][[group]]$items
    non_items <- setdiff(1:nrow(vibra_cols_compressed), items)
    plot(vibra_cols_compressed[non_items,], col = "#737373")
    points(vibra_cols_compressed[items,], col = FEW_COLS[1])
}

plot_group(1)
plot_target(2)
#plot(vibra_cols_compressed[groups[[2]][[5]][[1]]$items,])