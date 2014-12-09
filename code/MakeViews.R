source("StrongViews.R", chdir = TRUE)
source("SubgroupDiscovery.R", chdir = TRUE)

write_results <- function(algo, POIs, outfun){
    if (is.null(outfun)) return()
    
    POIs <- unlist(POIs, recursive = FALSE)
    for (i in 1:length(POIs)){
        POI <- POIs[[i]]
        outfun(algo, i, POI$score, POI$description)
    }
}

generate_views <- function(data, target_col, nbins_target = 2,
                           q=15, size_view=3, size_beam_q=25, pessimistic=TRUE,
                           min_freq = 0.05, k = 3, size_beam_k = 3, nbins = 5, levels = 3,
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
    return(POIs)
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