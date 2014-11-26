library(foreign)
source("InfoTheory.R")

##############
# Primitives #
##############
preprocess <- function(table){
    content <- lapply(table, function(col){
        if (length(unique(col)) < 32)   return(col) 
        if (is.factor(col))             return(NULL)
        if (is.numeric(col))            return(cut(col, 32))
        return(col)
    })
    
    nulls <- sapply(content, is.null)
    cat("Eliminated the following columns:\n")
    print(names(table)[nulls])
    data.frame(content[!nulls])
}


# Utils
get_next_items <- function(x, vect){
    pos <- which(vect == x)
    
    if (length(pos) != 1){
        warning("Duplicate or ghost column name detected!")
        pos <- pos[1]
    } 
    
    if (pos == length(vect)) return(NULL)
    vect[(pos+1):length(vect)]
}

decompose_sum <- function(total, max){
    n_max <- floor(total / max)
    remain <- total %% max
    elts <- c(remain, rep(max, n_max))
    elts[elts!=0]
}

##########
# Kernel #
##########
# Method 1: Exact-Exhaustive
maximum_improvement <- function(level_cur, level_max, max_joint_entropies){
    n_slack <- level_max - level_cur
    max_slack <- length(max_joint_entropies)
    
    # Easy cases
    if (n_slack == 0){
        return(0)
    } else if (n_slack <= max_slack){
        return(max_joint_entropies[n_slack])
    }
    
    slacks <- decompose_sum(n_slack, max_slack)
    sum(max_joint_entropies[slacks])
}

search_exact <- function(data, target_col, q, size_view){
    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # Initialization and useful variables
    views <- list()
    max_joint_entropies <- c()
    target_entropy <- entropy(target_col, data)
    
    # Main loop
    for (level in 1:size_view){
        cat("**** Running search for level", level, "\n")
        
        # Generates new candidates from the old ones
        if (level > 1) {
            new_cand_cols <- lapply(cand_cols, function(cand){
                next_cols <- get_next_items(cand[length(cand)], dim_names)
                lapply(next_cols, function(col) c(col, cand))
            })
            cand_cols <- unlist(new_cand_cols, recursive = FALSE)
        } else {
            cand_cols <- as.list(dim_names)
        }
        cat("Going through", length(cand_cols), "candidates\n")
        
        # Computes the strength of the view
        cat("Gets joint entropies...")
        joint_entropies <- sapply(cand_cols, joint_entropy, data)
        cat("done\nGets conditional joint entropies...")
        cond_joint_entropies <- sapply(cand_cols, cond_joint_entropy, target, data)
        cat("done\n")
        cand_strengths <- joint_entropies - cond_joint_entropies
        max_joint_entropies <- c(max_joint_entropies, max(joint_entropies))
        
        # Applies pruning
        all_scores <- if (length(views) > 1){
            c(cand_strengths, sapply(views, function(v) v$strength))
        } else {
            cand_strengths
        }
        min_pos <- min(q, length(all_scores))
        threshold <- sort(all_scores, decreasing = TRUE)[min_pos]
        # Strategy 1: dimension-based
        max_delta <- maximum_improvement(level, size_view, max_joint_entropies)
        prune_dim <- (cand_strengths + max_delta < threshold)
        cat("Removes", sum(prune_dim), "candidates based on dimension\n")
        # Strategy 2: target-based
        prune_target <- (cand_strengths + target_entropy < threshold)
        cat("Removes", sum(prune_target), "candidates based on target\n")
        
        # Wrapping up
        to_keep        <- !prune_dim & !prune_target
        cand_cols      <- cand_cols[to_keep]
        cand_strengths <- cand_strengths[to_keep]
        cat("Adding", length(cand_cols), "new views\n")
        new_views <- lapply(1:length(cand_cols), function(i){
            list(
                columns  = cand_cols[[i]],
                strength = cand_strengths[i]
            )
        })
        views <- c(new_views, views)
    }
    
    return(views)
}

# Method 2: Exact-Pruning

# Method 3: Approx-Exhaustive

# Method 4: Approx-Pruning

###############
# Experiments #
###############
# Load data
cereals <- read.delim("~/Projects/TurboGroups/data/cereal.csv")
communities  <- read.arff("~/Projects/TurboGroups/data/communities.arff")
communities <- data.frame(lapply(communities, function(col){
    if (is.character(col)) factor(col) else col
}))
census <- read.csv("~/Projects/TurboGroups/data/census.csv", na.strings="?")
census <- select(census, -fnlwgt)
#census <- census[, c(names(census)[1:5], "salary")]

# Preprocessing
data <- preprocess(census)
target <- "salary"

# Runs the search
exact <- search_exact(data, target, q = 5, size_view = 4)

# seq <- order(sapply(exact, function(v) v$strength), decreasing = TRUE)
# exact <- exact[seq]
# scores <- sapply(exact, function(v) v$strength)
# columns <- sapply(exact, function(v) paste0(v$columns, collapse=","))
# hist(scores)
# print("Best")
# print(columns[1:10])
# print("Worse")
# print(columns[(length(columns) - 10):length(columns)])