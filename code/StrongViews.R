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
    if (length(pos) > 1){
        warning("Duplicate columns name detected!")
        pos <- pos[1]
    }
    vect[pos:length(vect)]
    
}

##########
# Kernel #
##########
# Method 1: Exact-Exhaustive
upper_bound <- function(view, entropies, add_levels){
    name_columns_left <- names(entropies)[!names(entropies) %in% view]
    columns_left <- entropies[name_columns_left]
    
    sum(columns_left[1:add_levels])
}

search_exact <- function(data, target_col, q, n_columns){
    # Basic stuff
    dim_cols <- names(data)[!names(data) == target_col]
    n_columns <- min(n_columns, length(dim_cols))
    
    # Calculates the entropy of each column
    cat("Computes entropy for each column\n")
    entropies <- sapply(dim_cols, function(c) entropy(c, data))
    names(entropies) <- dim_cols
    entropies <- sort(entropies, decreasing = TRUE)
    
    # First, initialize with one variable views
    cat("Initializes...\n")
    views <- lapply(dim_cols, function(col){
        list(
            columns  = col,
            strength = mutual_information(col, target_col, data)
        )
    })
    if (n_columns < 2) return(views)
    
    # Higher dependencies
    for (level in 2:n_columns){
        cat("Search for level:", level, "\n")
        
        # Creates the permutations
        cat("Creates combinations\n")
        new_columns <- lapply(views, function(view){
            old_cols <- view$columns
            get_next_items(old_cols[length(old_cols)], dim_cols)
        })
        old_views <- rep(views, sapply(new_columns, length))
        new_columns <- unlist(new_columns)
        
        # Prunes non promising candidates
        scores <- sapply(old_views, function(v) v$strength)
        threshold <- sort(scores, decreasing = TRUE)[min(q, length(scores))]
        cat("Computes lower bounds - Threshold:", threshold, "\n")
        upper_bounds <- sapply(old_views, upper_bound,
                               entropies, n_columns - level + 1)
        
        # Runs the computations
        views <- lapply(1:length(new_columns), function(i){
            # Gets column names
            old_view <- old_views[[i]]
            new_col  <- new_columns[[i]]
            # Gets the data and computes the strenth gain
            if (new_col %in% old_view$columns) return(old_view)  
            strength_gain <- cond_mutual_information(new_col, target_col, 
                                                     old_view$columns, data)
            new_strength <- old_view$strength + strength_gain
            # Output
            list(
                columns = c(old_view$columns, new_col),
                strength = new_strength
            )
        })  
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
#census <- census[, c(names(census)[1:10], "salary")]

# Preprocessing
data <- preprocess(census)
target <- "salary"

# Runs the search
exact <- search_exact(data, target, q = 50, n = 5)

seq <- order(sapply(exact, function(v) v$strength), decreasing = TRUE)
exact <- exact[seq]
scores <- sapply(exact, function(v) v$strength)
columns <- sapply(exact, function(v) paste0(v$columns, collapse=","))
hist(scores)
print("Best")
print(columns[1:10])
print("Worse")
print(columns[(length(columns) - 10):length(columns)])