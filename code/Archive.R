old_search_exact <- function(data, target_col, q, n_columns){
    # Basic stuff
    dim_cols <- names(data)[!names(data) == target_col]
    n_columns <- min(n_columns, length(dim_cols))
    
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

upper_bound <- function(view, entropies, add_levels){
    name_columns_left <- names(entropies)[!names(entropies) %in% view]
    columns_left <- entropies[name_columns_left]
    
    sum(columns_left[1:add_levels])
}