library(foreign)
library(igraph)
library(tidyr)
library(dplyr)
source("Rlib/InfoTheory.R", chdir=TRUE)

##############
# Primitives #
##############
preprocess <- function(table, target, nbins_target = 10){
    cat("Preprocessing in progress...")
    
    # Special case: the target column
    t_col <- table[,target]
    if (is.numeric(t_col) && length(unique(t_col)) > nbins_target ){
        table[,target] <- cut(t_col, nbins_target)
    } else {
        table[,target] <- factor(t_col)
    }
    
    # The rest
    content <- lapply(table, function(col){
        new_col <- if (is.numeric(col) && length(unique(col)) > 1){
                        cut(col, ceiling(log2(length(col))) + 1)
                    } else if ( (is.factor(col) || is.character(col)) &&
                                    length(unique(col)) > 1 &&
                                    length(unique(col)) < 32 ){
                        factor(col)
                    } else {
                        print("Excluding")
                        NULL
                    }
        if (!is.null(new_col)) as.numeric(new_col)
    })
    names(content) <- names(table)
    
    nulls <- sapply(content, is.null)
    cat("Done. Eliminated the following columns:\n")
    print(names(table)[nulls])
    data.frame(content[!nulls])
}


# Utils
get_next_items <- function(x, vect){c
    pos <- which(vect == x)
    
    if (length(pos) != 1){
        warning("Duplicate or ghost column name detected!")
        pos <- pos[1]
    } 
    
    if (pos == length(vect)) return(NULL)
    vect[(pos+1):length(vect)]
}


write_results <- function(algo, view_set, logfun){
    if (is.null(logfun)) return()
    for (i in 1:length(view_set)){
        view <- view_set[[i]]
        logfun(algo, i, "Strength", view$strength)
        logfun(algo, i, "Columns",  paste0(view$columns, collapse = ", "))
    }
}

##########
# Kernel #
##########
# Method 1: Exact Values
search_exact <- function(data, target_col, q, size_view, size_beam=NULL,
                         logfun=NULL, outfun = NULL){
    
    cat("Starting exact search\n")
    TIME <- proc.time()["elapsed"]

    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # Initialization and useful variables
    view_columns   <- list()
    view_strengths <- c()
    
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
        cand_strengths <- sapply(cand_cols, fast_joint_mutual_information, target, data)
        
        # Applies pruning
        if (!is.null(size_beam)){
            # Gets the threhold
            all_strengths <- c(cand_strengths, view_strengths)
            min_pos <- min(size_beam, length(all_strengths))
            threshold <- sort(all_strengths, decreasing = TRUE)[min_pos]
            # Prunes
            to_prune <- (cand_strengths < threshold)
            cat("Removes", sum(to_prune), "candidates outside beam\n")
            cand_cols <- cand_cols[!to_prune]
            cand_strengths <- cand_strengths[!to_prune]
        }
        
        # Appending
        cat("Adding", length(cand_cols), "new views\n")
        view_columns   <- c(view_columns, cand_cols)
        view_strengths <- c(view_strengths, cand_strengths)
    }
    
    # Final filtering and wrapping up
    out_order <- order(view_strengths, decreasing = TRUE)
    out_order <- out_order[1:min(q, length(out_order))]
    views <- lapply(out_order, function(i){
        list(
            columns  = view_columns[[i]],
            strength = view_strengths[i]
        )
    })
    
    TIME2 <- proc.time()["elapsed"] - TIME
    if (!is.null(logfun))
        logfun("Exhaustive", "Time", TIME2)
    write_results("Exhaustive", views, outfun)
    
    return(views)
}



# Method 2: Approx
filter_views <- function(views, maxs=NULL){
    scores <- sapply(views, function(v) v$strength)
    out_order <- order(scores, decreasing = T)
    if (!is.null(maxs)){
        maxs <- min(maxs, length(views))
        out_order <- out_order[1:maxs]
    }
    views <- views[out_order]    
}

flush_views <- function(df, views = list(), size_beam=NULL){
    # Generates new view objects
    col_colnames <- grep("column*", names(df), value=TRUE)
    new_views <- lapply(1:nrow(df), function(i){
        list(
            columns =  as.character(df[i,col_colnames]),
            strength = as.numeric(df[i,"strength"])
        )
    })
    # Appends, filters, returns
    views <- c(views, new_views)
    views <- filter_views(views, size_beam)
    return(views)
}

compute_graph <- function(first_level, second_level, pessimistic = TRUE){
    
    regular <- second_level %>%
                    select(column1, column2, strength)
    
    inversed <- second_level %>%
                    mutate(tmp = column2) %>%
                    select(column2 = column1, column1 = tmp, strength)
    
    second_level <- rbind(regular,inversed)
    
    # Joins on the left side
    graph <- second_level %>%
                    inner_join(first_level, by = c("column1" = "column1")) %>%
                    mutate(strength = strength.x - strength.y) %>%
                    select(column1, column2, delta=strength)                
    
    return(graph)
}

generate_ids <- function(...){
    series <- cbind(..., deparse.level = 0)
    sapply(1:nrow(series), function(i){
        paste(sort(series[i,]), collapse="..")
    })
}

search_approx <- function(data, target_col, q=NULL, size_view, 
                          size_beam=NULL, pessimistic=TRUE,
                          logfun=NULL, outfun = NULL){
    
    cat("Starting approximate search\n")
    TIME <- proc.time()["elapsed"]
    
    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # First iterations are classic
    views <- list()
    first_level <- data.frame()
    second_level <- data.frame()
    for (i in 1:min(size_view, 2)){
        cat("*** Computing level", i, "... ")
        
        if (i == 1) {
            candidate_cols <- dim_names
        } else {
            candidate_cols <- first_level[["column1"]]
        }
        combs <- combn(candidate_cols, i)
        cat("Getting strength for", length(combs), "views...")
        strengths <- apply(combs, 2, fast_joint_mutual_information, target, data)
        
        views_df <- as.data.frame(t(combs), stringsAsFactors = F)
        names(views_df) <- paste0("column", 1:i)
        views_df[["strength"]] <- strengths
        views_df <- filter(views_df, row_number(desc(strength)) <= size_beam)
        
        views <- flush_views(views_df, views)
        if (i == 1) {
            first_level <- views_df
        } else {
            second_level <- views_df
        }
        
        cat("Done\n")
    }
    if (size_view <= 2) {
        views <- filter_views(views, q)
        TIME2 <- proc.time()["elapsed"] - TIME
        if (!is.null(logfun)){
            logfun("Approximative", "Time-Graph", 0)
            logfun("Approximative", "Time-Search", 0)
            logfun("Approximative", "Time-PostProcess", 0)
            logfun("Approximative", "Time", TIME2)
        }
        write_results("Approximative", views, outfun)
        return(views)
    }
        
    # Builds the graph
    cat("*** Computing graph...")
    graph <- compute_graph(first_level, second_level)
    cat(" Done\n")
    TIMEG <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Approximative", "Time-Graph", TIMEG - TIME)
    
    # Generates an id and ship to the main loop
    candidates <- mutate(second_level, id = generate_ids(column1, column2))
    for (level in 3:size_view){
        cat("*** Computing level", level, "... ")
        
        # Pivots
        piv_candidates <- candidates %>%
                        gather(col_index, col_name, matches("column*"))
        
        # gets neighbors
        neighbors <- piv_candidates %>%
                    inner_join(graph, by = c("col_name" = "column1")) %>%
                    anti_join(piv_candidates, 
                              by = c("id" = "id", "column2" = "col_name")) %>%
                    select(id, new_column = column2, delta)
        
        # picks a delta
        neighbors <- neighbors %>%
                     group_by(id, new_column) %>%
                     filter(delta == ifelse(pessimistic, min(delta), max(delta)))
        
        # joins back on the original data and generates ids
        old_colnames <- grep("column*", names(candidates), value=TRUE)
        new_colname  <- paste0("column", length(old_colnames) + 1)
        pasted_colnames <- paste0(old_colnames, collapse=",")
        id_generator <- as.formula(paste0("~generate_ids(", pasted_colnames, ",new_column)"))
        
        candidates <- candidates %>%
                        inner_join(neighbors, by = c("id" = "id")) %>%                    
                        mutate_(id = id_generator) %>%
                        mutate_(.dots = setNames(list("new_column"), new_colname)) %>%
                        mutate(strength = strength + delta) %>%
                        select(-delta, -new_column) 
        
        # dedpuplicates
        candidates <- candidates %>%
                        group_by(id) %>%
                        filter(1 == if (pessimistic){
                            row_number(strength) 
                            } else { 
                                row_number(desc(strength))
                            }) %>% ungroup
        
        # prunes and flushes
        if (!is.null(size_beam))
            candidates <- candidates %>% 
                          filter(row_number(desc(strength)) <= size_beam)
        views <- flush_views(candidates, views, size_beam)
        cat("Done\n")
    }
    TIMES <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Approximative", "Time-Search", TIMES - TIMEG)
    
    # Final calculations
    cat("Finalizes\n")
    view_columns <- lapply(views, function(v) v$columns)
    strengths <- sapply(view_columns, fast_joint_mutual_information, target, data)
    views <- lapply(1:length(view_columns), function(i){
        list(
            columns = view_columns[[i]],
            strength = strengths[i]
        )
    })
    
    views <- filter_views(views, q)
    
    TIMEP <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Approximative", "Time-PostProcess", TIMEP - TIMES)
    TIME2 <- proc.time()["elapsed"] - TIME
    if (!is.null(logfun)) logfun("Approximative", "Time", TIME2)
    write_results("Approximative", views, outfun)
    
    return(views)
}


# method 3: clique based
search_cliques <- function(data, target_col, q=NULL, size_view, size_beam,
                           logfun=NULL, outfun = NULL){
    cat("Starting clique search\n")
    TIME <- proc.time()["elapsed"]
    
    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # Building graph
    cat("*** Computing graph... ")
    combs <- combn(dim_names, 2)
    strengths  <- apply(combs, 2, fast_joint_mutual_information, target, data)
    graph <- data.frame(column1 = combs[1,],
                      column2 = combs[2,],
                      weight  = strengths,
                      stringsAsFactors = F)
    graph <- graph %>%
                arrange(desc(weight)) %>%
                slice(1:size_beam) %>%
                select(column1, column2, weight) %>% data.frame
    cat("done\n")
    TIMEG <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Clique", "Time-Graph", TIMEG - TIME)
    
    cat ("*** Searching for cliques in graph size ", nrow(graph), "...")
    # Gets cliques
    ig <- graph.data.frame(graph, directed=FALSE, vertices=NULL)
    cliq <- maximal.cliques(ig)
    cat(length(cliq), "max cliques found ...")
    
    # Breaks cliques down
    cat("expanding...")
    cliq <- lapply(cliq, function(cli) {
        if (length(cli) <= size_view) return(list(cli))
        subcliq <- combn(cli, size_view)
        subcliq <- lapply(1:ncol(subcliq), function(j) as.numeric(subcliq[,j]))
    })
    cliq <- unlist(cliq, recursive = FALSE)
    cat(length(cliq), "cliques derived")
    cat(" done\n")
    
    TIMES <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Clique", "Time-Search", TIMES - TIMEG)
    
    # Final calculationsl
     cat("*** Finalizes\n")
     view_columns <- lapply(cliq, function(c) dim_names[c])
     strengths <- sapply(view_columns, fast_joint_mutual_information, target, data)
     views <- lapply(1:length(view_columns), function(i){
         list(
             columns = view_columns[[i]],
             strength = strengths[i]
         )
     })
    
    views <- filter_views(views, q)
    
    TIMEP <- proc.time()["elapsed"]
    if (!is.null(logfun)) logfun("Clique", "Time-PostProcess", TIMEP - TIMES)
    TIME2 <- proc.time()["elapsed"] - TIME
    if (!is.null(logfun)) logfun("Clique", "Time", TIME2)
    write_results("Clique", views, outfun)
    
    return(views)
}

###############
# Experiments #
###############
# # Load data
# cereals <- read.delim("~/Projects/TurboGroups/data/cereal.csv")
# communities  <- read.arff("~/Projects/TurboGroups/data/communities.arff")
# census <- read.csv("~/Projects/TurboGroups/data/census.csv", na.strings="?")
# census <- select(census, -fnlwgt)
# 
# # Preprocessing
# # data <- communities
# # target <- "ViolentCrimesPerPop"
# #data <- rbind(data,)
# #target <- "salary"
# #data <- preprocess(census, target)
# Runs the search
# clean_data <- preprocess(data, target)
#  
# time <- Sys.time()
# beamed <- search_exact(clean_data, target, q = 25, size_view = 4, size_beam = 15)
# print(Sys.time() - time) ; time <- Sys.time(); cat("\n")
# 
# approx <- search_approx(clean_data, target, q = 25, size_view = 4, size_beam = 15)
# print(Sys.time() - time) ; time <- Sys.time(); cat("\n")
# 
# clique_approx <- search_cliques(data, target, q = 25, size_view = 4, top_threshold = 0.05)
# print(Sys.time() - time) ; time <- Sys.time(); cat("\n")
# 
# clean_data <- preprocess_NB(data, target)
# res <- search_exact_NB(clean_data, target, q = 5, size_view = 4, size_beam = 10)
