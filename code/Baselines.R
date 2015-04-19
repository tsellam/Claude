library(e1071)
library(class)
source("Rlib/InfoTheory.R", chdir=TRUE)


#######
# 4S! #
#######
FourS <- function(data, target, 
                  jar_loc=paste0(getwd(), "/4S/4S.jar"),
                  logfun=NULL, outfun = NULL){
    
    if (!file.exists(jar_loc)) stop("JAR archive for 4S not found!")
    cat("Running 4S....\n")
        
    # Normalizes and gets stats
    cat("Preprocessing...")
    TIME <- proc.time()["elapsed"]
    colnames <- names(data)
    ncols <- ncol(data) - 1
    nrows <- nrow(data)
    prep_data <- scale(data)
    max <- max(prep_data)
    TIME_PREPARE <- proc.time()["elapsed"] - TIME
    
    # Writes to CSV
    cat("Converting...")
    write.table(prep_data, file="ztmp_data.csv", sep = ";",
                quote = FALSE, col.names = FALSE, row.names = FALSE)
    cat("Done\n")
    
    TIME <- proc.time()["elapsed"]
    # Runs the thing
    command <- paste(
        "java -Xmx4096m", 
        "-jar", jar_loc,
        "-FILE_INPUT ztmp_data.csv",
        "-FILE_G_OUTPUT ztmp_graph",
        "-FILE_C_OUTPUT ztmp_cliques_out",
        "-FILE_B_OUTPUT ztmp_cliques_b_out",
        "-FILE_N_OUTPUT ztmp_subspaces.out",
        "-FILE_RUNTIME_OUTPUT ztmp_runtime.out",
        "-NUM_POINTS", nrows,
        "-NUM_DIMS", ncols,
        "-MAX_VAL", max,
        "-METHOD 0",
        "-THTYPE 0",
        "-FACTOR 1")
    print(command)
    cat("Running Java 4S...")
    system(command, ignore.stdout = TRUE)
    cat("Done!..\n")
    
    cat("Reading..")
    df_subspaces_num <- read.csv("ztmp_subspaces.out", header=FALSE)
    subspaces_num <- apply(df_subspaces_num, 1, function(row) c(na.omit(row) + 1))
    if (!is.list(subspaces_num)) subspaces_num <- list(subspaces_num)
    
    cat("Evaluating..")
    view_columns <- lapply(subspaces_num, function(v) colnames[v])
    strengths <- sapply(view_columns, fast_joint_mutual_information, target, data)
    views <- lapply(1:length(view_columns), function(i){
        list(
            columns = view_columns[[i]],
            strength = strengths[i]
        )
    })
    TIME_EVAL <- proc.time()["elapsed"] - TIME
    
    cat("Getting runtime...")
    TIME_RUN <- scan("ztmp_runtime.out",  quiet = TRUE) / 1000
    
    if (!is.null(logfun))
        logfun("4S", "Time", TIME_EVAL + TIME_PREPARE)
    write_results("4S", views, outfun)
    
    cat("Done\n")
    views
}

###############
# Naive Bayes #
###############
preprocess_NB <- function(data, target){
    if (is.numeric(data[[target]]) && length(unique(data[[target]])) > 10) {
        data[,target] <- cut(data[,target], 10)
    } else {
        data[,target] <- factor(data[,target])                
    }
    content <- lapply(data, function(col){
        if (length(unique(col)) < 2) {
            NULL
        } else if (!class(col) %in% c("factor", "numeric")){
            factor(col)
        } else {
            col
        }
    })
    nulls <- sapply(content, is.null)
    cat("Eliminated the following columns:\n")
    print(names(data)[nulls])
    data.frame(content[!nulls])
}

score <- function(pred, truth, F1=TRUE){
    cl  <- sort(unique(truth))
    cll <- as.character(cl)
    truth <- factor(truth, levels = cl, labels = cll)
    pred  <- factor(pred,  levels = cl, labels = cll)
    tab <- table(pred, truth)
    
    accuracy <- sum(diag(tab)) / sum(tab)
    if (!F1) return(accuracy)
    
    class_pred_count <- apply(tab, 1, sum)
    class_true_count <- apply(tab, 2, sum)
    class_pred_hits  <- diag(tab)
    precision <- ifelse(class_pred_count > 0,
                        class_pred_hits / class_pred_count,
                        1)
    recall    <- ifelse(class_true_count > 0,
                        class_pred_hits / class_true_count,
                        1)
    F1 <- 2 * precision * recall / (precision + recall)
    
    if (length(class_true_count) < 3){
        mino_class<- which.min(class_true_count)
        avg_F1 <- F1[mino_class]
    } else {
        avg_F1 <- sum(F1 * class_true_count, na.rm = TRUE) / sum(class_true_count)
    }
    
    return(avg_F1)
}

NB_strength <- function(columns, target, data,
                        folds=5, F1 = TRUE){
    set <- data[,c(columns, target)]
    set <- na.omit(set)
    
    bin_width <- floor(nrow(set) / folds)
    folds <- lapply(1:folds, function(i){
        c((i - 1) * bin_width, i * bin_width - 1)
    })
    
    scores <- c()
    for (fold in folds){
        test  <- set[fold[1]:fold[2],]
        train <- set[-fold[1]:-fold[2],]
        
        nb_model <- naiveBayes(train[, columns, drop=FALSE], train[[target]], laplace = 1)
        nb_predict <- predict(nb_model, test[, columns, drop=FALSE], type = "class")
        if (length(nb_predict) < 1 )
            nb_predict <- rep(train[[target]][1], length(test[[target]]))
        scores <- c(scores, score(nb_predict, test[[target]], F1))
    }
    return(mean(scores, na.rm = TRUE))
}

search_exact_NB <- function(data, target_col, q, size_view, size_beam=NULL,
                            logfun=NULL, outfun = NULL){
    
    cat("Starting beam search with Naive Bayes\n")
    TIME <- proc.time()["elapsed"]
    
    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # Initialization and useful variables
    view_columns   <- list()
    view_NB_strengths <- c()
    
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
        if (length(cand_cols) < 1) break
        
        # Computes the strength of the view
        cat("Gets view strength...")
        cand_NB_strengths <- sapply(cand_cols, NB_strength, target, data)
        cand_NB_strengths[!is.finite(cand_NB_strengths)] <- 0
        
        # Applies pruning
        if (!is.null(size_beam)){
            # Gets the threhold
            all_strengths <- c(cand_NB_strengths, view_NB_strengths)
            min_pos <- min(size_beam, length(all_strengths))
            threshold <- sort(all_strengths, decreasing = TRUE)[min_pos]
            # Prunes
            to_prune <- (cand_NB_strengths < threshold)
            cat("Removes", sum(to_prune), "candidates outside beam\n")
            cand_cols <- cand_cols[!to_prune]
            cand_NB_strengths <- cand_NB_strengths[!to_prune]
        }
        
        # Appending
        cat("Adding", length(cand_cols), "new views\n")
        view_columns   <- c(view_columns, cand_cols)
        view_NB_strengths <- c(view_NB_strengths, cand_NB_strengths)
    }
    
    # Real Score Calculations
    data <- as.data.frame(lapply(data, as.numeric))
    view_strengths <- sapply(view_columns, fast_joint_mutual_information,
                             target_col, data)
    
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
        logfun("Wrap_NaiveBayes", "Time", TIME2)
    write_results("Wrap_NaiveBayes", views, outfun)
    return(views)
}

get_NB_score <- function(view_set, data, target, logfun, algo = NULL){
    for (i in 1:length(view_set)){
        view <- view_set[[i]]
        strength <- view$strength
        NB_strength <- NB_strength(view$columns, target, data, folds=10)
        if (is.null(algo)) {
            logfun(i, strength, "NaiveBayes", NB_strength)
        } else {
            logfun(algo, i, "NaiveBayes - F1", NB_strength)
        }
    }
}


#######
# kNN #
#######
preprocess_kNN <- function(data, target){
    if (is.numeric(data[[target]]) && length(unique(data[[target]])) > 10)
        data[,target] <- cut(data[,target], 10)
    content <- lapply(data, function(col){
        if (length(unique(col)) < 2) {
            NULL
        } else if (!class(col) %in% c("numeric")){
            as.numeric(factor(col))
        } else {
            col
        }
    })
    nulls <- sapply(content, is.null)
    cat("Eliminated the following columns:\n")
    print(names(data)[nulls])
    data.frame(content[!nulls])
}


kNN_strength <- function(columns, target, data,
                        folds=5, F1 = TRUE){
    set <- data[,c(columns, target)]
    set <- na.omit(set)
    
    bin_width <- floor(nrow(set) / folds)
    folds <- lapply(1:folds, function(i){
        c((i - 1) * bin_width, i * bin_width - 1)
    })
    
    scores <- c()
    for (fold in folds){
        test  <- set[fold[1]:fold[2],]
        train <- set[-fold[1]:-fold[2],]
        
        knn_predict <- c()
        try({
            knn_predict <- knn(train[, columns, drop=FALSE],
                               test[, columns, drop=FALSE], 
                               train[[target]],
                               k = 5,
                               use.all = FALSE)
        }, silent = TRUE)
        
        if (length(knn_predict) > 1) {
            scores <- c(scores, score(knn_predict, test[[target]], F1))
        }
    }
    
    out <- if(length(scores) > 1){
            mean(scores, na.rm = TRUE)
        } else {
            NA
        }
        
    return(out)
}

search_exact_kNN <- function(data, target_col, q, size_view, size_beam=NULL,
                            logfun=NULL, outfun = NULL){
    
    cat("Starting beam search with kNN\n")
    TIME <- proc.time()["elapsed"]
    
    # Basic stuff
    dim_names <- names(data)[!names(data) == target_col]
    size_view <- min(size_view, length(dim_names))
    
    # Initialization and useful variables
    view_columns   <- list()
    view_kNN_strengths <- c()
    
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
        if (length(cand_cols) < 1) break
        
        # Computes the strength of the view
        cat("Gets view strength...")
        cand_kNN_strengths <- sapply(cand_cols, kNN_strength, target, data)
        cand_kNN_strengths[is.na(cand_kNN_strengths)] <- 0
        
        # Applies pruning
        if (!is.null(size_beam)){
            # Gets the threhold
            all_strengths <- c(cand_kNN_strengths, view_kNN_strengths)
            min_pos <- min(size_beam, length(all_strengths))
            threshold <- sort(all_strengths, decreasing = TRUE)[min_pos]
            # Prunes
            to_prune <- (cand_kNN_strengths < threshold)
            cat("Removes", sum(to_prune), "candidates outside beam\n")
            cand_cols <- cand_cols[!to_prune]
            cand_kNN_strengths <- cand_kNN_strengths[!to_prune]
        }
        
        # Appending
        cat("Adding", length(cand_cols), "new views\n")
        view_columns   <- c(view_columns, cand_cols)
        view_kNN_strengths <- c(view_kNN_strengths, cand_kNN_strengths)
    }
    
    # Real Score Calculations
    data <- as.data.frame(lapply(data, as.numeric))
    view_strengths <- sapply(view_columns, fast_joint_mutual_information,
                             target_col, data)
    
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
        logfun("Wrap_kNN", "Time", TIME2)
    write_results("Wrap_kNN", views, outfun)
    return(views)
}


get_kNN_score <- function(view_set, data, target, logfun, algo = NULL){
    for (i in 1:length(view_set)){
        view <- view_set[[i]]
        strength <- view$strength
        kNN_strength <- kNN_strength(view$columns, target, data, folds=10)
        if (is.null(algo)) {
            logfun(i, strength, "kNN", kNN_strength)
        } else {
            logfun(algo, i, "kNN - F1", kNN_strength)
        }
    }
}

###################
# DIVERSITY SCORE #
###################
get_diversity_score <- function(view_set, data, target, logfun, algo){
    all_cols <- unlist(sapply(view_set, function(v) v$columns))
    score <- length(unique(all_cols))
    logfun(algo, 0, "Diversity", score)
}