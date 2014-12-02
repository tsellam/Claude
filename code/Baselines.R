library(e1071)

preprocess_NB <- function(data, target){
    if (is.numeric(data[[target]]) && length(unique(t_col)) > 5)
        data[,target] <- cut(data[,target], 5)
    data <- lapply(data, function(col){
        if (!class(col) %in% c("factor", "numeric")){
            factor(col)
        } else{
            col
        }
    })
    as.data.frame(data)
}

score <- function(pred, truth, F1=TRUE){
    tab <- table(pred, truth)
    accuracy <- sum(diag(tab)) / sum(tab)
    if (!F1) return(accuracy)
    
    class_pred_count <- apply(tab, 1, sum)
    class_true_count <- apply(tab, 2, sum)
    class_pred_hits  <- diag(tab)
    precision <- class_pred_hits / class_pred_count
    recall    <- class_pred_hits / class_true_count
    F1 <- 2 * precision * recall / (precision + recall)
    
    avg_F1 <- sum(F1 * class_true_count, na.rm = TRUE) / sum(class_true_count)
    return(avg_F1)
}
predict <- sample(c(0, 1), 20, replace=T)
true <- sample(c(0, 1), 20, replace=T)
score(predict, true)

NB_strength <- function(columns, target, data,
                        folds=2, F1 = TRUE){
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
        data.matrix(test[, columns, drop=FALSE])
        
        nb_predict <- predict(nb_model, test[, columns, drop=FALSE], type = "class")
        if (length(nb_predict) < 1 ) stop()
        scores <- c(scores, score(nb_predict, test[[target]], F1))
    }
    
    return(mean(scores))
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
        cat("Gets view strength...")
        cand_strengths <- sapply(cand_cols, NB_strength, target, data)
        
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
        logfun("Wrap_NaiveBayes", "Time", TIME2)
    write_results("Wrap_NaiveBayes", views, outfun)
    
    return(views)
}


get_NB_score <- function(view_set, data, target, logfun){
    for (i in 1:length(view_set)){
        view <- view_set[[i]]
        strength <- view$strength
        NB_strength <- NB_strength(view$columns, target, data, folds=10)
        logfun(i, strength, NB_strength)
    }
}
