source("Rlib/InfoTheory.R", chdir=TRUE)
library(tidyr)
library(dplyr)
library(ggplot2)

#########
# Utils #
#########
preprocess_for_sd <- function(data_file, target, nbins_target = 2){
    
    dimensions <- names(data_file)[names(data_file) != target]
    
    if (is.numeric(data_file[[target]]))
        data_file[[target]] <- cut(data_file[[target]], nbins_target)
    
    for(colname in dimensions){
        
        col <- data_file[[colname]]
        if (length(unique(col)) < 2)
            data_file[[colname]] <- NULL
        if (is.numeric(col)) next
        if (is.factor(col)) next
        if (is.character(col))
            data_file[[colname]] <- factor(col)
    }
    
    data_file
    
}

plot_subgroups <- function(groups, data_file){
    pdf("Subgroups.pdf")
    for (group in groups){
        pl <- qplot(ViolentCrimesPerPop,
                    data = data_file %>% slice(group$items),
                    main = group$description)
        print(pl)
    }
    dev.off()
}

##################
# Core algorithm #
##################
filter_refinments <- function(refinments, maxs=NULL){
    scores <- as.numeric(sapply(refinments, function(v) v$score))
    out_order <- order(scores, decreasing = T)
    if (!is.null(maxs)){
        maxs <- min(maxs, length(refinments))
        out_order <- out_order[1:maxs]
    }
    refinments <- refinments[out_order]    
}

flush_refinments <- function(df, refinments = list(), size_beam=NULL){
    # Generates new refinment objects
    new_refinments <- lapply(1:nrow(df), function(i){
        new_predicate <- paste0(df$column[[i]], ": ", df$label[[i]])
        description <- paste0(df$root_description[[i]], new_predicate, sed = " - ")
        list(
            description =  description,
            items = df[i,"items"][[1]][[1]],
            score = as.numeric(df[i,"score"])
        )
    })
    
    # Appends, filters, returns
    refinments <- c(refinments, new_refinments)
    refinments <- filter_refinments(refinments, size_beam)
    return(refinments)
}

bin_col <- function(var, nbins){
    if (is.factor(var)) return(as.character(var))
    if (is.numeric(var)) return(as.character(cut(var, nbins)))
    stop("SD only accepts factors or numerics")
}

subgroup_discovery <- function(df, cols, target,
                               min_freq = 0.05, k = 25, 
                               size_beam = 25,  nbins = 5,
                               levels = 3){

    if (!is.data.frame(df)) stop("Please use a valid data frame.\n")
    
    orig_cols <- cols
    cols <- cols[cols %in% names(df)]
    if (length(cols) > length(orig_cols)) warning("Dropped some columns")
    
    if (!is.data.frame(df) ||
            !all(sapply(df[,cols], function(col) {
                is.numeric(col) ||  is.factor(col)
            }))
        )
        stop("Incorrect input data, make sure your input is a data frame of factors and numerics")
    
    df <- na.omit(df[, c(cols, target)])
    
    subgroups <- list(
        list(
            description = "",
            items = 1:nrow(df),
            score = 0
        )
    )
    target_col <- df[[target]]
    for (l in 1:levels){
        cat("**** Searching for level", l, "...")
        
        # Generates refinments
        cat("Generates and evaluates refinments...")
        refinments <- lapply(subgroups, function(old_subgroup){
            
            # Gets items
            subgroup_items <- df %>%
                                slice(old_subgroup$items) %>%
                                mutate(row = old_subgroup$items,
                                       root_description = old_subgroup$description)
            
            # Preprocesses
            just_ones <- sapply(cols, function(col) 
                length(unique(subgroup_items[[col]])) < 2
            )
            
            good_cols <- cols[!just_ones]
            if (sum(just_ones > 1)){
                selectr <- paste0("-", cols[just_ones])
                subgroup_items <- subgroup_items %>%
                                    select_(.dots = selectr)
            } 
            
            bin_expr <- paste0("~bin_col(", good_cols, ", ", nbins,")")
            bin_expr   <- sapply(bin_expr, as.formula)
            names(bin_expr) <- good_cols
            subgroup_items <- subgroup_items %>%
                mutate_(.dots = bin_expr)
            
            # Does the maths
            subgroup_items <- subgroup_items %>%
                gather_("column", "label", good_cols) %>% 
                group_by(root_description, column, label) %>% 
                do(items = .$row,
                   KL    = fast_kullback_leibler(target_col, .[[target]]),
                   count = nrow(.)) %>%
                mutate(freq = count / length(target_col),
                       score = KL * freq) %>%
                data.frame
            
            # Filters
            subgroup_items <- subgroup_items %>%
                            filter(freq >= min_freq)
            
            if (nrow(subgroup_items) > 1)
                subgroup_items <- subgroup_items %>%
                            filter(row_number(desc(score)) <= size_beam)
            
            return(subgroup_items)
        })
        
        cat("Updates candidates... ")
        refinments <-  rbind_all(refinments)
        subgroups <- flush_refinments(refinments, subgroups, size_beam)
        
        cat("done\n")
    }
    
    return(subgroups)
}
# 
# # Experiments
# library(foreign)
# data_file <- read.arff("~/Projects/TurboGroups/data/communities.arff")
# backup <- data_file
# 
# target <- "ViolentCrimesPerPop"
# dimensions <- names(data_file)[names(data_file) != target]
# data_file <- preprocess_for_sd(data_file, target, 8)
# 
# groups <- subgroup_discovery(data_file, dimensions, "ViolentCrimesPerPop")
# 
# plot_subgroups(groups, data_file)

