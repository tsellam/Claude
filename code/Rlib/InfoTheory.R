library(dplyr)
dyn.load("info_theory.so")

###########
# Entropy #
###########
.entropy <- function(s){
    s <- na.omit(s)
    tab <- table(s)
    tab <- tab[tab != 0]
    probas <- tab / sum(tab)
    -1 * sum(log2(probas) * probas)
}

entropy <- function(col, data){
    if (!is.data.frame(data) || !col %in% names(data))
        stop("Incorrect input data")
    
    df <- data %>%
        select(series = get(col)) %>%
        filter(!is.na(series))
    
    counts <- df %>%
        group_by(series) %>%
        summarize(count = n())
    
    per_item <- counts %>% 
        mutate(total = sum(count),
               proba = count / total,
               info  = -log2(proba) * proba)
    
    sum(per_item$info)
}

#################
# Two variables #
#################
cond_entropy <- function(col2, col1, data){
    if (!is.data.frame(data) 
        || !(col1 %in% names(data))
        || !(col2 %in% names(data)))
        stop("Incorrect input data")
    
    df <- data %>%
        select(s1 = get(col1), s2 = get(col2)) %>%
        filter(!is.na(s1) & !is.na(s2))
    
    probas_given_s1 <- df %>%
        group_by(s1, s2) %>%
        summarize(count_s1 = n()) %>%
        mutate(total_s1 = sum(count_s1),
               proba_s1 = count_s1 / total_s1,
               info_s1  = -log2(proba_s1) * proba_s1)
    
    agg <- probas_given_s1 %>%
        summarize(entropy = sum(info_s1), count = first(total_s1)) %>%
        mutate(proba = count / sum(count)) %>%
        summarize(entropy = sum(entropy * proba)) %>%
        as.numeric    
}

mutual_info_check <- function(col1, col2, data){
    if (!is.data.frame(data) 
        || !(col1 %in% names(data))
        || !(col2 %in% names(data)))
        stop("Incorrect input data")
    
    df <- data %>%
        select(s1 = get(col1), s2 = get(col2)) %>%
        filter(!is.na(s1) & !is.na(s2))
    
    entropy_col2 <- entropy("s2", df)
    entropy_col2_cond_col1 <- cond_entropy("s2", "s1", df)
    
    entropy_col2 - entropy_col2_cond_col1
}

mutual_information <- function(col1, col2, data){
    if (!is.data.frame(data) 
        || !(col1 %in% names(data))
        || !(col2 %in% names(data)))
        stop("Incorrect input data")
    
    df <- data %>%
        select(s1 = get(col1), s2 = get(col2)) %>%
        filter(!is.na(s1) & !is.na(s2))
        
    entropy1 <- entropy("s1", df)
    entropy2 <- entropy("s2", df)
    entropy_12 <- joint_entropy(c("s1", "s2"), df)
    
    entropy1 + entropy2 - entropy_12 
}

############################
# Three variables and more #
############################
joint_entropy <- function(cols, data, simplify=TRUE){
    if (!is.data.frame(data) || !all(cols %in% names(data)))
        stop("Incorrect input data")
    
    selector  <- as.list(cols)
    na_filter <- paste0("!is.na(", cols, ")", collapse = " && ")
    
    counts <- data %>%
        select_(.dots=selector) %>%
        filter_(na_filter) %>%
        group_by_(.dots=selector) %>%
        summarize(count = n())
    
    total <- sum(counts$count)
    
    per_item <- counts %>% 
        mutate(proba = count / total,
               info  = -log2(proba) * proba) %>%
               as.data.frame %>%
               summarize(entropy = sum(info))
    
    if (simplify){
        return(as.numeric(per_item$entropy))
    } else {
        return(per_item)
    }
}

cond_joint_entropy <- function(cols, cond_cols, data, simplify=TRUE){
    if (!is.data.frame(data) 
        || !all(c(cols, cond_cols) %in% names(data)))
        stop("Incorrect input data")
    
    all_columns <- c(cols, cond_cols)
    na_filter <- paste0("!is.na(", all_columns, ")", collapse = " && ")
    
    df <- data %>%
        select_(.dots = as.list(all_columns)) %>%
        filter_(na_filter) %>%
        group_by_(.dots = as.list(cond_cols))
    
    if (nrow(df) < 1) return(0)
    
    per_group <- df %>% do(data.frame(
        JE = joint_entropy(cols, .),
        count = nrow(.))
    )
    
    per_group <- as.data.frame(per_group)
    cmi <- sum( per_group$JE * per_group$count ) / sum(per_group$count)
    return(cmi)
}

cond_mutual_information <- function(col1, col2, cond_cols, data){
    if (!is.data.frame(data) 
        || !all(c(col1, col2, cond_cols) %in% names(data)))
        stop("Incorrect input data")
        
    all_columns <- c(cond_cols, col1, col2)
    na_filter <- paste0("!is.na(", all_columns, ")", collapse = " && ")
    
    df <- data %>%
            select_(.dots = as.list(all_columns)) %>%
            filter_(na_filter) %>%
            group_by_(.dots = as.list(cond_cols))
    
    per_group <- df %>% do(data.frame(
            MI = mutual_information(col1, col2, .),
            count = nrow(.))
        )
    
    per_group <- as.data.frame(per_group)
    cmi <- sum( per_group$MI * per_group$count ) / sum(per_group$count)
    return(cmi)
}

##########################
# Fast C implementations #
##########################
fast_joint_mutual_information <- function(cols, target, data){
        
    if (!is.data.frame(data) ||
        !all(c(cols, target) %in% names(data)) ||
        !all(sapply(data[c(cols, target)], is.numeric)))
        stop("Incorrect input data")
    
    # Remove NAs
    no_nas <- na.omit(data[,c(cols, target)])
    if (nrow(no_nas) < 2) return(0)
    # Merge cols
    data_cols <- do.call(paste, no_nas[cols])
    # Lowers indices
    data_cols  <- as.integer(factor(data_cols))
    target_col <- as.integer(factor(no_nas[,target]))
    if (length(data_cols) != length(target_col)) stop("FUCK")
    # Super fast calculations
    joint_entropy <- .Call("jointEntropy", data_cols, target_col)
    entropy1      <- .Call("entropy", data_cols)
    entropy2      <- .Call("entropy", target_col)
    jmi <- entropy1 + entropy2 - joint_entropy
    return(jmi)
}


# col1 : the model, ideal value
# col2 : the approximation
fast_kullback_leibler <- function(col1, col2){
    
    if (!(is.numeric(col1) || is.factor(col1)) ||
        !(is.numeric(col2) || is.factor(col2)) )
        stop("Incorrect input data!")
    
    col1 <- na.omit(col1)
    col2 <- na.omit(col2)
    
    if (length(col1) < 2 || length(col2) < 2)
        return(NA)    
    
    if (is.factor(col1) && is.factor(col1)){
        if (!setequal(levels(col1), levels(col2)))
            warning("BEWARE: suspicious levels in KL divergence!")
        col1 <- as.numeric(col1)
        col2 <- as.numeric(col2)
    }
    
    if (max(col2) > max(col1))
        warning("BEWARE: levels of SMALL variable not found in BIG variable!")
    levs <- sort(unique(col1))
    col1 <- as.integer(factor(col1, levels=levs))
    col2 <- as.integer(factor(col2, levels=levs))
    
    KL_divergence <- .Call("KullbackLeibler", col1, col2)
    return(KL_divergence)
}