library(dplyr)

###########
# Entropy #
###########
.entropy <- function(s){
    s <- na.omit(s)
    tab <- table(s)
    tab <- tab[tab != 0]
    probas <- tab / sum(tab)
    -1 * sum(log(probas) * probas)
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
               info  = -log(proba) * proba)
    
    sum(per_item$info)
}

#################
# Two variables #
#################
joint_entropy <- function(cols, data){
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
             info  = -log(proba) * proba)
   
    sum(per_item$info)
}

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
               info_s1  = -log(proba_s1) * proba_s1)
    
    agg <- probas_given_s1 %>%
        summarize(entropy = sum(info_s1), count = first(total_s1)) %>%
        mutate(proba = count / sum(count)) %>%
        summarize(entropy = sum(entropy * proba)) %>%
        as.numeric    
}

mutual_information <- function(col1, col2, data){
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

mutual_info_check <- function(col1, col2, data){
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

###################
# Three variables #
###################
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
    if (nrow(df) < 2) return(0)
    
    per_group <- df %>% do(data.frame(
            MI = mutual_information(col1, col2, .),
            count = nrow(.))
        )
    
    per_group <- as.data.frame(per_group)
    cmi <- sum( per_group$MI * per_group$count ) / sum(per_group$count)
    return(cmi)
}