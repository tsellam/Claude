library(dplyr)
library(tidyr)

itemsets <- list(
    list(columns = as.character(c(1,2,3)), strength=0.12),
    list(columns = as.character(c(2,3)), strength=0.2),
    list(columns = as.character(c(3,4,5)), strength=0.1),
    list(columns = as.character(c(4,5)), strength=4.2),
    list(columns = as.character(c(6)), strength=0.4),
    list(columns = as.character(c(1,2,3,4,5)), strength=1.5),
    list(columns = as.character(c(2,3,4)), strength=3.2),
    list(columns = as.character(c(2,3,5)), strength=3.3)
)

get_cover <- function(df){
    # Standard cover order
    df_items <- df %>%
        arrange(desc(n_items.y), desc(support.y), code_id) %>%
        mutate(order = row_number(), selected = FALSE) %>%
        select(set_id, support.x, column,
               code_id, n_items.y,
               order, selected)
        
    # Filtering
    columns <- df_items %>%
                select(column) %>%
                distinct()
    while(nrow(columns) > 0){
        
        if (all(df_items$selected))
            stop("Inifinite loop detected")
            
        # Gets first, non used, covering code words
        covers <- columns %>%
                        inner_join(df_items, by = c("column" = "column")) %>%
                        filter(!selected) %>%
                        group_by(set_id, code_id, n_items.y, order) %>%
                        summarize(inters = n_items.y) %>%
                        filter(inters >= n_items.y) %>%
                        ungroup
        
        first_cover <- covers %>%
                        filter(order == min(order)) %>%
                        select(code_id)
        
        first_cover_id <- first_cover[[1]]
        
        # Updates the items table
        df_items <- df_items %>%
                    mutate(selected = selected | 
                          (code_id == first_cover_id))
        
        covers <- df_items %>%
                    filter(selected) %>%
                    select(column)
                    
        columns <- columns %>%
                    anti_join(covers, by = c("column", "column"))
    }
    
    out <- df_items %>%
        filter(selected) %>%
        select(set_id, support.x, code_id) %>%
        distinct()
}

# Initialization
itemset_table <- lapply(1:length(itemsets), function(i){
    s <- itemsets[[i]]
    data.frame(column = s$columns,
               support = rep(s$strength, length(s$columns)),
               n_items = rep(length(s$columns), length(s$columns)),
               set_id =  rep(i, length(s$columns)))
})
itemset_table <- rbind_all(itemset_table)

standard_code_table <- select(itemset_table, column) %>%
                        distinct() %>%
                        inner_join(itemset_table, by = ("column" = "column")) %>%
                        group_by(column) %>%
                        summarize(support = sum(support)) %>%
                        mutate(code_length = log2(sum(support) / support)) %>%
                        arrange(code_length) %>%
                        mutate(code_id = -1 * row_number(), n_items = 1)

size_db <- inner_join(itemset_table, standard_code_table, by = ("column" = "column")) %>%
            group_by(set_id) %>%
            summarize(size_set = sum(support.x * code_length)) %>%
            summarize(size_table = sum(size_set))
size_db <- size_db$size_table

size_table <- standard_code_table %>%
                summarize(size_ct = sum(2*code_length))
description_length <- size_db + size_table


# Iterations
itemsets <- itemset_table %>%
            arrange(desc(support), desc(n_items), set_id) %>%
            select(set_id)
for(new_itemset_id in itemsets[[1]]){
    cat("Testing set:", as.character(new_itemset_id), ":")
    
    # Retrieving set
    new_itemset <- itemset_table %>%
                    filter(set_id == new_itemset_id)
    print(new_itemset$column)
    
    new_itemset <- new_itemset %>%
                    mutate(code_id = set_id, code_length = 1*NA) %>%
                    select(-set_id)
    
    # Appending it to table
    new_code_table <- rbind(standard_code_table, new_itemset)
                        
    # Calculates the affectations...
    code_matching <- inner_join(itemset_table,
                                new_code_table,
                                by = c("column" = "column"))
    
    # Gets candidate code words
    inclusion_matches <- code_matching %>%
                            select(set_id, code_id, n_items.y, column) %>%
                            group_by(set_id, code_id, n_items.y) %>%
                            summarize(inters = length(column)) %>%
                            filter(inters == n_items.y) %>%
                            select(-n_items.y, -inters)
    code_matching <- code_matching %>%
                        semi_join(inclusion_matches,
                                  by=c("set_id" = "set_id", 
                                      "code_id" = "code_id"))
    
    # Filters the code words with standard cover order
    code_matching <- code_matching %>%
                        group_by(set_id, support.x) %>%
                        do(get_cover(.))
    
    # Gets all the lengths
    code_lengths <- code_matching %>%
                    group_by(code_id) %>%
                    summarize(support = sum(support.x)) %>%
                    mutate(code_length = log2(sum(support) / support))
    
    size_table <- code_matching %>%
                    inner_join(code_lengths, by = c("code_id" = "code_id")) %>%
                    group_by(set_id) %>%
                    summarize(size_set = sum(support.x * code_length))
    
    size_table <- sum(size_table$size_set)
    
    
    
    print("")
}