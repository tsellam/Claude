#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0|| exists("R_TEST")) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")
library(foreign)

source("../code/StrongViews.R", chdir = TRUE)
source("../code/Baselines.R", chdir = TRUE)

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")


file_log     <- "compare_scores.out"
log_headers <- c("file", "size_view",
                 "view", "strength", 
                 "algo", "F1_score\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)


s <- 3
if (test_mode){
    q <- 25
} else {
    q <- 500
}

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")

    # Prepares a function to output stuff
    writelog <- function(...){
        line <- paste0(c(arff_file, s, ... ), collapse="\t")
        cat(line, "\n", file = file_log, append = TRUE)
    }
  
    tryCatch({
        cat("Loading file...\n")
        file  <- read.arff(paste0(files_location, "/", arff_file))
        file  <- file[sample(1:nrow(file), min(nrow(file), 10000), replace=FALSE),]
        target <- names(file)[[ncol(file)]]
        
        cat("Running algos\n")
        
        # Generates random views and gets strength
        clean_data <- preprocess(file, target)
        
        cat("Generating random views\n")
        colnames  <- names(clean_data)
        dim_names <- colnames[1:(length(colnames) - 1)]
        target    <- colnames[length(colnames)]
        
        combinations <- lapply(1:q, function(i) 
            unique(sample(x=dim_names, 4 ,replace = TRUE)))
        
        cat("Computing score\n")
        views <- lapply(combinations, function(cols){
            return(list(
                columns  = cols,
                strength = fast_joint_mutual_information(cols, target, clean_data)
            ))
        })
        
        # Get NB score
        cat("Computing NB F1\n")
        clean_data_NB <- preprocess_NB(file, target)
        get_NB_score(views, clean_data_NB, target, writelog)
        
        # Get kNN score
        cat("Computing kNN F1\n")
        clean_data_kNN <- preprocess_kNN(file, target)
        get_kNN_score(views, clean_data_kNN, target, writelog)
        
        cat("Done\n")
     },
    error = function(e){
        cat("ERROR!\n")
        print(e)
    })

}
