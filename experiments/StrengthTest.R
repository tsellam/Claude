#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")

library(foreign)

source("../code/StrongViews.R", chdir = TRUE)
source("../code/Baselines.R", chdir = TRUE)

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")

file_log     <- "compare_scores.out"
log_headers <- c("file", "beam_size", "size_view",
                 "view", "strength", "NB_score\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)


s <- 3
if (test_mode){
    file_list <- file_list
    q <- 25
    b <- 25
} else {
    q <- 250
    b <- 250
}

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")

    # Prepares a function to output stuff
    writelog <- function(...){
        line <- paste0(c(arff_file, b, s, ... ), collapse="\t")
        cat(line, "\n", file = file_log, append = TRUE)
    }
  
    tryCatch({
        cat("Loading file...\n")
        file  <- read.arff(paste0(files_location, "/", arff_file))
        target <- names(file)[[ncol(file)]]
        
        cat("Running algos\n")
        
        # Claude stuff
        clean_data <- preprocess(file, target)
        beamed <- search_exact(clean_data, target, q = q,
                    size_view = s, size_beam = b)
        
        # Baseline
        clean_data <- preprocess_NB(file, target)
        get_NB_score(beamed, clean_data, target, writelog)
        
        cat("Done\n")
    },
    error = function(e){
        cat("ERROR!\n")
        print(e)
    })

}