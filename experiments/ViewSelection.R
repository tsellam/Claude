#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")

library(foreign)
library(R.utils)

source("../code/StrongViews.R", chdir = TRUE)
source("../code/Baselines.R", chdir = TRUE)

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")
sizes <- file.info(paste0(files_location, "/", file_list))$size
file_list <- file_list[order(sizes)]

file_log     <- "results.log"
log_headers <- c("file", "beam_size", "size_view",
                  "algo","key", "value\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)

file_out <- "results.out"
out_headers <- c("file", "beam_size", "size_view", 
                 "algo", "view", "key", "value\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)


if (test_mode){
    q    <- c(5)
    beam <- c(5)
    size_view <- c(3)
    file_list <- file_list
} else {
    q    <- c(50)
    beam <- c(50, 250)
    size_view <- c(2, 5)    
}

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")
    
    for (b in beam){
        for (s in size_view){

            # Prepares a function to output stuff
            writelog <- function(...){
                line <- paste0(c(arff_file, b, s, ... ), collapse="\t")
                cat(line, "\n", file = file_log, append = TRUE)
            }
            writeout <- function(...){
                line <- paste0(c(arff_file, b, s, ...) , collapse="\t")
                cat(line, "\n", file = file_out, append = TRUE)
            }
            
            tryCatch({
                cat("Loading file...\n")
                file  <- read.arff(paste0(files_location, "/", arff_file))
                target <- names(file)[[ncol(file)]]
                
                cat("Running algos\n")
                
                # Claude stuff
                clean_data <- preprocess(file, target)
                beamed <- evalWithTimeout(
                            search_exact(clean_data, target, q = q,
                                         size_view = s, size_beam = b,
                                         logfun = writelog, outfun = writeout),
                            timeout = 900)
                
                approx <-  evalWithTimeout(
                            search_approx(clean_data, target, q = q,
                                size_view = s, size_beam = b,
                                logfun = writelog, outfun = writeout),
                            timeout = 900)
                
                clique_approx <-  evalWithTimeout(
                            search_cliques(clean_data, target, q = q,
                                size_view = s, size_beam = b, 
                                logfun = writelog, outfun = writeout),
                            timeout = 900)
                
                # Baseline
                clean_data <- preprocess_NB(file, target)
                beamed_NB <-  evalWithTimeout(
                            search_exact_NB(clean_data, target, q = q,
                               size_view = s, size_beam = b,
                               logfun = writelog, outfun = writeout),
                            timeout = 900)


                cat("Done\n")
            },
            error = function(e){
                cat("ERROR!\n")
                print(e)
            })
        }
    }
}
