#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0 || exists("R_TEST")) TRUE else FALSE
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
    file_list <- file_list[3]
} else {
    q    <- c(25)
    beam <- c(25, 250)
    size_view <- c(3, 8)    
}

wrapper <- function(..., score_function, algo){
     tryCatch({
        out <- evalWithTimeout(... , timeout=1800)
        score_function(out, algo)
     },
     error = function(e){
            cat("Error, or TIMEOUT!\n")
            print(e)
        }
     )
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
                file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]
                target <- names(file)[[ncol(file)]]
                
                cat("Running algos\n")
                
                # Claude stuff
                clean_data <- preprocess(file, target)
                clean_data_NB <- preprocess_NB(file, target)
                
                 score_function <- function(res, algo){
                     get_NB_score(res, clean_data_NB, target, writeout, algo)
                 }
             
                beamed <- wrapper(
                              search_exact(clean_data, target, q = q,
                                     size_view = s, size_beam = b,
                                     logfun = writelog, outfun = writeout),
                                    score_function = score_function,
                                    algo = "Exhaustive"
                                  )

                approx <- wrapper(
                            search_approx(clean_data, target, q = q,
                                size_view = s, size_beam = b,
                                logfun = writelog, outfun = writeout),
                                score_function = score_function,
                                algo = "Approximative"
                            )                
                
                # Baseline
                beamed_NB <-  wrapper(
                            search_exact_NB(clean_data, target, q = q,
                               size_view = s, size_beam = b,
                               logfun = writelog, outfun = writeout),
                            score_function = score_function,
                            algo = "Wrap_NaiveBayes"
                )
                
                cat("Done\n")
            },
             error = function(e){
                 cat("ERROR!\n")
                 print(e)
             })
        }
    }
}
