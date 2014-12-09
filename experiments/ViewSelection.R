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
    file_list <- file_list
} else {
    q    <- c(25)
    beam <- c(25, 250)
    size_view <- c(3, 8)    
}

wrapper <- function(...){
    tryCatch(
        evalWithTimeout(
            ...,
            timeout=1800
        ),
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
                beamed <- wrapper(
                                  search_exact(clean_data, target, q = q,
                                         size_view = s, size_beam = b,
                                         logfun = writelog, outfun = writeout)
                                  )
                
                approx <- wrapper(
                            search_approx(clean_data, target, q = q,
                                size_view = s, size_beam = b,
                                logfun = writelog, outfun = writeout)
                            )
                
                clique_approx <-  wrapper(
                            search_cliques(clean_data, target, q = q,
                                size_view = s, size_beam = b, 
                                logfun = writelog, outfun = writeout)
                            )
                
                # Baseline
                clean_data <- preprocess_NB(file, target)
                beamed_NB <-  wrapper(
                            search_exact_NB(clean_data, target, q = q,
                               size_view = s, size_beam = b,
                               logfun = writelog, outfun = writeout)
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
