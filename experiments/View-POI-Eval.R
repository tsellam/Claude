#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0 || exists("R_TEST")) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")

library(foreign)
library(R.utils)

source("../code/MakeViews.R", chdir = TRUE)

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")
sizes <- file.info(paste0(files_location, "/", file_list))$size
file_list <- file_list[order(sizes)]

file_log     <- "view+poi.log"
log_headers <- c("file", "q", "k", "size",
                  "algo","key", "value\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)

file_out <- "view+poi.out"
out_headers <- c("file", "q", "k", "size",
                 "algo", "view", "score", "description\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)


if (test_mode){
    qs    <- c(5)
    ks    <- c(5)
    size_views <- c(3)
} else {
    qs    <- c(10, 25)
    ks    <- c(10, 25)
    size_views <- c(3, 8)    
}

wrapper <- function(...){
    tryCatch(
        evalWithTimeout(
            ...,
            timeout=900
        ),
        error = function(e){
                cat("Error, or TIMEOUT!\n")
                print(e)
        }
    )
}

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")
    
    for (q in qs){
        for (k in ks){
            for (s in size_views){

                # Prepares a function to output stuff
                writelog <- function(...){
                    line <- paste0(c(arff_file, q, k, s,  ... ), collapse="\t")
                    cat(line, "\n", file = file_log, append = TRUE)
                }
                writeout <- function(...){
                    line <- paste0(c(arff_file, q, k, s, ...) , collapse="\t")
                    cat(line, "\n", file = file_out, append = TRUE)
                }
                
                tryCatch({
                    cat("Loading file...\n")
                    file  <- read.arff(paste0(files_location, "/", arff_file))
                    file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]
                    target <- names(file)[[ncol(file)]]
                    
                    cat("Running algos\n")
                                        
                    cat("With View Selection\n")
                    wrapper(
                        generate_views(file, target, nbins_target = 5,
                                       q = q, size_view = s, size_beam_q = floor(1.5*q), pessimistic=TRUE,
                                       min_freq = 0.05, k = k, size_beam_k = floor(1.5*k), nbins = 5, levels = 3,
                                       just_POIs = FALSE, logfun = writelog, outfun = writeout)
                    )
                    
                    cat("\nWihtout View Selection\n")
                    wrapper(
                        generate_views(file, target, nbins_target = 5,
                                       min_freq = 0.05, k = k * q, size_beam_k = floor(1.5 * k * q), nbins = 5, levels = 3,
                                       just_POIs = TRUE, logfun = writelog, outfun = writeout)
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
}
