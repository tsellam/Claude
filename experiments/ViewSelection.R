#!/usr/bin/Rscript
args <- commandArgs(trailingOnly = TRUE)
test_mode <- if (length(args) > 0 || exists("R_TEST")) TRUE else FALSE
if (test_mode) print("*** Test Mode! ***")

library(foreign)
library(R.utils)

source("../code/StrongViews.R", chdir = TRUE)
source("../code/Baselines.R", chdir = TRUE)
jar_loc <- paste0(getwd(), "/../code/4S/4S.jar")

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")
sizes <- file.info(paste0(files_location, "/", file_list))$size
file_list <- file_list[order(sizes)]

file_log     <- "results.log"
log_headers <- c("experiment", "file", "q", "beam_size", "size_view",
                  "dedup", "algo","key", "value\n")
cat(paste0(log_headers, collapse="\t"), file = file_log)

file_out <- "results.out"
out_headers <- c("experiment", "file", "q", "beam_size", "size_view", 
                 "dedup", "algo", "view", "key", "value\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)

if (test_mode){
    file_list <- "vowel.arff" #file_list[3]
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

#####################################
# First Experiment: Different Algos #
#####################################
for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")
    
    tryCatch({

         # Loading and Preprocessing
         cat("Loading file...\n")
         file  <- read.arff(paste0(files_location, "/", arff_file))
         file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]
         target <- names(file)[[ncol(file)]]
         
         clean_data <- preprocess(file, target)

         # Prepares a function to output stuff
         writelog <- function(...){
             line <- paste0(c("VaryAlgos", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ... ), collapse="\t")
             cat(line, "\n", file = file_log, append = TRUE, sep = "")
         }
         writeout <- function(...){
             line <- paste0(c("VaryAlgos", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ...) , collapse="\t")
             cat(line, "\n", file = file_out, append = TRUE, sep = "")
         }
        
        # Creates alternative score function
        clean_data_NB <- preprocess_NB(file, target)
        clean_data_kNN <- preprocess_kNN(file, target)
        
         score_function <- function(res, algo){
             get_NB_score(res, clean_data_NB, target, writeout, algo)
             get_kNN_score(res, clean_data_kNN, target, writeout, algo)
         }
    
        cat("Running algos\n")
        
        # First run of 4S to get the parameters
        fourS_views <- FourS(clean_data, target, jar_loc = jar_loc)

        s_4s <- ceiling(max(
           sapply(fourS_views, function(v) length(v$columns))
        ))
        s <- s_4s
        q <- length(fourS_views)
        b <- ceiling(max(2.0*q, 2.0*s))
        d_factor <- NULL
        
        cat("Parameters: q-", q, ", b-", b, ", s-", s, "\n", sep = "")
         
        #  Baselines
        fourS_views <-  wrapper(
            FourS(clean_data, target, jar_loc = jar_loc,
                    logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "4S"
        )
        
        beamed_NB <-  wrapper(
            search_exact_NB(clean_data_NB, target, q = q,
                            size_view = s, size_beam = b,
                            logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_NaiveBayes"
        )
        
        beamed_kNN <-  wrapper(
            search_exact_kNN(clean_data_kNN, target, q = q,
                            size_view = s, size_beam = b,
                            logfun = writelog, outfun = writeout),
            score_function = score_function,
            algo = "Wrap_kNN"
        )
        
        # Own boys
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
        
        clique_approx <-  wrapper(
                    search_cliques(clean_data, target, q = q,
                        size_view = s, size_beam = b, 
                        logfun = writelog, outfun = writeout),
                    score_function = score_function,
                    algo = "Clique"
                    )

        cat("Done\n")
    },
     error = function(e){
         cat("ERROR!\n")
         print(e)
     })
}

#################################
# Second Experiment: Vary Beam #
################################
cat("\n\nSECOND ROUND OF EXPERIMENTS\n")

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")
    
    tryCatch({

        # Loading and Preprocessing
        cat("Loading file...\n")
        file  <- read.arff(paste0(files_location, "/", arff_file))
        file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]
        target <- names(file)[[ncol(file)]]
        
        clean_data <- preprocess(file, target)
        
        # Prepares a function to output stuff
        writelog <- function(...){
            line <- paste0(c("VaryBeam", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ... ), collapse="\t")
            cat(line, "\n", file = file_log, append = TRUE, sep = "")
        }
        writeout <- function(...){
            line <- paste0(c("VaryBeam", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ...) , collapse="\t")
            cat(line, "\n", file = file_out, append = TRUE, sep = "")
        }
        
        # Creates alternative score function
        clean_data_NB <- preprocess_NB(file, target)
        clean_data_kNN <- preprocess_kNN(file, target)
        
        score_function <- function(res, algo){
            get_NB_score(res, clean_data_NB, target, writeout, algo)
            get_kNN_score(res, clean_data_kNN, target, writeout, algo)
        }
        
        s <- 5
        q <- 25
        d_factor <- NULL
        
        for (b in c(25, 50, 100, 250)){
            cat("Parameters: q-", q, ", b-", b, ", s-", s, "\n", sep = "")
            
            # Own boys
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
            
            cat("Done\n")
        }
    },
    error = function(e){
        cat("ERROR!\n")
        print(e)
    })
}

################################
# Last Experiment: deduplicate #
################################
cat("\nTHIRD ROUND OF EXPERIMENTS\n")

for (arff_file in file_list){
    cat("\n**** Doing file", arff_file, "\n")
    
    tryCatch({
        
        # Loading and Preprocessing
        cat("Loading file...\n")
        file  <- read.arff(paste0(files_location, "/", arff_file))
        file  <- file[sample(1:nrow(file), nrow(file), replace=FALSE),]
        target <- names(file)[[ncol(file)]]
        
        clean_data <- preprocess(file, target)
        
        # Prepares a function to output stuff
        writelog <- function(...){
            line <- paste0(c("VaryDeduplication", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ... ), collapse="\t")
            cat(line, "\n", file = file_log, append = TRUE, sep = "")
        }
        writeout <- function(...){
            line <- paste0(c("VaryDeduplication", arff_file, q, b, s, ifelse(is.null(d_factor), "100", d_factor), ...) , collapse="\t")
            cat(line, "\n", file = file_out, append = TRUE, sep = "")
        }
        
        # Creates alternative score function
        clean_data_NB <- preprocess_NB(file, target)
        clean_data_kNN <- preprocess_kNN(file, target)
        
        score_function <- function(res, algo){
            get_NB_score(res, clean_data_NB, target, writeout, algo)
            get_kNN_score(res, clean_data_kNN, target, writeout, algo)
            get_diversity_score(res, clean_data, target, writeout, algo)
        }
        
        s <- 5
        q <- 25
        b <- 500
        
        for (d_factor in c(5, 25, 50, 100)){
            
            d <- ceiling((d_factor / 100) * min(b, ncol(clean_data)) * ncol(clean_data))
            cat("Parameters: d-", d, "\n", sep = "")
                        
            approx <- wrapper(
                search_approx(clean_data, target, q = q,
                              size_view = s, size_beam = b, dup_factor = d,
                              logfun = writelog, outfun = writeout),
                score_function = score_function,
                algo = "Approximative"
            )
            
            cat("Done\n")
        }
    },
    error = function(e){
        cat("ERROR!\n")
        print(e)
    })
}