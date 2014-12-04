library(foreign)

source("../code/StrongViews.R", chdir = TRUE)
source("../code/Baselines.R", chdir = TRUE)

files_location <- "../data/experiments"
file_list <- list.files(path = files_location, pattern = "*.arff$")

file_out <- "entropies.out"
out_headers <- c("file", "entropy\n")
cat(paste0(out_headers, collapse="\t"), file = file_out)

for (arff_file in file_list){
    cat("Loading file...\n")
    file  <- read.arff(paste0(files_location, "/", arff_file))
    target <- names(file)[[ncol(file)]]
    
    cat("Getting entropy\n")
    H <- entropy(target, file)
    
    cat(sub(".arff", "", arff_file), "\t", H, "\n", file=file_out, append = TRUE)
    print(H)
}