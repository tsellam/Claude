library("dplyr")
library("tidyr")
source("graph-utils.R")

FOLDER <- "21-04"

###############
# PREPARATION #
###############
# Out files
files <- list.files(FOLDER, pattern = "view\\+poi.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")

cat(length(files), " result files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
out_file <- rbind_all(file_contents)

# Timing files
files <- list.files(FOLDER, pattern = "view\\+poi.log", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")

cat(length(files), " log files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
log_file <- rbind_all(file_contents)

# Filters and Prettifies
black_list <-  c("internet_usage.arff", "insurance.arff")#, "liver.arff")
out_file <- out_file %>%
    filter(!file %in% black_list) %>%
    mutate(file = sub(".arff", "", file))

log_file <- log_file %>%
    filter(!file %in% black_list) %>%
    mutate(file = sub(".arff", "", file)) %>%
    filter(key == "Time")


####################
# Plots the Scores #
####################
to_plot <- out_file %>%
            group_by(file, algo) %>%
            summarize(med_score = median(score),
                      min_score = min(score),
                      max_score = max(score))

g1 <- ggplot(to_plot, aes(x = file, y = med_score, 
                          ymin = min_score, ymax = max_score,
                          color = algo, fill = algo)) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.75)

g1 <- prettify(g1)

print(g1)

#####################
# Plots the runtimes #
#####################
to_plot <- log_file %>%
    spread(key, value)

g2 <- ggplot(to_plot, aes(x = file, y = Time, color = algo, fill = algo)) +
        geom_bar(position = "dodge", stat = "identity")# +
    #coord_cartesian(ylim = c(0,30))
g2 <- prettify(g2)

print(g2)

