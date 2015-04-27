#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)
library(xtable)

FOLDER <- "25-04"

###############
# PREPARATION #
###############
# Out files
files <- list.files(FOLDER, pattern = "results.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), " result files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
out_file <- rbind_all(file_contents)

# Timing files
files <- list.files(FOLDER, pattern = "results.log", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), " log files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
log_file <- rbind_all(file_contents)

# Entropies
files <- list.files(FOLDER, pattern = "entropies.out", recursive = TRUE)
files <- paste(FOLDER, files, sep = "/")
cat(length(files), "entropy files found!\n")
file_contents <- lapply(files, function(f){
    cat("Reading", f, "\n")
    read.delim(f, stringsAsFactors = FALSE)
})
entropies <- rbind_all(file_contents)


##########################
# Filters and Prettifies #
##########################
black_list <-  c("internet_usage.arff", "insurance.arff")
out_file <- out_file %>%
            filter(!file %in% black_list) %>%
            filter(!algo %in% c("Wrap_NaiveBayes")) %>%
            mutate(file = sub(".arff", "", file)) %>%
            mutate(algo = factor(algo, levels = c("ApproximativePrune",
                                                  "Approximative",
                                                  "4S",
                                                  "4S_official",
                                                  "Clique",
                                                  "Exhaustive",
                                                  "Wrap_kNN"),
                                        labels = c("ApproximativePrune",
                                                  "Claude",
                                                  "4S",
                                                  "4S_official",
                                                  "Clique",
                                                  "Exact",
                                                  "Wrap 5-NN"))) %>%
            mutate(file = factor(file, 
                                 levels = c("adult", "communities", "musk",
                                            "magic", "pendigits", "bank",
                                            "insurance", "breast", "letrec"),
                                 labels = c("USCensus", "Crime", "MuskMolecules",
                                            "MAGICTelescope", "PenDigits", "BankMarketing",
                                            "Insurance", "BreastCancer", "LetterRecog")))

log_file <- log_file %>%
    filter(!file %in% black_list) %>%
    filter(!algo %in% c("Wrap_NaiveBayes", "4S_official")) %>%
    mutate(file = sub(".arff", "", file)) %>% 
    mutate(algo = factor(algo, levels = c("ApproximativePrune",
                                          "Approximative",
                                          "4S",
                                          "4S_official",
                                          "Clique",
                                          "Exhaustive",
                                          "Wrap_kNN"),
                         labels = c("ApproximativePrune",
                                    "Claude",
                                    "4S",
                                    "4S_official",
                                    "Clique",
                                    "Exact",
                                    "Wrap 5-NN"))) %>%
    mutate(file = factor(file, 
                         levels = c("adult", "communities", "musk",
                                    "magic", "pendigits", "bank",
                                    "insurance", "breast", "letrec"),
                         labels = c("USCensus", "Crime", "MuskMolecules",
                                    "MAGICTelescope", "PenDigits", "BankMarketing",
                                    "Insurance", "BreastCancer", "LetterRecog")))

entropies <- entropies %>%
                mutate(file = factor(file, 
                                  levels = c("adult", "communities", "musk",
                                             "magic", "pendigits", "bank",
                                             "insurance", "breast", "letrec"),
                                  labels = c("USCensus", "Crime", "MuskMolecules",
                                             "MAGICTelescope", "PenDigits", "BankMarketing",
                                             "Insurance", "BreastCancer", "LetterRecog")))

#####################
# Plots view scores #
#####################
to_plot <- out_file %>%
            filter(experiment == "VaryAlgos") %>%
            filter(grepl("- F1", key) | grepl("Strength", key)) %>%
            spread(key, value, convert = TRUE) %>%
            mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE))

to_plot <- to_plot %>%
           group_by(file, algo) %>%
           summarize(med_F1 = median(F1), min_F1 = min(F1), max_F1 = max(F1)) %>%
           ungroup

all_combis <-  unique(merge(to_plot$file, to_plot$algo))
colnames(all_combis) <- c("file", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("algo", "file"))

p1 <- ggplot(to_plot, aes(x = file, y = med_F1,
                          ymax=max_F1, ymin=min_F1,
                          color = algo, fill= algo)) +
        geom_pointrange(position = position_dodge(width = 0.5),
                        size = 0.4) +
        scale_x_discrete(name = "Dataset") +
        scale_y_continuous(name = "Accuracy - F1")
        
    
p1 <- prettify(p1) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(p1)

ggsave("../documents/plots/tmp_view-scores.pdf", p1,
       width = 16, height = 4, units = "cm")

####################
# Plots time spent #
####################
to_plot <- log_file %>%
    filter(experiment == "VaryAlgos") %>%
    filter(key == "Time") %>%
    spread(key, value)

all_combis <-  unique(merge(to_plot$file, to_plot$algo))
colnames(all_combis) <- c("file", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("algo", "file")) %>%
            mutate(xceeds = ifelse(is.na(Time), "X", ""))

to_plot[is.na(to_plot)] <- 3600

p2 <- ggplot(to_plot, aes(x = file, y = Time, fill = algo, color=algo, label = xceeds)) +
    geom_bar(stat = "identity", position = "dodge", width=.75) +
    geom_text(aes(y=55), color="black",
              position = position_dodge(width=.75), size=2) +
    scale_x_discrete(name="Dataset") + 
    scale_y_continuous(name="Execution Time (s)") +
    coord_cartesian(ylim = c(0,60))

p2 <- prettify(p2) +
    theme(axis.text.x = element_text(angle = 15, hjust = 1))


print(p2)

ggsave("../documents/plots/tmp_view-times.pdf", p2,
       width = 16, height = 4, units = "cm")

# to_table <- to_plot %>%
#             select(file, algo, Time) %>%
#             spread(algo, Time) %>%
#             arrange(Claude)
# rownames(to_table) <- to_table$file
# to_table$file <- NULL
# 
# old_n <- colnames(to_table)
# to_table <- apply(to_table, 1, function(row){
#     out <- as.character(round(row, 2))
#     out[is.na(row)] <- "*"
#     out[which.min(row)] <- paste0('\\cellcolor{grn} ', out[which.min(row)])
#     out[which.max(row)] <- paste0('\\cellcolor{red} ', out[which.max(row)])
#     out
# })
# to_table <- t(to_table)
# colnames(to_table) <- old_n
# 
# print(xtable(to_table), sanitize.text.function = identity)




###################################
# FILTER FOR CLOSE UP EXPERIMENTS #
###################################
zoom_files <- c("MuskMolecules", "MAGICTelescope", "LetterRecog", "USCensus")


###############################
# Plots vary beam experiments #
###############################
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryBeam" & algo == "Claude") %>%
    filter(grepl("- F1", key) | grepl("Strength", key)) %>%
    filter(file %in% zoom_files) %>%
    spread(key, value, convert = TRUE) %>%
    mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE)) %>%
    select(file, beam_size, view, F1, Strength)
    
to_plot <- to_plot %>%
            inner_join(entropies) %>%
            mutate(Strength = Strength / entropy) %>%
            select(-entropy)

to_plot <- to_plot %>%
            group_by(file, beam_size) %>%
            summarise(med_strength = median(Strength),
                      min_strength = min(Strength),
                      max_strength = max(Strength))

# Prepares the log file
time_to_plot <- log_file %>%
    filter(experiment == "VaryBeam" & algo == "Claude" & key == "Time") %>%
    select(file, beam_size, Time=value)
            
# Joins!p
to_plot <- inner_join(to_plot, time_to_plot, by= c("file"="file", "beam_size" = "beam_size"))

p3 <- ggplot(to_plot, aes(x = Time, y = med_strength, ymin=min_strength, ymax=max_strength)) +
    scale_y_continuous(limits=c(0,1)) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(name = "Execution Time (s)") +
    scale_y_continuous(name = "View Strength (Normalized)") +
    facet_wrap(~ file, scales="free") +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
    geom_ribbon(alpha = 0.3)
    
p3 <- prettify(p3)
print(p3)
ggsave("../documents/plots/tmp_view-vary-beam.pdf", p3,
       width = 9, height = 6, units = "cm")

##################################
# Prepares diversify experiments #
##################################
# Prepares the out file
to_plot <- out_file %>%
    filter(experiment == "VaryDeduplication" & algo == "Claude") %>%
    filter(grepl("- F1", key) | grepl("Strength", key) | grepl("Diversity", key)| grepl("Dissimilarity", key)) %>%
   filter(file %in% zoom_files) %>%
    spread(key, value, convert = TRUE) %>%
    mutate(F1 = pmax(`kNN - F1`, `NaiveBayes - F1`, na.rm = TRUE)) %>%
    select(file, dedup, view, F1, Strength, Diversity, Dissimilarity)

to_plot <- to_plot %>%
    inner_join(entropies) %>%
    mutate(Strength = Strength / entropy) %>%
    select(-entropy)

to_plot <- to_plot %>%
    group_by(file, dedup) %>%
    summarise(med_strength = median(Strength, na.rm = TRUE),
              min_strength = min(Strength, na.rm = TRUE),
              max_strength = max(Strength, na.rm = TRUE),
              Diversity = max(Diversity, na.rm = TRUE),
              Dissimilarity = max(Diversity, na.rm = TRUE),
              F1 = median(F1, na.rm = TRUE))

p4 <- ggplot(to_plot, aes(x=100-dedup, y=Diversity)) +
        scale_x_continuous(name="Deduplication Level (%)") +
        scale_y_continuous(name="# Distinct Variables", breaks= pretty_breaks()) +
        geom_point(size = 0.8) +
        geom_line() +
        facet_wrap(~ file, ncol = 2, scales="free") + expand_limits(y=0)


p4 <- prettify(p4)
print(p4)

ggsave("../documents/plots/tmp_view-vary-diversification.pdf", p4,
       width = 9, height = 6, units = "cm")        

#system("cd ../documents ; ../documents/renderPDF.sh")