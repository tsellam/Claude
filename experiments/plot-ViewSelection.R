#!/usr/bin/Rscript
source("graph-utils.R")
library(dplyr)
library(tidyr)

###############
# PREPARATION #
###############
log_file <- read.delim("~/Projects/TurboGroups/experiments/results-12:12/agg.results.log",
                       stringsAsFactors=FALSE)
out_file <- read.delim("~/Projects/TurboGroups/experiments/results-12:12/agg.results.out",
                       stringsAsFactors=FALSE)

log_file <- log_file %>%
    filter(!(file %in% c("insurance.arff", "cover_type.arff")))%>%
    filter(!algo %in% c("Clique", "Wrap_NaiveBayes"))

out_file <- out_file %>%
    filter(!(file %in% c("insurance.arff", "cover_type.arff")))%>%
    filter(!algo %in% c("Clique", "Wrap_NaiveBayes"))


log_file$algo <- factor(log_file$algo,
                        levels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"),
                        labels = c("Exact", "Approximative", "Clique", "Wrap_NaiveBayes"))
log_file$size_view <- factor(log_file$size_view, 
                             levels = unique(sort(log_file$size_view)),
                             labels = paste(unique(sort(log_file$size_view)), " dim."))
log_file$beam_size <- factor(log_file$beam_size,
                             levels = unique(sort(out_file$beam_size)),
                             labels = paste("Beam Size: ", unique(sort(out_file$beam_size))))

order_files <- log_file %>%
                filter(beam_size == "Beam Size:  250" &
                           size_view == "3  dim." &
                           key == "Time" & algo == "Approximative") %>%
                           arrange(value) %>% select(file)
order_files <- order_files[,1]
    
log_file$file <- factor(log_file$file, levels = order_files, labels = order_files)
levels(log_file$file) <- sub(".arff", "", levels(log_file$file))
log_file <- arrange(log_file, file, size_view, beam_size, algo)


out_file$algo <- factor(out_file$algo,
                        levels = c("Exhaustive", "Approximative", "Clique", "Wrap_NaiveBayes"),
                        labels = c("Exact", "Approximative", "Clique", "Wrap_NaiveBayes"))
out_file$size_view <- factor(out_file$size_view, 
                             levels = unique(sort(out_file$size_view)),
                             labels = paste(unique(sort(out_file$size_view)), " dim."))
out_file$beam_size <- factor(out_file$beam_size, 
                             levels = unique(sort(out_file$beam_size)),
                             labels = paste("Beam Size: ", unique(sort(out_file$beam_size))))
out_file$file <- factor(out_file$file , levels = order_files, labels = order_files)
levels(out_file$file) <- sub(".arff", "", levels(out_file$file))
out_file <- arrange(out_file, file, size_view, beam_size, algo)

#############
# PLOTTING #
############
accuracies <- out_file %>%
                filter(key == "Strength") %>%
                mutate(strength = as.numeric(value)) %>%
                group_by(file, beam_size, size_view, algo) %>%
                summarize(strength = mean(strength)) %>% 
                data.frame

algo_names <- as.character(unique(accuracies$algo))
accuracies <- accuracies %>%
                spread(algo, strength) %>%
                gather_(gather_cols = algo_names,
                        na.rm = FALSE) %>%
                mutate(algo = variable,
                       strength = value,
                       xceed = ifelse(is.na(strength),"X", "")) %>%
                select(-value, -variable) %>%
                data.frame

target_entropies <- read.delim("entropies.out")
accuracies <- left_join(accuracies, target_entropies, by = c('file' = 'file'))
accuracies <- mutate(accuracies, strength = strength / entropy)


algo_accuracy <- ggplot(accuracies, aes(x = file, y=strength, color = algo, fill = algo)) +
                        geom_bar(stat = "identity", position = "dodge") +
                        facet_grid(size_view ~ beam_size) +
                        geom_text(aes(y=0.01, label = xceed),
                                  position = position_dodge(width = 0.9),
                                  size=2,
                                  color = "black") +
                        scale_x_discrete(name="Dataset") +
                        scale_y_continuous(name = "View Score (Normalized)", breaks = c(0,1))

algo_accuracy <- prettify(algo_accuracy) +
                    theme(legend.position = "top", axis.text.x = element_text(angle = 22, hjust = 1))
print(algo_accuracy)



#############
# RUNTIMES! #
#############

runtimes <- log_file %>%
    filter(key == "Time") %>%
    mutate(runtime = value) %>% 
    filter(!algo == "Wrap_NaiveBayes") %>%
    select(-key, -value)

algo_names <- as.character(unique(runtimes$algo))
runtimes <- runtimes %>%
    spread(algo, runtime) %>%
    gather_(gather_cols = algo_names,
            na.rm = FALSE) %>%
    mutate(algo = variable,
           xceed = ifelse(is.na(value) ,">2000", round(value, 1))) %>%
    group_by(file, beam_size, size_view) %>%
    mutate(runtime = ifelse(is.na(value), 2000, value)) %>%
    data.frame


runtimes <- runtimes %>%
                group_by(size_view, beam_size, file) %>%
                mutate(runtime = runtime / max(runtime))

algo_runtime <- ggplot(runtimes, aes(x = file, y=runtime * 100, color = algo, fill = algo)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(y=runtime * 100 / 2, label = xceed),
              position = position_dodge(width = 0.9),
              size=2,
              color = "black") +
    facet_grid(size_view ~ beam_size) +
    scale_x_discrete(name="Dataset") +
    scale_y_continuous(name = "Runtime (% max)", breaks = c(0, 100))

algo_runtime <- prettify(algo_runtime)+
    theme(legend.position = "top", axis.text.x = element_text(angle = 22, hjust = 1))
print(algo_runtime)



ggsave("../documents/plots/tmp_column-select-score.pdf", algo_accuracy, width = 8.5, height = 2.25)
#ggsave("../documents/plots/tmp_column-select-score-beam.pdf", beam_accuracy, width = 6.66, height = 2)
ggsave("../documents/plots/tmp_column-select-time.pdf", algo_runtime, width = 8.5, height = 2.25)






# 
# # Vary Algos
# algo_runtime <- runtimes %>%
#     filter(as.numeric(beam_size) == 1) %>%
#     data.frame
# 
# algo_runtime <- ggplot(algo_runtime, aes(x = factor(algo), y=runtime, fill=algo, label=xceed)) +
#     scale_x_discrete(labels=NULL, name=NULL) +
#     geom_bar(stat = "identity", position = "dodge") +
#     geom_text(aes(y=runtime / 2), 
#               position = position_dodge(width = 0.9),
#               color = "darkgrey",
#               size=2) +
#     facet_grid( file ~ size_view, scales = "free_y")
# 
# algo_runtime <- prettify(algo_runtime)
# 
# plot(algo_runtime)

# 
# 
# # Vary Algos
# algo_accuracies <- accuracies %>%
#                 filter(as.numeric(beam_size) == 1) %>%
#                 group_by(file, beam_size, size_view) %>%
#                 mutate(strength = strength / max(strength, na.rm = TRUE) * 100) %>%
#                 data.frame
# 
# algo_accuracy <- ggplot(algo_accuracies, aes(x = factor(algo), y=strength, fill=algo, label=xceed)) +
#                     scale_x_discrete(labels=NULL, name="") +
#                     scale_y_continuous(name = "Strength (% max)", limits=c(0,100)) +
#                     geom_bar(stat = "identity", position = "dodge") +
#                     geom_text(aes(y=0), 
#                               position = position_dodge(width = 0.9),
#                               color = "darkgrey",
#                               size=1.5) +
#                     facet_grid( size_view ~ file)
# 
# algo_accuracy <- prettify(algo_accuracy)
# 
# plot(algo_accuracy)
# 
# 
# # Vary beam size
# beam_accuracies <-  accuracies %>%
#    filter(as.numeric(size_view) == 1) %>%
#     group_by(file) %>%
#     mutate(strength = strength / max(strength, na.rm = TRUE) * 100) %>%
#     data.frame
# 
# beam_accuracy <- ggplot(beam_accuracies, 
#                         aes(x = beam_size, y=strength, fill=algo, label=xceed)) +
#     scale_x_discrete(name = "Beam Size") +
#     scale_y_continuous(name = "Strength (% max)", limits=c(0,100)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     geom_text(aes(y=0), 
#               position = position_dodge(width = 0.9),
#               color = "darkgrey",
#               size=3) +
#     facet_grid( . ~ file)
# 
# beam_accuracy <- prettify(beam_accuracy)
# print(beam_accuracy)


