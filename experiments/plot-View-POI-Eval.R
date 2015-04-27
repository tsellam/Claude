library("dplyr")
library("tidyr")
source("graph-utils.R")

FOLDER <- "25-04"

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
black_list <-  c("internet_usage.arff", "insurance.arff")
out_file <- out_file %>%
    filter(!file %in% black_list) %>%
    mutate(file = sub(".arff", "", file))%>%
    mutate(algo = factor(algo,
                         levels = c("Just_POIs_FALSE", "Just_POIs_TRUE"),
                         labels = c("Claude", "FullSpace"))) %>%
    mutate(file = factor(file, 
                         levels = c("adult", "communities", "musk",
                                    "magic", "pendigits", "bank",
                                    "insurance", "breast", "letrec"),
                         labels = c("USCensus", "Crime", "MuskMolecules",
                                    "MAGICTelescope", "PenDigits", "BankMarketing",
                                    "Insurance", "BreastCancer", "LetterRecog")))


log_file <- log_file %>%
    filter(!file %in% black_list) %>%
    mutate(file = sub(".arff", "", file)) %>%
    filter(key == "Time") %>%
    mutate(algo = factor(algo,
                         levels = c("Just_POIs_FALSE", "Just_POIs_TRUE"),
                         labels = c("Claude", "FullSpace"))) %>%
    mutate(file = factor(file, 
                         levels = c("adult", "communities", "musk",
                                    "magic", "pendigits", "bank",
                                    "insurance", "breast", "letrec"),
                         labels = c("USCensus", "Crime", "MuskMolecules",
                                    "MAGICTelescope", "PenDigits", "BankMarketing",
                                    "Insurance", "BreastCancer", "LetterRecog")))


####################
# Plots the Scores #
####################
to_plot <- out_file %>%
            group_by(file, algo) %>%
            summarize(med_score = median(score),
                      min_score = min(score),
                      max_score = max(score))

all_combis <-  unique(merge(to_plot$file, to_plot$algo))
colnames(all_combis) <- c("file", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("algo", "file"))

g1 <- ggplot(to_plot, aes(x = file, y = med_score, 
                          ymin = min_score, ymax = max_score,
                          color = algo, fill = algo)) +
    geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
    scale_x_discrete(name="Dataset") +
    scale_y_continuous(name="POI Score (bits)")

g1 <- prettify(g1)+
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(g1)
ggsave("../documents/plots/tmp_POI-score.pdf", g1,
       width = 16, height = 4, units = "cm")

#####################
# Plots the runtimes #
#####################
to_plot <- log_file %>%
    spread(key, value)

all_combis <-  unique(merge(to_plot$file, to_plot$algo))
colnames(all_combis) <- c("file", "algo")
to_plot <- left_join(all_combis, to_plot, by =  c("algo", "file")) %>%
    mutate(xceeds = ifelse(is.na(Time), "X", ""))
to_plot[is.na(to_plot)] <- 3600

g2 <- ggplot(to_plot, aes(x = file, y = Time, color = algo, fill = algo, label = xceeds)) +
        geom_bar(position = "dodge", stat = "identity", width=0.5) +
        geom_text(aes(y=2800), color = "black", position = position_dodge(width=.5), size=3) +
        scale_x_discrete(name="Dataset") +
        scale_y_continuous(name="Execution Time (s)") +
        coord_cartesian(ylim = c(0,3600))

g2 <- prettify(g2)+
    theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(g2)
ggsave("../documents/plots/tmp_POI-timing.pdf", g2,
       width = 16, height = 4, units = "cm")

#system("cd ../documents ; ../documents/renderPDF.sh")