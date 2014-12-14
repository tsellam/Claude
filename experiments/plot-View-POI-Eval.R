library("dplyr")
library("tidyr")
source("graph-utils.R")

# !!!!!!!!!!! WARNING !!!!!!!!!!! #
# THERE IS A FILE REPAIR FUNCTION #
# !!!!!!!!!!! WARNING !!!!!!!!!!! #


# Reading, preprocessing and so on
log_file <- read.delim("~/Projects/TurboGroups/experiments/results-12:12/agg.view+poi.log")
log_file <- log_file %>%
    #group_by(file, q, k, size, key) %>%
    #mutate(algo = c("ViewSearch + POI", "POI")[row_number()]) %>%
    #ungroup %>%
    mutate(algo = factor(algo,
                         levels = c("Just_POIs_FALSE", "Just_POIs_TRUE"), 
                         labels = c("ViewSearch + POI", "POI"))) %>%
    filter( (q==10 & k==10) | (q==25 & k==25)  ) %>%
    mutate(file = sub(".arff", "", file),
           setup = paste0("q = ", q, ", k = ", k),
           size = factor(size,
                         levels = unique(sort(size)),
                         labels = paste(unique(sort(size)), " dimensions")))

out_file <- read.delim("~/Projects/TurboGroups/experiments/results-12:12/agg.view+poi.out")
out_file <- out_file %>%
    #group_by(file, q, k, size, view) %>%
    #mutate(algo = c("ViewSearch + POI", "POI")[row_number()]) %>%
    #data.frame %>%
    mutate(algo = factor(algo,
                         levels = c("Just_POIs_FALSE", "Just_POIs_TRUE"), 
                         labels = c("ViewSearch + POI", "POI"))) %>%
    select(-description) %>%
    filter( (q==10 & k==10) | (q==25 & k==50)  ) %>%
    mutate(file = sub(".arff", "", file),
           setup = paste0("q = ", q, ", k = ", k),
           size = factor(size,
                          levels = unique(sort(size)),
                          labels = paste(unique(sort(size)), " dimensions")))
    
#####################
# Plotting accuracy #
#####################
no_view <- out_file %>%
    filter(algo == "POI") %>%
    select(-view, -score, -algo) %>%
    distinct()
out_file <- out_file %>% semi_join(no_view)

acc_plot <- ggplot(out_file, aes(x = factor(file), y = score, fill = algo, color = algo)) +
            geom_boxplot(outlier.colour = "darkgrey", outlier.size = .5, ) +
            facet_grid(size ~ setup, scale = "free") +
            scale_x_discrete(name = "Dataset") +
            scale_y_continuous(name = "POI Divergence")
acc_plot <- prettify(acc_plot)+
    theme(legend.position = "top", axis.text.x = element_text(angle = 22, hjust = 1))

plot(acc_plot)

##################
# Plotting SPEED #
##################
log_file <- log_file %>%
                filter(key == "Time") %>%
                mutate(time = value, xceed = round(time)) %>%
                select(-key, -value)

all_matches <- log_file %>%
                select(-algo, -time, -xceed) %>%
                distinct()

no_match <- all_matches %>%
                anti_join(filter(log_file, algo == "POI"))%>%
                mutate(algo = "POI", time = 1999, xceed = ">1999")
log_file <- rbind(log_file, no_match)

log_file <- log_file %>%
            group_by(file, size, setup) %>%
            mutate(time = time / max(time)) %>%
            ungroup


time_plot <- ggplot(log_file, aes(x = factor(file),
                                  y = time * 100,
                                  fill = algo,
                                  color = algo)) +
                geom_bar(stat = "identity", position = "dodge") +
                geom_text(aes(label = xceed, y = time * 100 / 2), 
                          position = position_dodge(width = 0.9),
                          color = "black",
                          size=2) +
                facet_grid(size ~ setup, scale = "free") +
                scale_x_discrete(name = "Dataset") +
                scale_y_continuous(name = "Runtime (% max)", breaks = c(0,100))

time_plot <- prettify(time_plot) +
    theme(legend.position = "top", axis.text.x = element_text(angle = 22, hjust = 1))

plot(time_plot)

ggsave("../documents/plots/tmp_View-POI-Acc.pdf", acc_plot, width = 8.5, height = 2.25)
ggsave("../documents/plots/tmp_View-POI-Time.pdf", time_plot, width = 8.5, height = 2.25)