library(pROC)
library(patchwork)
library(tidyverse)
library(ggrepel)

# Load plot theme and colors
source("plot_theme.R")

# Simulation
# Here we simulate data to show what the data selection in the model development
# does to the performance of a model that only uses MAP as a predictor.
# The specific parameters of the simulation are simply chosen to place most
# data in a relevant interval: between 55 and 95 mmHg.
# The correlation coefficient (r) is chosen to give MAP a modest predictive power,
# similar to what is presented by Davies et al. (http://doi.org/10.1213/ANE.0000000000004121)
# However, any correlation coefficient between 0 and ~0.9 will convey the intended message.
set.seed(1)
correlated_map <- faux::rnorm_multi(n = 1000, 
                                    mu = c(75, 75), 
                                    sd = c(10, 10),
                                    r = 0.6,
                                    # "map_predictor" represents the current map value
                                    # "map_outcome" represents what map is 5 minutes from now.
                                    varnames = list("map_predictor", "map_outcome")
                                    )



# Add definitions of events and zones.
d <- as_tibble(correlated_map) %>% 
    mutate(
        # If map_outcome is < than 65, this is represented as a hypotensive event
        hypo = map_outcome < 65,
        # If map_outcome is between 65 and 75, this is a greyzone event.
        grey = map_outcome >= 65 & map_outcome < 75,
        # If map_outcome and map_predictor is >= 75, this is a nonevent. 
        nonhypo = map_outcome >= 75 & map_predictor >= 75,
        ) %>% 
    filter(
        # We remove points where map is below 65 in the predictor
        map_predictor > 65,
           
        # Remove extreme points simply to reduce white space in the plots.
        # This simulation is not intended to mimic real data.
        map_predictor < 105,
        map_outcome > 45,
        map_outcome < 105
        )

# Here is a plot to show how we have labelled the data.
ggplot(d, aes(map_predictor, map_outcome, color = case_when(hypo ~ "Hypo",
                                                            grey ~ "Grey zone",
                                                            nonhypo ~ "Non-hypo",
                                                            NA ~ "Excluded"))) +
    geom_point() +
    labs(color = "label")

## Now we can make the ROC analyses

# With all data
roc_analysis_full <- roc(hypo~map_predictor, data = d)
# Excluding grey zone
roc_analysis_grey <- roc(hypo~map_predictor, data = filter(d, !grey))
# Excluding data that is neither event or non-event per model-development definition
roc_analysis_biased <- roc(hypo~map_predictor, data = filter(d, hypo | nonhypo))

# BONUS: We can simulate something that looks like the HPI roc curve, simply by adding noise to MAP.
roc_analysis_biased_hpi <- roc(hypo~ hpi, 
                             data = filter(d, hypo | nonhypo) %>% 
                                 mutate(hpi = map_predictor + rnorm(n(), sd = 3)))

plot(roc_analysis_full)
plot(roc_analysis_grey, col = "red", add = TRUE)
plot(roc_analysis_biased, col = "green", add = TRUE)
plot(roc_analysis_biased_hpi, lty = 2, add = TRUE)
legend(x = 0.5, y = 0.4, legend = c("Full analysis", "No grey zone", "HPI development selection",
                  "HPI development selection + noise"),
       lty = c(1,1,1,2),
       col = c("black", "red", "green", "black"))  

## The remaining code makes combines this analysis into a paneled figure ## 

# First we extract all combinations of sensitivity, specificity and thresholds for  
# all ROC analyses
roc_data_full <- as.data.frame(roc_analysis_full[c("sensitivities", 
                                                   "specificities", 
                                                   "thresholds")]) %>% 
    arrange(desc(specificities), sensitivities)

roc_data_grey <- as.data.frame(roc_analysis_grey[c("sensitivities", 
                                                   "specificities", 
                                                   "thresholds")]) %>% 
    arrange(desc(specificities), sensitivities)

roc_data_hpi_paper <- as.data.frame(roc_analysis_biased[c("sensitivities", 
                                                   "specificities", 
                                                   "thresholds")]) %>% 
    arrange(desc(specificities), sensitivities)

# We extract the rows with thresholds closest to 70, 75 and 80
# This is used for labeling the ROC curves.
roc_label_filter <- function(roc_data) {
    roc_data %>%
        filter(
            row_number() == which.min(abs(thresholds - 70)) |
                row_number() == which.min(abs(thresholds - 75)) |
                row_number() == which.min(abs(thresholds - 80))
        )
}

roc_labels_full <- roc_label_filter(roc_data_full)
roc_labels_grey <- roc_label_filter(roc_data_grey)
roc_labels_hpi_paper <- roc_label_filter(roc_data_hpi_paper)

# Make the 3 ROC panels
roc_plot_full <- ggplot(roc_data_full, aes(1-specificities, sensitivities)) +
    geom_step() +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
    geom_label_repel(aes(label = glue::glue("MAP[now] < {round(thresholds, 0)}")),
                    parse = TRUE,
                    size = 3.1,
                    box.padding = 0.1,
                    point.size = 0.1,
                    segment.color = "#555555",
                    position = position_nudge_repel(y = -0.2, x=0.2),
                    ylim = c(0.15, 1),
                    direction = "y",
                    label.size  = NA,
                    #vjust = 1, 
                    hjust = 0,
                    family = "Helvetica",
                    data = roc_labels_full
    ) +
    annotate("label",
             x = 1, y = 0, 
             label = glue::glue("AUC = {round(auc(roc_analysis_full), 2)}"),
             hjust = 1, vjust = -0,
             label.size = NA,
             family = "Helvetica",
             #label.size = NA,
             size = 3.5) + 
    labs(x = "1 - specificity", y = "Sensitivity") +
    coord_equal()

roc_plot_grey <- ggplot(roc_data_grey, aes(1-specificities, sensitivities)) +
    geom_step() +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
    geom_label_repel(aes(label = glue::glue("MAP[now] < {round(thresholds, 0)}")),
                     parse = TRUE,
                     size = 3.1,
                     box.padding = 0.1,
                     point.size = 0.1,
                     segment.color = "#555555",
                     direction = "y",
                     position = position_nudge_repel(y = -0.2, x=0.2),
                     ylim = c(0.15, 1),
                     label.size  = NA,
                     #vjust = 1, 
                     hjust = 0,
                     family = "Helvetica",
                     data = roc_labels_grey
    ) +
    annotate("label",
             x = 1, y = 0, 
             label = glue::glue("AUC = {round(auc(roc_analysis_grey), 2)}"),
             hjust = 1, vjust = -0,
             label.size = NA,
              family = "Helvetica",
              #label.size = NA,
              size = 3.5) + 
    labs(x = "1 - specificity", y = "Sensitivity") +
    coord_equal()

roc_plot_hpi_paper <- ggplot(roc_data_hpi_paper, aes(1-specificities, sensitivities)) +
    geom_step() +
    geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey") +
    geom_label_repel(aes(label = glue::glue("MAP[now] < {round(thresholds, 0)}")),
                    parse = TRUE,
                    size = 3.1,
                    box.padding = 0.1,
                    point.size = 0.3,
                    segment.color = "#555555",
                    position = position_nudge_repel(y = -0.1, x=0.2),
                    ylim = c(0.15, 1),
                    direction = "y",
                    label.size  = NA,
                    #vjust = 1, 
                    hjust = 0,
                    family = "Helvetica",
                    data = roc_labels_hpi_paper
    ) +
    annotate("label",
             x = 1, y = 0, 
             label = glue::glue("AUC = {round(auc(roc_analysis_biased), 2)}"),
             hjust = 1, vjust = -0,
             label.size = NA,
             family = "Helvetica",
             size = 3.5) + 
    labs(x = "1 - specificity", y = "Sensitivity") +
    coord_equal()

# Make the 3 scatter plots
color_scale <- scale_color_manual(values = plot_colors,
                                  labels = c("Non-hypotensive events", "Hypotensive events"))

labels <- d %>% 
    mutate(label = ifelse(hypo, "Hypotensive events", "Non-hypotensive events")) %>% 
    group_by(label) %>% 
    summarise(across(starts_with("map"), .fns = mean), hypo = first(hypo))

scatter_plot_full <- ggplot(d, aes(map_predictor, map_outcome, color = hypo)) + 
    geom_point(size = 0.7, show.legend = FALSE) +
    geom_hline(yintercept = 65, color = "grey") +
    scale_y_continuous(breaks = seq(55, 95, by = 10)) + 
    scale_x_continuous(breaks = seq(65, 95, by = 10)) +
    
    labs(x = "MAP<sub>now</sub> (predictor) [mmHg]", y = "MAP in 5 minutes (outcome) [mmHg]", color = "") + 
    ggtitle("**A** Full analysis") +
    color_scale +
    coord_equal() +
    theme(axis.title.x = ggtext::element_markdown())

scatter_plot_full_w_labs <- scatter_plot_full
scatter_plot_full_w_labs$layers <- c(
    geom_label_repel(aes(label = label),
                     size = 3.3,
                     box.padding = 0,
                     point.size = 0.3,
                     point.padding = 0,
                     min.segment.length = 0,
                     segment.color = "#555555",
                     position = position_nudge_repel(y = c(-100, 42), x= c(100, -100)),
                     label.size  = NA,
                     #vjust = 1, 
                     force = 1000,
                     hjust = 0,
                     family = "Helvetica",
                     data = labels,
                     show.legend = FALSE
    ),
    scatter_plot_full$layers
)

scatter_plot_full_w_labs

scatter_plot_grey <- scatter_plot_full_w_labs %+% filter(d, !grey) +
    geom_hline(yintercept = 75, color = "grey") +
    ggtitle("**B** Events where MAP (outcome) is between 65 and 75 mmHg (“gray zone”) are excluded") 
               
scatter_plot_grey

scatter_plot_hpi_paper <- scatter_plot_grey %+% filter(d, hypo | nonhypo) +
    geom_vline(xintercept = 75, color = "grey") +
    ggtitle("**C** Samples corresponding to non-hypotensive events must have a MAP (predictor) ≥ 75 mmHg") 

scatter_plot_hpi_paper

# Combine plots
scatter_plot_full_w_labs + scatter_plot_grey + scatter_plot_hpi_paper +
    roc_plot_full + roc_plot_grey + roc_plot_hpi_paper + 
    plot_layout(ncol = 3, byrow = TRUE, guides = "collect") 
#ggsave("figs/simulation2.png", width = 8, device = ragg::agg_png, bg = "white")
#ggsave("figs/simulation2.tiff", width = 8, bg = "white")
ggsave("figs/figure2_simulation.pdf", device = cairo_pdf, width = 9, height = 6.5)
