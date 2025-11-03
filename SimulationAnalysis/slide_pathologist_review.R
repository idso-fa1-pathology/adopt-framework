# 0) Setup environment 
### library prepration
library(readxl)
library(tidyverse)

# data_set <- "Internal-80"
data_set <- "External-86" 

### directory prepration
simulation_dir <- "/Volumes/idso_fa1_pathology/TIER2/ping-aistil/simulation-analysis"
setwd(simulation_dir)
simulation_fig_dir <- file.path(simulation_dir, "figures", data_set)
if (!dir.exists(simulation_fig_dir)) {
  dir.create(simulation_fig_dir, recursive = TRUE)
}

# 1) load & organize data 
slides <-read_excel(file.path(simulation_dir, "data", sprintf("MDA-TNBC-%s-AIsTIL-Review.xlsx", data_set)), sheet = 1)
slides <- slides %>% 
  mutate(`R1-PathReview` = factor(
    `R1-PathReview`,
    levels = c("Accept", "TissueUnsuitable", "Reject") 
  )) # factor into three levels
slides <- slides %>% 
  mutate(`R2-PathReview` = factor(
    `R2-PathReview`,
    levels = c("Accept", "TissueUnsuitable", "Reject") 
  )) # factor into three levels

slides <- slides %>% arrange(`SlideSize(M)`) # smallest → largest
slides <- slides %>% mutate(SlideName = factor(SlideName, levels = SlideName))

# 2) Plot pathologist R1's time amd decision ---------------------------------------------
# Calculate median and 95th percentile
p1_median_time <- median(slides$`R1-ReviewTime(sec)`, na.rm = TRUE)
p1_percentile95_time <- quantile(slides$`R1-ReviewTime(sec)`, probs = 0.95, na.rm = TRUE)
p1_review_plot <- ggplot(slides, aes(x = SlideName)) +
  geom_segment(aes(x = SlideName, xend = SlideName, y = 0, yend = `R1-ReviewTime(sec)`, color = `R1-PathReview`)) +  # Vertical lines
  geom_point(aes(y = `R1-ReviewTime(sec)`, color = `R1-PathReview`), size = slides$`R1-ReviewTime(sec)`/300.0) + 
  scale_color_manual(values = c("Accept" = "#00A651", "TissueUnsuitable" = "#0078D4", "Reject" = "#D32F2F")) +  
  labs(color = "") +  # Removes the legend title
  theme_minimal(base_size = 16) +
  labs(title = "Patholgoist1 - Review Time & Decision", x = NULL, y = "Time per slide (Seconds)") +
  ylim(0, 1100) + 
  theme(legend.position = "top") + 
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 24),      # Change legend text size
  )  
  # # Add horizontal lines for median and 95th percentile
  # geom_hline(yintercept = p1_median_time, linetype = "dashed", color = "blue", size = 2) +
  # geom_hline(yintercept = p1_percentile95_time, linetype = "dashed", color = "red", size = 2) +
  # # Add annotations for clarity
  # annotate("text", x = -Inf, y = p1_median_time, label = sprintf("Median: %.0f s", p1_median_time), 
  #          hjust = 0, vjust = -0.5, size = 8, color = "blue") +
  # annotate("text", x = -Inf, y = p1_percentile95_time, label = sprintf("95th Percentile: %.0f s", p1_percentile95_time), 
  #          hjust = 0, vjust = -0.5, size = 8, color = "red")
# print(p1_review_plot)
p1_review_plot_path <- file.path(simulation_fig_dir, "p1_review_ditribution.pdf")
ggsave(filename = p1_review_plot_path, plot = p1_review_plot,
       width = 7, height = 6, units = "in", dpi = 300)


# 3) Plot pathologist R2's time amd decision ---------------------------------------------
# Calculate median and 95th percentile
p2_median_time <- median(slides$`R2-ReviewTime(sec)`, na.rm = TRUE)
p2_percentile95_time <- quantile(slides$`R2-ReviewTime(sec)`, probs = 0.95, na.rm = TRUE)
p2_review_plot <- ggplot(slides, aes(x = SlideName)) +
  geom_segment(aes(x = SlideName, xend = SlideName, y = 0, yend = `R2-ReviewTime(sec)`, color = `R2-PathReview`)) +  # Vertical lines
  geom_point(aes(y = `R2-ReviewTime(sec)`, color = `R2-PathReview`), size = slides$`R2-ReviewTime(sec)`/300.0) + 
  scale_color_manual(values = c("Accept" = "#00A651", "TissueUnsuitable" = "#0078D4", "Reject" = "#D32F2F")) +  
  labs(color = "") +  # Removes the legend title
  theme_minimal(base_size = 16) +
  labs(title = "Patholgoist2 - Review Time & Decision", x = NULL, y = "Time per slide (Seconds)") +
  ylim(0, 1100) + 
  theme(legend.position = "top") + 
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(), 
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 24),      # Change legend text size
  )  
# # Add horizontal lines for median and 95th percentile
# geom_hline(yintercept = p2_median_time, linetype = "dashed", color = "blue", size = 2) +
# geom_hline(yintercept = p2_percentile95_time, linetype = "dashed", color = "red", size = 2) +
# # Add annotations for clarity
# annotate("text", x = -Inf, y = p2_median_time, label = sprintf("Median: %.0f s", p2_median_time), 
#          hjust = 0, vjust = -0.5, size = 8, color = "blue") +
# annotate("text", x = -Inf, y = p2_percentile95_time, label = sprintf("95th Percentile: %.0f s", p2_percentile95_time), 
#          hjust = 0, vjust = -0.5, size = 8, color = "red")
# print(p2_review_plot)
p2_review_plot_path <- file.path(simulation_fig_dir, "p2_review_ditribution.pdf")
ggsave(filename = p2_review_plot_path, plot = p2_review_plot,
       width = 7, height = 6, units = "in", dpi = 300)