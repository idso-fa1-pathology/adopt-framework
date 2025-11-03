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
slides <- slides %>% mutate(`RunTime(H)` = `RunTime(sec)` / 3600.0)
slides <- slides %>% mutate(`StorageSpace(G)` = as.integer(`RunStorage(M)` / 1024.0))


# 2.1) Plot slide size  distribution ------------------------------------------
# Calculate median and 95th percentile
median_size <- median(slides$`SlideSize(M)`, na.rm = TRUE)
percentile_95 <- quantile(slides$`SlideSize(M)`, probs = 0.95, na.rm = TRUE)
slide_size_plot <- ggplot(slides, aes(SlideName, `SlideSize(M)`)) +
  geom_col(fill = "gray40") +
  geom_hline(yintercept = median_size, color = "royalblue", linetype = "dashed", size = 2) +
  geom_hline(yintercept = percentile_95, color = "coral", linetype = "dashed", size = 2) +
  labs(title = "Slide Image Size",
       x = NULL, y = "MB") +
  theme_minimal(base_size = 20) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),  # White plot background
    panel.background = element_rect(fill = "white", colour = NA)  # White panel background
  ) +
  # Add left-aligned text annotations
  annotate("text", x = -Inf, y = median_size, label = sprintf("Median: %.0f", median_size),
           hjust = 0, vjust = -0.5, color = "royalblue", size = 8) +
  annotate("text", x = -Inf, y = percentile_95, label = sprintf("95th Percentile: %.0f", percentile_95),
           hjust = 0, vjust = -0.5, color = "coral", size = 8)
# print(slide_size_plot)
slide_size_plot_path <- file.path(simulation_fig_dir, "slide_size_ditribution.png")
ggsave(filename = slide_size_plot_path, plot = slide_size_plot, 
       width = 20, height = 5, units = "in", dpi = 600)


# 2.2) Plot run time distribution ---------------------------------------------
median_time <- median(slides$`RunTime(H)`, na.rm = TRUE)
percentile95_time <- quantile(slides$`RunTime(H)`, probs = 0.95, na.rm = TRUE)
run_time_plot <- ggplot(slides, aes(SlideName, `RunTime(H)`)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = median_time, color = "royalblue", linetype = "dashed", size = 2) +
  geom_hline(yintercept = percentile95_time, color = "coral", linetype = "dashed", size = 2) +
  labs(title = "Run Time",
       x = NULL, y = "Hour") +
  theme_minimal(base_size = 20) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),  # White plot background
    panel.background = element_rect(fill = "white", colour = NA)  # White panel background
  ) +
  # Add left-aligned text annotations
  annotate("text", x = -Inf, y = median_time, label = sprintf("Median: %.1f", median_time),
           hjust = 0, vjust = -0.5, color = "royalblue", size = 8) +
  annotate("text", x = -Inf, y = percentile95_time, label = sprintf("95th Percentile: %.1f", percentile95_time),
           hjust = 0, vjust = -0.5, color = "coral", size = 8)
# print(run_time_plot)
run_time_plot_path <- file.path(simulation_fig_dir, "run_time_ditribution.png")
ggsave(filename = run_time_plot_path, plot = run_time_plot,
       width = 20, height = 5, units = "in", dpi = 600)


# 2.3) Plot run storage distribution ----------------------------------------
median_storage <- median(slides$`StorageSpace(G)`, na.rm = TRUE)
percentile95_storage <- quantile(slides$`StorageSpace(G)`, probs = 0.95, na.rm = TRUE)
run_storage_plot <- ggplot(slides, aes(SlideName, `StorageSpace(G)`)) +
  geom_col(fill = "mediumpurple") +
  geom_hline(yintercept = median_storage, color = "royalblue", linetype = "dashed", size = 2) +
  geom_hline(yintercept = percentile95_storage, color = "coral", linetype = "dashed", size = 2) +
  labs(title = "Storage Space",
       x = NULL, y = "GB") +
  theme_minimal(base_size = 20) +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  theme(
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),  # White plot background
    panel.background = element_rect(fill = "white", colour = NA)  # White panel background
  )+
  # Add left-aligned text annotations
  annotate("text", x = -Inf, y = median_storage, label = sprintf("Median: %.1f", median_storage),
           hjust = 0, vjust = -0.5, color = "royalblue", size = 8) +
  annotate("text", x = -Inf, y = percentile95_storage, label = sprintf("95th Percentile: %.1f", percentile95_storage),
           hjust = 0, vjust = -0.5, color = "coral", size = 8)
# print(run_storage_plot)
run_storage_plot_path <- file.path(simulation_fig_dir, "run_storage_ditribution.png")
ggsave(filename = run_storage_plot_path, plot = run_storage_plot,
       width = 20, height = 5, units = "in", dpi = 600)