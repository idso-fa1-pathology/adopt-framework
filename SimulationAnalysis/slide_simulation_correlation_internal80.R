# 0) Setup environment 
### library prepration
library(readxl)
library(tidyverse)
library(tibble)
library(ggplot2)
library(ggpmisc)
library(dplyr)

data_set <- "Internal-80"

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
slides <- slides %>% mutate(`StorageSpace(G)` = as.integer(`RunStorage(M)` / 1024.0))
slides <- slides %>% mutate(`RunTime(H)` = `RunTime(sec)` / 3600.0)


# Function to create a correlation plot with dual y-axes and user-specified max limits
create_correlation_plot_dual <- function(data, x_var, y1_var, y2_var, y1_lim = NULL, y2_lim = NULL) {
  # Extract vectors
  x <- data[[x_var]]
  y1 <- data[[y1_var]]
  y2 <- data[[y2_var]]
  
  # Compute correlations and p-values
  cor_y1 <- cor.test(x, y1)
  cor_y2 <- cor.test(x, y2)
  
  # Create labels for annotations
  label_y1 <- paste0(y1_var, ": R = ", round(cor_y1$estimate, 3), 
                     ", p = ", format.pval(cor_y1$p.value, digits = 3))
  label_y2 <- paste0(y2_var, ": R = ", round(cor_y2$estimate, 3), 
                     ", p = ", format.pval(cor_y2$p.value, digits = 3))
  
  # Compute ranges for axes
  xlim <- range(x, na.rm = TRUE)
  ylim1 <- if (!is.null(y1_lim)) {
    c(min(y1, na.rm = TRUE), y1_lim)  # Use user-specified max for y1
  } else {
    range(y1, na.rm = TRUE)
  }
  ylim2 <- if (!is.null(y2_lim)) {
    c(min(y2, na.rm = TRUE), y2_lim)  # Use user-specified max for y2
  } else {
    range(y2, na.rm = TRUE)
  }
  
  # Transformation coefficients to map y2 to y1 range for plotting
  b <- diff(ylim1) / diff(ylim2)
  a <- ylim1[1] - b * ylim2[1]
  
  # Define colors for each variable
  colors <- c("coral", "royalblue")
  
  names(colors) <- c(y1_var, y2_var)
  
  # Create the plot
  p <- ggplot(data, aes(x = .data[[x_var]])) +
    # Points and regression line for y1 (left axis)
    geom_point(aes(y = .data[[y1_var]], color = y1_var), alpha = 0.5) +
    geom_smooth(aes(y = .data[[y1_var]], color = y1_var), method = "lm", se = FALSE) +
    # Points and regression line for y2 (right axis, transformed to left axis scale)
    geom_point(aes(y = a + .data[[y2_var]] * b, color = y2_var), alpha = 0.5) +
    geom_smooth(aes(y = a + .data[[y2_var]] * b, color = y2_var), method = "lm", se = FALSE) +
    # Define left and right y-axes with limits
    scale_y_continuous(
      name = y1_var,  # Left y-axis label
      limits = ylim1,  # Apply y1 limits
      sec.axis = sec_axis(~ (. - a) / b, name = y2_var, breaks = seq(ylim2[1], ylim2[2], length.out = 5))  # Right y-axis with y2 limits
    ) +
    scale_color_manual(name = "Variables", values = colors) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.title.y.left = element_text(color = colors[1]),
      axis.text.y.left = element_text(color = colors[1]),
      axis.title.y.right = element_text(color = colors[2]),
      axis.text.y.right = element_text(color = colors[2])
    ) +
    labs(x = x_var)
  
  # Add correlation annotations
  dx <- diff(xlim) * 0.05
  dy <- diff(ylim1) * 0.05
  p <- p +
    annotate("text", x = xlim[1] + dx, y = ylim1[2] - dy, 
             label = label_y1, color = colors[1], hjust = 0, vjust = 1) +
    annotate("text", x = xlim[2] - dx, y = ylim1[2] - dy, 
             label = label_y2, color = colors[2], hjust = 1, vjust = 1)
  
  return(p)
}

# Create the plot
correlation_plot <- create_correlation_plot_dual(slides, "SlideSize(M)", "RunTime(H)", "StorageSpace(G)",
                                                 y1_lim = 6.8, y2_lim = 118)
# print(correlation_plot)
correlation_plot_path <- file.path(simulation_fig_dir, "slide_run_correlation.pdf")
ggsave(filename = correlation_plot_path, plot = correlation_plot,
       width = 7, height = 6, units = "in", dpi = 300)
