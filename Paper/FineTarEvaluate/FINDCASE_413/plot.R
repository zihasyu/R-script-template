library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(scales)
library(tidyr)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read the CSV file
data <- read.csv("all_data.csv", header = TRUE)
font_add('Arial','C:/Windows/Fonts/arial.ttf')
# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
PerMask=8
sizeRatio=1.5

# Get the column names (first row)
col_names <- colnames(data)

# Convert data to long format with CDF calculation
df_long <- data %>%
  # Convert to long format
  pivot_longer(cols = everything(), 
               names_to = "source", 
               values_to = "y_value") %>%
  # Remove NA values
  filter(!is.na(y_value)) %>%
  # Group by source to calculate CDF
  group_by(source) %>%
  # Create normalized x-axis (0 to 1)
  mutate(x_value = row_number() / n()) %>%
  ungroup()

# Create line plot function
create_line_plot <- function(df) {
  # Calculate interval for displaying points
  df_with_interval <- df %>%
    group_by(source) %>%
    mutate(total_points = n(),
           interval = if(first(total_points) <= PerMask) 1 else max(floor(first(total_points) / PerMask), 1),
           show_point = row_number() %% interval == 1 | row_number() == total_points | total_points <= PerMask) %>%
    ungroup()
  
  p <- ggplot(df_with_interval, aes(x = x_value, y = y_value, color = source, shape = source, linetype = source)) +
    scale_shape_manual(values = c(23, 25, 24, 21)) +
    scale_colour_brewer(palette = "Set1") + 
    scale_linetype_manual(values = c(1, 1, 1, 1)) +              
    geom_line(size = 1.5*sizeRatio) +
    geom_point(data = df_with_interval %>% filter(show_point),
               size = 6*sizeRatio, stroke = 2, fill = "white") + 
    scale_x_continuous(labels = scales::percent_format()) +  # Format x-axis as percentage
    scale_y_continuous(labels = scales::percent_format()) +  # Format y-axis as percentage
    theme_bw() +
    theme(text = element_text(family = "Arial")) +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 19*sizeRatio), 
      legend.key.height = unit(1.8, "line"),
      legend.key.width = unit(2, "line"),
      legend.direction = "horizontal",
      legend.text.align = 0,
      legend.margin = margin(t = 0, unit = 'cm'),
      legend.title = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0, 0.1), "cm"),
      axis.title.x = element_text(size = 24*sizeRatio), 
      axis.title.y = element_text(size = 24*sizeRatio),  
      axis.text = element_text(size = 22*sizeRatio, color = "black"), 
      aspect.ratio = 0.5, 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    labs(x = 'CDF For Version', y = 'FRP')
  return(p)
}

# Create the plot
p <- create_line_plot(df_long)

# Save the plot
ggsave('CDF-chart-csv.pdf', plot = p, height = 6.2, width = 10)

print(p)