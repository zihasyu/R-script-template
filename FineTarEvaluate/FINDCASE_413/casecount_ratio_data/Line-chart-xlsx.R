library(ggplot2)
library(cowplot)
require(RColorBrewer)
require(scales)
library(readxl)
library(tidyr)
library(dplyr)
# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

create_line_plot <- function(df) {
  # 计算每个 source 应该显示的间隔
  df_with_interval <- df %>%
    group_by(source) %>%
    mutate(total_points = n(),
           interval = if(first(total_points) <= 15) 1 else max(floor(first(total_points) / 15), 1),
           # 如果一个 source 的总点数小于或等于 15，我们设置间隔为 1，这意味着会显示所有的点。
           # 在 show_point 的计算中，我们添加了一个条件 total_points <= 15。这确保了如果一个 source 的总点数小于或等于 15，所有的点都会被显示。
           # 对于点数超过 15 的 source，我们仍然使用之前的逻辑来选择要显示的点。
           show_point = row_number() %% interval == 1 | row_number() == total_points | total_points <= 15) %>%
    ungroup()
  
  p <- ggplot(df_with_interval, aes(x = x_value, y = y_value, color = source, shape = source, linetype = source)) +
    scale_shape_manual(values = c(23, 25, 24, 21, 22)) +
    scale_colour_brewer(palette = "Set1") + 
    scale_linetype_manual(values = c(1, 1, 1, 1, 1)) +              
    geom_line(size = 1.5) +
    geom_point(data = df_with_interval %>% filter(show_point),
               size = 6, stroke = 2, fill = "white") + 
    theme_bw() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 19), 
      legend.key.height = unit(1.8, "line"),
      legend.key.width = unit(2, "line"),
      legend.direction = "horizontal",
      legend.text.align = 0,
      legend.margin = margin(t = 0, unit = 'cm'),
      legend.title = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0, 0.1), "cm"),
      axis.title.x = element_text(size = 24), 
      axis.title.y = element_text(size = 24),  
      axis.text = element_text(size = 22, color = "black"), 
      aspect.ratio = 0.5, 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    labs(x = 'Number of versions', y = 'Number of SFs')
  return(p)
}

# 读取 Excel 文件
df <- read_excel("../data/9-SF.xlsx")

# 将数据转换为长格式
df_long <- df %>%
  pivot_longer(cols = -x_value, 
               names_to = "source", 
               values_to = "y_value")

# 设置 source 的因子级别
df_long$source <- factor(df_long$source, 
                         levels = c('Finesse', 'Odess', 'NTrans', 'Palantir', 'BiSearch'))

p <- create_line_plot(df_long)

# 保存图表到文件
ggsave('Line-chart-xlsx.pdf', plot = p, height = 5, width = 10)

print(p)