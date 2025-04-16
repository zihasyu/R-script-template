library(ggplot2)
library(cowplot)
require(RColorBrewer)
require(scales)
library(readxl)
library(tidyr)
library(dplyr)
# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 定义生成普通折线图的函数
create_line_plot <- function(df) {
  p <- ggplot(df, aes(x = x_value, y = y_value, color = source, shape = source, linetype = source)) +
    scale_shape_manual(values = c(23)) +
    scale_colour_brewer(palette = "Set1") + 
    scale_linetype_manual(values = c(1)) +              
    geom_line(size = 1.5) +
    geom_point(size = 6, stroke = 2, fill = "white") + 
    theme_bw() +
    theme(
      legend.position = "",
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
    labs(x = 'Number of chunks', y = 'Reduction size')
  return(p)
}

# 手动输入数据
x_values <- seq(1, 8, by = 1)
y_values <- c(
  0,3991,	7057,9793,	12099	,13825	,15902	,17894
)

df <- data.frame(
  x_value = rep(x_values, 1),
  y_value = y_values,
  source = factor(rep(c('NTrans'), each = length(x_values)),
                  levels = c('NTrans')) # 设置顺序
)

# 绘制折线图

p <- create_line_plot(df)

# 保存图表到文件
ggsave('pic1-Line-chart.pdf', plot = p, height = 5, width = 10)

print(p)