# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../../include/MyTemplate.R")
library(ggplot2)
library(ggbreak)
library(readxl)
library(extrafont)
library(showtext)
library(gg.gap)
font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()
windowsFonts(Arial=windowsFont("Arial"))
# 准备数据
COD = c(37441.2813, 4700.4758, 1816.5276, 1118.77845)
CDC = c(64766.7393, 6925.21205, 1847.4916, 787.54015)

# 基本使用 - 用默认值
my_plot <- create_bandwidth_chart(
  COD_values = COD,
  CDC_values = CDC
)

# 显示图表
print(my_plot)

# 设置坐标轴范围
my_plot2 <- create_bandwidth_chart(
  COD_values = COD,
  CDC_values = CDC,
  x_limits = c(-0.4, 3.4),
  y_limits = c(3460, 73000)
)

# 保存到特定位置
create_bandwidth_chart(
  COD_values = COD,
  CDC_values = CDC,
  export_path = "./",
  export_name = "bandwidth_comparison.pdf"
)