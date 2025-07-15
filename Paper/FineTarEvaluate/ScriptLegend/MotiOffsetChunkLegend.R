library(here)
library(ggplot2)
library(cowplot)  # 用于提取图例

source(here("MyR", "MyR.R"))
# 使用示例
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
series_names <- c("Automake", "Gcc", "Linux", "Web")
create_legend_pdf(
  series_names = series_names,
  export_path = "./",
  export_name = "my_legend.pdf",
  legend_title = "",
  legend_position = "horizontal",  # 或 "vertical"
    # colors = c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF"),
  plot_width = 8,
  plot_height =0.5,
  text_size = 38,
  title_size = 38
)