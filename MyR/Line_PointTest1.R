# 示例：使用原始两条线数据
source("./Line_Point.R")
data <- data.frame(
  Clients = 1:10,
  TreeThroughput = c(732.2491481, 952.408246, 1029.554317, 1013.542929, 
                    1024.616102, 992.3178544, 1026.550428, 980.5707219, 
                    937.0588234, 862.7067682),
  UniqueThroughput = c(302.955622, 548.5730204, 629.5684444, 700.9250037, 
                      737.803191, 738.1069898, 732.2160773, 735.9434862, 
                      727.8515278, 697.9939864)
)

# 基本使用 - 无图例
plot_line_with_points(
  data,
  colors = c("#AD0626", "#2C3359"),
  shapes = c(23, 24),
  x_label = "Number of clients",
  y_label = "Download (MiB/s)",
  export_name = "download_comparison.pdf",
  x_lim = c(1.0, 10),
  y_lim = c(0, 1080),
  x_breaks = c(1, 5, 10, 14),
  y_breaks = c(0, 500, 1000)
)

# 显示带顶部图例的图表
plot_line_with_points(
  data,
  colors = c("#AD0626", "#2C3359"),
  shapes = c(23, 24),
  column_names = c("Tree Structure", "Unique Structure"),
  x_label = "Number of clients",
  y_label = "Download (MiB/s)",
  export_name = "download_with_legend.pdf",
  show_legend = TRUE,
  legend_position = "top",
  legend_text_size = 32
)