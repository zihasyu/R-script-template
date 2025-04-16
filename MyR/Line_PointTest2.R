# 创建一个较大的示例数据集
source("./Line_Point.R")
set.seed(123)
x_values <- 1:100
data_large <- data.frame(
  Time = x_values,
  Method1 = cumsum(rnorm(100, mean = 5, sd = 10)),
  Method2 = cumsum(rnorm(100, mean = 3, sd = 8)),
  Method3 = cumsum(rnorm(100, mean = 7, sd = 12))
)

# 使用函数绘制图表，每条线只显示10个均匀分布的点
plot_line_with_selected_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  num_points = 10,  # 每条线上显示10个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "large_dataset_plot.pdf"
)

# 增加点数，查看效果
plot_line_with_selected_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  num_points = 20,  # 每条线上显示20个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "large_dataset_more_points.pdf",
  show_legend = TRUE
)

# 使用更少的点，只显示关键点
plot_line_with_selected_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  num_points = 5,  # 每条线上只显示5个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "large_dataset_fewer_points.pdf"
)

