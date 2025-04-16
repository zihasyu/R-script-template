# 创建一个较大的示例数据集
source("Line_Point.R")
set.seed(123)
x_values <- 1:100
data_large <- data.frame(
  Time = x_values,
  Method1 = cumsum(rnorm(100, mean = 5, sd = 10)),
  Method2 = cumsum(rnorm(100, mean = 3, sd = 8)),
  Method3 = cumsum(rnorm(100, mean = 7, sd = 12))
)

# 使用函数绘制图表，只使用10个均匀抽样的点来绘制线条和标记
plot_with_sampled_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  sample_size = 10,  # 每条线抽样10个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "sampled_plot_10points.pdf"
)

# 增加抽样点数，查看效果
plot_with_sampled_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  sample_size = 20,  # 每条线抽样20个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "sampled_plot_20points.pdf",
  show_legend = TRUE
)

# 使用较少的点，更加简化
plot_with_sampled_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B", "Algorithm C"),
  sample_size = 5,  # 每条线只抽样5个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_name = "sampled_plot_5points.pdf"
)