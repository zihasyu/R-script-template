#Test for Line chart of cdf x-axis
source("./Line_NoPoint.R")
test_cdf_data <- list()
test_cdf_data$System1 <- c(120, 180, 220, 280, 350, 410, 480, 550, 600, 650)
test_cdf_data$System2 <- c(200, 280, 350, 390, 420, 450, 470)
test_cdf_data$System3 <- c(150, 220, 270, 320, 370, 410, 430, 460)
test_cdf_data$System4 <- c(300, 320, 340, 370, 410, 450)

# 不要转换为数据框，直接使用列表
# 删除这一行: test_cdf_df <- as.data.frame(test_cdf_data)

# 使用函数绘图 - 直接传递列表而不是数据框
plot_line_comparison_xcdf(
  data = test_cdf_data,  # 注意这里直接使用列表
  x_label = "cdf",
  y_label = "y(MiB/s)",
  export_name = "test_line_comparison_cdf.pdf"
)

# 使用自定义范围的版本
plot_line_comparison_xcdf(
  data = test_cdf_data,  # 注意这里直接使用列表
  x_label = "cdf",
  y_label = "y (MiB/s)",
#   x_lim = c(0.2, 0.9),
#   y_lim = c(100, 600),
  export_name = "test_line_comparison_cdf_with_limits.pdf"
)