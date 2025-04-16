#Test for Line chart of the natural isometric x-axis
source("./Line_NoPoint.R")
# 创建测试数据
set.seed(123) # 设置随机种子以确保可重复性

# 创建一个包含四列数据的数据框，模拟四个不同系统的上传速度
data_df <- data.frame(
  System1 = c(120, 350, 280, 410, 380, 450, 520, 480, 550, 600),
  System2 = c(200, 280, 350, 390, 420, 380, 410, 450, 470, 510),
  System3 = c(150, 220, 270, 320, 370, 410, 430, 460, 500, 530),
  System4 = c(300, 320, 340, 370, 410, 450, 470, 510, 540, 580)
)

# 添加一些随机波动使数据看起来更真实
for(i in 1:ncol(data_df)) {
  data_df[,i] <- data_df[,i] + rnorm(nrow(data_df), 0, 20)
}

# 确保所有值都是正的
data_df[data_df < 0] <- 0

# 查看数据
head(data_df)

# 使用函数绘图
plot_line_comparison(
  data = data_df,
  x_label = "Snapshot",
  y_label = "Upload (MiB/s)",
  export_name = "test_line_comparison.pdf"
)

# 使用自定义范围的版本
plot_line_comparison(
  data = data_df,
  x_label = "Snapshot",
  y_label = "Upload (MiB/s)",
  x_lim = c(2, 9),
  y_lim = c(100, 600),
  export_name = "test_line_comparison_with_limits.pdf"
)

# 带有自定义刻度的版本
plot_line_comparison(
  data = data_df,
  x_label = "Snapshot",
  y_label = "Upload (MiB/s)",
  x_breaks = c(1, 3, 5, 7, 9),
  y_breaks = seq(100, 600, by = 100),
  export_name = "test_line_comparison_with_breaks.pdf"
)