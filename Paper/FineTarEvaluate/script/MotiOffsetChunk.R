library(here)
library(readxl)
library(ggplot2)
source(here("MyR", "MyR.R"))

# 读取Excel数据
data_path <- here("Paper/FineTarEvaluate/ShiftChunk/Overall/OffsetChunk_MaxVersion.xlsx")
offset_data <- read_excel(data_path)

# 确保输出文件夹存在
output_dir <- here("Paper/FineTarEvaluate/ShiftChunk/Overall/plots")
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 检查数据结构
cat("数据列名:", colnames(offset_data), "\n")
cat("数据行数:", nrow(offset_data), "\n")

# 移除所有NA行
offset_data <- na.omit(offset_data)

# 使用plot_line_comparison_xcdf()生成折线图
plot <- plot_line_comparison_xcdf(
  data = offset_data,
  export_path = output_dir,
  export_name = "/offsetchunk_comparison.pdf",
  x_label = "CDF For Version Numbers",
  y_label = "SCR",
  # colors = c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF"),
  line_size = 2.8,
  axis_text_size = 20,
  x_title_size = 22,
  y_title_size = 22,
  plot_width = 10,
  plot_height = 3,
  range_extension = 1.1,  # 新参数：控制轴范围的扩展系数
)

# 可选：如果希望返回或打印一些信息
cat("图表已生成并保存到:", file.path(output_dir, "offsetchunk_comparison.pdf"), "\n")