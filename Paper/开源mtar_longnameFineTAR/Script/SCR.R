library(here)
library(readxl)
library(ggplot2)
source(here("MyR", "MyR.R"))

# 读取Excel数据
data_path <- here("Paper/开源mtar_longnameFineTAR/ShiftChunk/Overall/OffsetChunk_MaxVersion.xlsx")
offset_data <- read_excel(data_path)

# 确保输出文件夹存在
output_dir <- here("Paper/开源mtar_longnameFineTAR/ShiftChunk/plots")
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 检查数据结构
cat("数据列名:", colnames(offset_data), "\n")
cat("数据行数:", nrow(offset_data), "\n")

# 不移除NA行，保持原始数据结构
# offset_data <- na.omit(offset_data)  # 注释掉这行

# 使用plot_line_comparison_xcdf()生成折线图
plot <- plot_line_comparison_xcdf(
  data = offset_data,
  export_path = output_dir,
  export_name = "/offsetchunk_comparison.pdf",
  x_label = "CDF of Versions",
  y_label = "FDR",
  line_size = 2.8,
  axis_text_size = 30,
  x_title_size = 30,
  y_title_size = 30,
  plot_width = 5,
  plot_height = 3.4,
  range_extension = 1,
  x_breaks = c(0,0.25,0.5,0.75,1),
  y_breaks = c(0.05,0.1,0.15,0.2,0.25),
  x_expand = c(0,0),
  y_expand = c(0,0.12)
)

cat("图表已生成并保存到:", file.path(output_dir, "offsetchunk_comparison.pdf"), "\n")

