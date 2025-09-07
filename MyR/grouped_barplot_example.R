library(here)
source(here("MyR", "MyR.R"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 示例：创建8个分组，每组4个柱子的数据
# 假设您有8个实验场景，每个场景测试4种方法

# 模拟数据生成（您需要替换为真实数据）
set.seed(123)
data_matrix <- list()

# 8个分组的标签
group_labels <- c("Dataset1", "Dataset2", "Dataset3", "Dataset4", 
                  "Dataset5", "Dataset6", "Dataset7", "Dataset8")

# 4个柱子的标签（代表4种方法）
bar_labels <- c("Method A", "Method B", "Method C", "Method D")

# 为每个分组创建4个柱子的数据
for(i in 1:8) {
  group_data <- list()
  for(j in 1:4) {
    # 每个柱子包含多次实验结果（用于计算置信区间）
    # 您需要将这里的模拟数据替换为真实数据
    group_data[[j]] <- rnorm(10, mean = runif(1, 0.5, 2.0), sd = 0.1)
  }
  data_matrix[[i]] <- group_data
}

# 使用新函数创建分组柱状图
p <- create_grouped_barplot_with_ci(
  data_matrix = data_matrix,
  group_labels = group_labels,
  bar_labels = bar_labels,
  fill_colors = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12"),
  x_label = "Datasets",
  y_label = "Performance Metric",
  export_name = "grouped_barplot_with_ci.pdf",
  export_path = "./",
  width = 16,
  height = 8,
  show_legend = TRUE,
  legend_position = "top",
  show_data_labels = FALSE,
  axis_text_size = 14,
  legend_text_size = 12
)

# 显示图形
print(p)

# 如果您的数据结构不同，可以参考以下数据准备方法：

# 方法1：如果您有CSV文件
# data <- read.csv("your_data.csv")
# 然后按照分组和柱子重新组织数据

# 方法2：如果您有现有的向量数据
# 假设您有如下形式的数据：
# group1_method1 <- c(1.2, 1.3, 1.1, 1.4, 1.2)
# group1_method2 <- c(1.5, 1.6, 1.4, 1.7, 1.5)
# ...

# 您可以这样组织：
# data_matrix_example <- list(
#   list(group1_method1, group1_method2, group1_method3, group1_method4),
#   list(group2_method1, group2_method2, group2_method3, group2_method4),
#   # ... 继续到第8组
# )

