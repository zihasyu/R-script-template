library(here)
source(here("MyR", "MyR.R"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 假设有3组数据
set.seed(1)
data_list <- list(
  rnorm(10, 5, 1.2),
  rnorm(12, 7, 1.5),
  rnorm(8, 4, 0.8)
)
labels <- c("A", "B", "C")
fill_colors <- c("#AD0626", "#B79AD1", "#3CB371")
fill_names <- c("Group A", "Group B", "Group C")

p <- create_barplot_with_ci(
  data_list = data_list,
  labels = labels,
  fill_colors = fill_colors,
  fill_names = fill_names,
  export_name = "./test.pdf"
)
print(p)
# # 完全自定义的调用
# plot <- create_comparison_barplot(
#   data1 = c(2.222, 0.6, 0.4),
#   data2 = c(0.3, 0.5, 0.7),
#   labels = c("A", "B", "C"),
#   # fill_colors = c("blue", "red"),
#   fill_names = c("Original", "Modified"),
#   x_label = "Categories",
#   y_label = "Metrics",
#   export_name = "new_plot.pdf"
# )

