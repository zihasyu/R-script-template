library(here)
source(here("MyR", "MyR.R"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data1 <- c(0.211018, 0.473857, 0.031107, 0.178961)
data2 <- c(0.21847, 0.0581643, 0.0334536, 0.130792)
labels <- c("Linux", "Web", "Chro.", "Auto.")

# 基本调用
# plot <- create_comparison_barplot(data1, data2, labels)

# 显示图例的调用
plot <- create_comparison_barplot(
  data1 = data1,
  data2 = data2,
  labels = labels,
  fill_names = c("Off. C.", "Mix. Mod."),
  y_label = "FRP",
  show_legend = TRUE
)

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
exportPath="./"
exportName="bar.pdf"
ggsave(paste(exportPath, exportName, sep=""), plot = plot, width = 12, height = 6)