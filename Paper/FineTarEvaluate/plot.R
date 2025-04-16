library(here)
source(here("MyR", "MyR.R"))

set.seed(123)
x_values <- 1:100
data_large <- data.frame(
  Time = x_values,
  Method1 = cumsum(rnorm(100, mean = 5, sd = 10)),
  Method2 = cumsum(rnorm(100, mean = 3, sd = 8))
  # Method3 = cumsum(rnorm(100, mean = 7, sd = 12))
)
plot_line_with_selected_points(
  data_large,
  column_names = c("Algorithm A", "Algorithm B"),
  num_points = 10,  # 每条线上显示10个点
  x_label = "Time Step",
  y_label = "Cumulative Value",
  export_path =here("Paper/FineTarEvaluate"),
  export_name = "/plot/example1.pdf"
)