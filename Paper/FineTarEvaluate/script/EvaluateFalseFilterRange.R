library(here)
library(readxl)
library(dplyr)

# 加载自定义绘图函数
source(here("MyR", "MyR.R"))

# 读取Excel数据
excel_path <- here("Paper/FineTarEvaluate/evaluate_FalseFilter_Range/Overall/compression_ratios.xlsx")
compression_data <- read_excel(excel_path)

# 读取参考线数据
refline_path <- here("Paper/FineTarEvaluate/evaluate_FalseFilter_Range/Overall/ReferenceLine.xlsx")
refline_data <- read_excel(refline_path)

# 获取列名
column_names <- colnames(compression_data)
refline_column_names <- colnames(refline_data)

# 确保输出文件夹存在
plot_dir <- here("Paper/FineTarEvaluate/evaluate_FalseFilter_Range/Overall/plots")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 每一列单独绘制一张图
for(i in 1:length(column_names)) {
  # 获取列名和数据
  col_name <- column_names[i]
  col_data <- compression_data[[i]]
  
  # 从列名中提取简短的系统名称
  system_name <- gsub("SA_BiSearch_|_NonameHash", "", col_name)
  
  # 显示处理进度
  cat("正在处理图", i, ":", system_name, "\n")
  
  # 移除NA值
  valid_data <- col_data[!is.na(col_data)]
  
  # 如果有有效数据
  if(length(valid_data) > 0) {
    # 创建数据框
    # plot_data <- as.data.frame(valid_data)
    
    plot_data <- data.frame(
        Time = 1:length(valid_data),
        Method1 = valid_data
        # ,Method2 = valid_data0
    )

    plot_nopoint_data <- data.frame(
        # Time = 1:length(valid_data1),
        Method1 = valid_data
        # Method2 = valid_data2
    )
    # names(plot_data) <- system_name
    # names(plot_nopoint_data) <- system_name
    # 查找对应的参考线值
    ref_value <- NA
    if(col_name %in% refline_column_names) {
      ref_value <- refline_data[[col_name]][1]  # 假设每列只有一个值
    } else {
      # 尝试匹配系统名称
      matching_refline <- grep(system_name, refline_column_names, value = TRUE)
      if(length(matching_refline) > 0) {
        ref_value <- refline_data[[matching_refline[1]]][1]
      } else {
        warning("找不到", system_name, "的参考线数据")
      }
    }
    
    # 生成图表
    plot_name <- paste0(system_name, "_with_refline.pdf")
    
    # 使用plot_line_comparison_ref函数绘图
    # plot_line_with_points_ref
    plot_with_sampled_points_ref(
      data = plot_data,
      x_label = "Threshold",
      y_label = "OCR",
      export_path = plot_dir,
      export_name = paste0("/", plot_name),
      ref_line = !is.na(ref_value),  # 如果找到参考线值则显示
      ref_line_y = if(is.na(ref_value)) 0 else ref_value,
    #   ref_line_size = 3,
      # ref_line_color = "red"  # 使用红色参考线
    )
    
    cat("已生成图表:", plot_name, "\n")
  } else {
    cat("警告: 列", col_name, "没有有效数据\n")
  }
}

# 生成包含所有系列的总图
# 移除所有NA值，并保持每列相同长度
all_valid_data <- compression_data %>% 
  na.omit()

if(nrow(all_valid_data) > 0) {
  # 使用plot_line_comparison_ref函数绘制总图
  plot_line_comparison_ref(
    data = all_valid_data,
    x_label = "Version",
    y_label = "OCR",
    export_path = plot_dir,
    export_name = "/all_systems_comparison.pdf",
    ref_line = FALSE,  # 总图不使用参考线以避免混淆
    plot_width = 12,   # 加宽以容纳更多数据
    plot_height = 7    # 加高以便更好地显示
  )
  
  cat("已生成总体比较图表\n")
} else {
  cat("警告: 没有所有列都具有的有效数据\n")
}