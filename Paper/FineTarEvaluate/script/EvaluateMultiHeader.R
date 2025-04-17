library(here)
library(readxl)
source(here("MyR", "MyR.R"))

# 读取Excel数据
excel_path <- here("Paper/FineTarEvaluate/evaluate_multiHeader/compression_ratios.xlsx")
compression_data <- read_excel(excel_path)

# 获取列名（文本）
column_names <- colnames(compression_data)
num_columns <- length(column_names)

# 每组图表使用5列数据
GROUP_SIZE <- 5

# 计算需要生成的图表数量
num_plots <- floor(num_columns / GROUP_SIZE)

# 确保输出文件夹存在
plot_dir <- here("Paper/FineTarEvaluate/evaluate_multiHeader/plots")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 为每五列生成一个图表
for(i in 1:num_plots) {
  # 计算当前处理的五列索引
  col_start_idx <- (i-1)*GROUP_SIZE + 1
  col_end_idx <- min(col_start_idx + GROUP_SIZE - 1, num_columns)
  
  # 提取当前组的列名和系统名称
  current_col_names <- column_names[col_start_idx:col_end_idx]
  system_names <- gsub("SA_BiSearch_|_NonameHash", "", current_col_names)
  
  # 显示处理进度
  cat("正在处理图", i, ":", paste(system_names, collapse=", "), "\n")
  
  # 创建用于存储有效数据的列表
  all_valid_data <- list()
  max_valid_length <- 0
  
  # 提取每列数据并检查有效性
  for(j in 1:(col_end_idx - col_start_idx + 1)) {
    col_idx <- col_start_idx + j - 1
    col_data <- compression_data[[col_idx]]
    valid_data <- col_data[!is.na(col_data)]
    
    if(length(valid_data) > 0) {
      all_valid_data[[j]] <- valid_data
      max_valid_length <- max(max_valid_length, length(valid_data))
    }
  }
  
  # 如果有有效数据
  if(length(all_valid_data) > 0 && max_valid_length > 0) {
    # 创建一个空的数据框
    plot_data <- data.frame(Time = 1:max_valid_length)
    
    # 为数据框添加每列有效数据
    for(j in 1:length(all_valid_data)) {
      # 获取当前列的有效数据
      valid_data <- all_valid_data[[j]]
      
      # 如果数据长度小于最大长度，使用NA填充
      if(length(valid_data) < max_valid_length) {
        valid_data <- c(valid_data, rep(NA, max_valid_length - length(valid_data)))
      }
      
      # 将数据添加到数据框
      plot_data[[paste0("Method", j)]] <- valid_data
    }
    
    # 生成图表文件名
    group_name <- paste(system_names, collapse="_")
    if(nchar(group_name) > 100) {  # 避免文件名过长
      group_name <- paste0("Group_", i)
    }
    plot_name <- paste0(group_name, ".pdf")
    
    # 准备不带时间列的数据框，用于plot_line_comparison
    plot_nopoint_data <- plot_data[, -1, drop = FALSE]  # 移除第一列（Time）
    
    # 使用plot_line_comparison函数绘图
    plot_line_comparison(
      data = plot_nopoint_data,
      x_label = "Version",
      y_label = "OCR",
      export_path = plot_dir,
      export_name = paste0("/", plot_name)
    )
    
    cat("已生成图表:", plot_name, "\n")
  } else {
    cat("警告: 组", i, "没有有效数据\n")
  }
}

# 处理剩余的列（如果有）
remaining_cols <- num_columns %% GROUP_SIZE
if(remaining_cols > 0) {
  col_start_idx <- num_plots * GROUP_SIZE + 1
  col_end_idx <- num_columns
  
  cat("处理剩余的", remaining_cols, "列\n")
  
  # 提取当前组的列名和系统名称
  current_col_names <- column_names[col_start_idx:col_end_idx]
  system_names <- gsub("SA_BiSearch_|_NonameHash", "", current_col_names)
  
  # 创建用于存储有效数据的列表
  all_valid_data <- list()
  max_valid_length <- 0
  
  # 提取每列数据并检查有效性
  for(j in 1:(col_end_idx - col_start_idx + 1)) {
    col_idx <- col_start_idx + j - 1
    col_data <- compression_data[[col_idx]]
    valid_data <- col_data[!is.na(col_data)]
    
    if(length(valid_data) > 0) {
      all_valid_data[[j]] <- valid_data
      max_valid_length <- max(max_valid_length, length(valid_data))
    }
  }
  
  # 如果有有效数据
  if(length(all_valid_data) > 0 && max_valid_length > 0) {
    # 创建一个空的数据框
    plot_data <- data.frame(Time = 1:max_valid_length)
    
    # 为数据框添加每列有效数据
    for(j in 1:length(all_valid_data)) {
      # 获取当前列的有效数据
      valid_data <- all_valid_data[[j]]
      
      # 如果数据长度小于最大长度，使用NA填充
      if(length(valid_data) < max_valid_length) {
        valid_data <- c(valid_data, rep(NA, max_valid_length - length(valid_data)))
      }
      
      # 将数据添加到数据框
      plot_data[[paste0("Method", j)]] <- valid_data
    }
    
    # 生成图表文件名
    group_name <- paste(system_names, collapse="_")
    if(nchar(group_name) > 100) {  # 避免文件名过长
      group_name <- paste0("Group_Remaining")
    }
    plot_name <- paste0(group_name, ".pdf")
    
    # 准备不带时间列的数据框
    plot_nopoint_data <- plot_data[, -1, drop = FALSE]  # 移除第一列（Time）
    
    # 使用plot_line_comparison函数绘图
    plot_line_comparison(
      data = plot_nopoint_data,
      x_label = "Version",
      y_label = "OCR",
      export_path = plot_dir,
      export_name = paste0("/", plot_name)
    )
    
    cat("已生成图表:", plot_name, "\n")
  }
}