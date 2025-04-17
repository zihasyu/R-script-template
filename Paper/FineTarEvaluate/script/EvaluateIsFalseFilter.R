library(here)
library(readxl)
source(here("MyR", "MyR.R"))

# 读取Excel数据
excel_path <- here("Paper/FineTarEvaluate/evaluate_FalseFilter_Is/compression_ratios.xlsx")
compression_data <- read_excel(excel_path)

# 获取列名（文本）
column_names <- colnames(compression_data)
num_columns <- length(column_names)

# 判断列数是否为偶数，如果是奇数可能需要特殊处理
if(num_columns %% 2 != 0) {
  warning("列数为奇数，最后一列可能没有配对")
}

# 计算需要生成的图表数量
num_plots <- floor(num_columns / 2)

# 确保输出文件夹存在
plot_dir <- here("Paper/FineTarEvaluate/evaluate_FalseFilter_Is/plots")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 为每两列生成一个图表
for(i in 1:num_plots) {
  # 计算当前处理的两列索引
  col1_idx <- (i-1)*2 + 1
  col2_idx <- col1_idx + 1
  
  if(col2_idx <= num_columns) {
    # 提取当前两列的数据
    col1_name <- column_names[col1_idx]
    col2_name <- column_names[col2_idx]
    
    # 从列名中提取简短的系统名称
    system_name1 <- gsub("SA_BiSearch_|_NonameHash", "", col1_name)
    system_name2 <- gsub("SA_BiSearch_|_NonameHash", "", col2_name)
    
    # 显示处理进度
    cat("正在处理图", i, ":", system_name1, "和", system_name2, "\n")
    
    # 提取两列数据
    col1_data <- compression_data[[col1_idx]]
    col2_data <- compression_data[[col2_idx]]
    
    # 确定有效数据长度（移除NA值后）
    col1_valid <- !is.na(col1_data)
    col2_valid <- !is.na(col2_data)
    
    # 找出两列都有效的行
    valid_rows <- col1_valid & col2_valid
    
    # 如果有有效行
    if(sum(valid_rows) > 0) {
      # 仅使用有效行
      valid_data1 <- col1_data[valid_rows]
      valid_data2 <- col2_data[valid_rows]
      
      # 创建数据框
      # 尝试方法1：使用Method1和Method2作为列名，与示例代码一致
      plot_data <- data.frame(
        Time = 1:length(valid_data1),
        Method1 = valid_data1,
        Method2 = valid_data2
      )
      plot_nopoint_data <- data.frame(
        # Time = 1:length(valid_data1),
        Method1 = valid_data1,
        Method2 = valid_data2
      )
      
      # 生成图表
      plot_name <- paste0(system_name1, "_vs_", system_name2, ".pdf")
      

      # plot_line_comparison(
      #   data =plot_nopoint_data ,
      #   # column_names = c(system_name1, system_name2),  # 只使用系统名称作为图例标签
      #   x_label = "Version",
      #   y_label = "OCR",
      #   export_path = plot_dir,
      #   export_name = paste0("/", plot_name)
      # )

      plot_with_sampled_points(
        data =plot_data,
      # column_names = c(system_name1, system_name2),  # 只使用系统名称作为图例标签
        x_label = "Version",
        y_label = "OCR",
        export_path = plot_dir,
        export_name = paste0("/", plot_name)
      )

      # plot_line_with_selected_points(
      #   data = plot_data,
      #   # column_names = c(system_name1, system_name2),  # 只使用系统名称作为图例标签
      #   num_points = 10,
      #   x_label = "Version",
      #   y_label = "Compression Ratio",
      #   export_path = plot_dir,
      #   export_name = paste0("/", plot_name)
      # )
      
      cat("已生成图表:", plot_name, "\n")
      
      # 如果上述方法不行，尝试打印函数的参数信息
      # cat("参数信息:", deparse(args(plot_line_with_selected_points)), "\n")
    } else {
      cat("警告: 列", col1_name, "和", col2_name, "没有共同的有效数据\n")
    }
  }
}