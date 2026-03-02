# 加载必要的库
library(dplyr)
library(readr)
# 如果尚未安装 openxlsx，请先运行 install.packages("openxlsx")
library(openxlsx)

# 设置工作目录到包含CSV文件的文件夹
# 请确保路径正确
setwd("d:/language/RStudio/project/R-script-template/Paper/TOS-26-3/performance（开源mtar版）/GitResults/")

# 获取当前目录下所有的CSV文件名
all_files <- list.files(pattern = "\\.csv$")

# 提取数据集的前缀名称（例如 "automake", "coreutils"）
detailed_files <- all_files[grepl("_detailed_log\\.csv$", all_files)]
prefixes <- sub("_detailed_log\\.csv$", "", detailed_files)

# 创建两个空的列表，用于存储待合并的列
perf_cols_list <- list()
restore_cols_list <- list()

# 循环处理每个数据集前缀
for (prefix in prefixes) {
  # 构建详细日志和恢复日志的文件名
  detailed_file <- paste0(prefix, "_detailed_log.csv")
  restore_file <- paste0(prefix, "_restore_log.csv")

  # 检查文件是否存在
  if (file.exists(detailed_file) && file.exists(restore_file)) {
    # --- 处理 perf 数据 ---
    detailed_log <- read_csv(detailed_file, show_col_types = FALSE)
    perf_col <- detailed_log %>% select(`Throughput_MB/s`)
    colnames(perf_col) <- prefix
    perf_cols_list[[prefix]] <- perf_col

    # --- 处理 restore 数据 ---
    restore_log <- read_csv(restore_file, show_col_types = FALSE)
    restore_col <- restore_log %>% select(`RestoreThroughput_MB/s`)
    colnames(restore_col) <- prefix
    restore_cols_list[[prefix]] <- restore_col
    
    cat(paste("Processed:", prefix, "\n"))
    
  } else {
    cat(paste("Warning: Missing files for prefix", prefix, "\n"))
  }
}

# --- 统一并合并 perf 数据 ---
# 找到所有perf列中的最大行数
max_rows_perf <- max(sapply(perf_cols_list, nrow))
# 调整每一列的行数以匹配最大行数，不足的用NA填充
perf_cols_aligned <- lapply(perf_cols_list, function(df) {
  na_rows <- max_rows_perf - nrow(df)
  if (na_rows > 0) {
    na_df <- as.data.frame(matrix(NA, nrow = na_rows, ncol = 1))
    colnames(na_df) <- colnames(df)
    df <- rbind(df, na_df)
  }
  return(df)
})
# 将列表中的所有列合并成一个数据框
perf_data <- do.call(cbind, perf_cols_aligned)


# --- 统一并合并 restore 数据 ---
# 找到所有restore列中的最大行数
max_rows_restore <- max(sapply(restore_cols_list, nrow))
# 调整每一列的行数以匹配最大行数，不足的用NA填充
restore_cols_aligned <- lapply(restore_cols_list, function(df) {
  na_rows <- max_rows_restore - nrow(df)
  if (na_rows > 0) {
    na_df <- as.data.frame(matrix(NA, nrow = na_rows, ncol = 1))
    colnames(na_df) <- colnames(df)
    df <- rbind(df, na_df)
  }
  return(df)
})
# 将列表中的所有列合并成一个数据框
restore_data <- do.call(cbind, restore_cols_aligned)


# --- 将数据写入 XLSX 文件 ---
# 创建一个新的工作簿
wb <- createWorkbook()

# 添加一个名为 "perf" 的工作表并写入数据
addWorksheet(wb, "perf")
writeData(wb, "perf", perf_data)

# 添加一个名为 "restore" 的工作表并写入数据
addWorksheet(wb, "restore")
writeData(wb, "restore", restore_data)

# 保存工作簿
# R中的NA值在Excel中将显示为空白单元格
saveWorkbook(wb, "performance_data.xlsx", overwrite = TRUE)

cat("\nData has been saved to performance_data.xlsx with 'perf' and 'restore' sheets.\n")