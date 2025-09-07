library(here)
library(readxl)
library(writexl)
library(stringr)
library(dplyr)

# 设置工作目录
base_dir <- here("Paper/开源mtar_longnameFineTAR/ShiftChunk")

# 获取目录中所有txt文件
txt_files <- list.files(path = base_dir, pattern = "^Dedup_Skip_.+\\.txt$", full.names = TRUE)

# 创建结果存储列表
results <- list()
max_rows <- 0

# 处理每个txt文件
for (file_path in txt_files) {
  # 提取文件名并生成列名
  file_name <- basename(file_path)
  # 从文件名中提取系统名称（去掉"Dedup_Skip_"和".txt"）
  system_name <- gsub("Dedup_Skip_|.txt", "", file_name)
  cat("处理文件:", system_name, "\n")
  
  # 读取文件内容
  file_content <- readLines(file_path)
  
  # 初始化存储结果的向量
  offset_chunks <- numeric()
  
  # 查找所有"OffsetChunk percentage"行
  for (i in 1:length(file_content)) {
    line <- file_content[i]
    if (grepl("OffsetChunk percentage", line)) {
      # 提取百分比值
      value_str <- gsub(".*OffsetChunk percentage\\s+([0-9.]+).*", "\\1", line)
      # 尝试转换为数值
      value <- as.numeric(value_str)
      if (!is.na(value)) {
        offset_chunks <- c(offset_chunks, value)
      }
    }
  }
  
  # 更新最大行数
  max_rows <- max(max_rows, length(offset_chunks))
  
  # 将结果添加到列表中
  results[[system_name]] <- offset_chunks
  
  cat("已提取", length(offset_chunks), "个值\n")
}

# 创建数据框，并确保所有列具有相同的长度
result_df <- data.frame(matrix(NA, nrow = max_rows, ncol = 0))

for (system_name in names(results)) {
  values <- results[[system_name]]
  # 如果长度不足，则用NA填充
  if (length(values) < max_rows) {
    values <- c(values, rep(NA, max_rows - length(values)))
  }
  result_df[[system_name]] <- values
}

# 确保列的顺序与文件列表顺序一致
ordered_names <- sapply(txt_files, function(path) {
  gsub("Dedup_Skip_|.txt", "", basename(path))
})
result_df <- result_df[, ordered_names]

# 创建输出目录（如果不存在）
output_dir <- file.path(base_dir, "Overall")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 导出为Excel文件
output_file <- file.path(output_dir, "OffsetChunk_Percentage.xlsx")
write_xlsx(result_df, output_file)

cat("提取完成，结果已保存到:", output_file, "\n")
cat("总共处理了", length(txt_files), "个文件\n")
cat("Excel文件包含", ncol(result_df), "列,", nrow(result_df), "行数据\n")

