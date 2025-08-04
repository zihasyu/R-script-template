# 安装必要的包
if(!require(readr)) install.packages("readr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(writexl)) install.packages("writexl")
if(!require(stringr)) install.packages("stringr")

library(readr)
library(dplyr)
library(writexl)
library(stringr)

# 设置工作目录为当前文件所在目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 目标目录
directory <- "."  # 当前目录

# 获取目录中所有txt文件
txt_files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)

# 创建空数据框 - 每个文件只有一个值（最后一行）
results <- data.frame(matrix(ncol = length(txt_files), nrow = 1))

# 设置列名为文件名（不含扩展名）
colnames(results) <- gsub("\\.txt$", "", basename(txt_files))

# 处理每个文件
for (i in 1:length(txt_files)) {
  # 读取文件内容
  content <- readLines(txt_files[i], warn = FALSE)
  
  # 从最后一行开始向前查找包含 "Restore throughput:" 的行
  restore_value <- NA
  for (j in length(content):1) {
    if (grepl("Restore throughput:", content[j])) {
      # 提取数值
      match <- regmatches(
        content[j],
        regexpr("Restore throughput:\\s*([0-9]+\\.?[0-9]*)", content[j])
      )
      if (length(match) > 0) {
        restore_value <- as.numeric(gsub(".*Restore throughput:\\s*([0-9]+\\.?[0-9]*).*", "\\1", match))
        break
      }
    }
  }
  
  # 打印调试信息
  cat("文件", basename(txt_files[i]), "最后一行Restore throughput值:", restore_value, "\n")
  
  # 保存结果到相应列
  results[1, i] <- restore_value
}

# 保存结果到Excel文件
write_xlsx(results, path = file.path(directory, "TotalRestore.xlsx"))

cat("save to ", file.path(directory, "TotalRestore.xlsx"), "\n")
cat("处理了 ", length(txt_files), " 个文件\n")

