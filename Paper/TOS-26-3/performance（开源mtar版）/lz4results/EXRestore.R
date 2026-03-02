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
txt_files <- list.files(path = directory, pattern = "\\.log$", full.names = TRUE)

# 先确定每个文件有多少个匹配项，以便创建适当大小的数据框
max_matches <- 0
for (file in txt_files) {
  content <- readLines(file, warn = FALSE)
  content <- paste(content, collapse = "\n")

  matches <- str_extract_all(content, " 解压吞吐量: \\s*\\d+\\.\\d+")
  num_matches <- length(unlist(matches))
  
  if(num_matches > max_matches) {
    max_matches <- num_matches
  }
}

# 创建空数据框 - 列是文件，行是每个文件中的多个值
results <- data.frame(matrix(ncol = length(txt_files), nrow = max_matches))

# 设置列名为文件名（不含扩展名）
colnames(results) <- gsub("^compression_", "TZ_", gsub("\\.log$", "", basename(txt_files)))

# 处理每个文件
for (i in 1:length(txt_files)) {
  # 读取文件内容，确保处理各种换行符
  content <- readLines(txt_files[i], warn = FALSE)
  content <- paste(content, collapse = " ")  # 使用空格而不是\n连接
  
  # 使用更宽松的正则表达式匹配
  matches <- regmatches(
    content,
    gregexpr("解压吞吐量: \\s*([0-9]+\\.?[0-9]*)", content)
  )
  matches <- unlist(matches)
  
  # 从匹配文本中提取数值，更加健壮的方式
  ratios <- as.numeric(gsub(".*解压吞吐量: \\s*([0-9]+\\.?[0-9]*).*", "\\1", matches))
  
  # 打印更多调试信息
  cat("文件", basename(txt_files[i]), "找到", length(ratios), "个匹配项\n")
  
  # 保存结果到相应列
  for (j in 1:length(ratios)) {
    if (j <= max_matches) {
      results[j, i] <- ratios[j]
    }
  }
}
# 保存结果到Excel文件
write_xlsx(results, path = file.path(directory, "Restore.xlsx"))

cat("save to ", file.path(directory, "Restore.xlsx"), "\n")
cat("Find ", max_matches, " matched file \n")

