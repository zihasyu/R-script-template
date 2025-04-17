# 加载所需的库
library(readxl)
library(writexl)

# 设置工作目录为当前文件所在目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 读取日志文件
log_content <- readLines("chunkInfoLog.txt")

# 提取包含"Overall Compression Ratio:"的行
ratio_lines <- grep("Overall Compression Ratio:", log_content, value = TRUE)

# 提取压缩比数据
compression_ratios <- gsub(".*Overall Compression Ratio: ([0-9.]+).*", "\\1", ratio_lines)
compression_ratios <- as.numeric(compression_ratios)

# 提取路径中的数据集名称
instruction_lines <- grep("./BiSearch -i /mnt/dataset2/", log_content, value = TRUE)
dataset_names <- gsub("./BiSearch -i /mnt/dataset2/([^ ]+).*", "\\1", instruction_lines)

# 确定每组数据的大小（即连续20个小节为一组）
group_size <- 21

# 确定数据集数量
datasets_count <- length(compression_ratios) %/% group_size

# 创建一个数据框来存储结果
result_df <- data.frame(matrix(NA, nrow = group_size, ncol = datasets_count))

# 为列命名
colnames(result_df) <- dataset_names[seq(1, length(dataset_names), by = group_size)][1:datasets_count]

# 填充数据框
for (i in 1:datasets_count) {
  start_idx = (i-1)*group_size + 1
  end_idx = i*group_size
  result_df[, i] <- compression_ratios[start_idx:end_idx]
}

# 添加行名（可选，显示a参数值）
rownames(result_df) <- paste("a =", 0:(group_size-1))

# 写入Excel文件
write_xlsx(result_df, "compression_ratios.xlsx")

print("数据已成功提取并保存到compression_ratios.xlsx")