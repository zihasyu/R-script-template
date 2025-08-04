#!/bin/bash

# 批量ZIP压缩脚本，记录压缩率和吞吐量
# 输出目录为当前路径
OUTPUT_DIR="./compressed_datasets"
MAIN_LOG_FILE="./compression_summary.txt"

# 创建输出目录
mkdir -p "$OUTPUT_DIR"

# 初始化主日志文件
echo "=== ZIP压缩/解压报告汇总 ===" > "$MAIN_LOG_FILE"
echo "开始时间: $(date)" >> "$MAIN_LOG_FILE"
echo "" >> "$MAIN_LOG_FILE"

# 数据集配置（基于你的test.sh）
declare -a datasets=(
    "/mnt/dataset2/react:react:100"
    "/mnt/dataset2/netty:netty:99" 
    "/mnt/dataset2/Cpython:Cpython:100"
    "/mnt/dataset2/automake_tarballs:automake:100"
    "/mnt/dataset2/coreutils_tarballs:coreutils:28"
    "/mnt/dataset2/GNU_GCC/gcc-packed/tar:gcc:117"
    "/mnt/dataset2/linux:linux:270"
    "/mnt/dataset2/WEB:WEB:102"
)

process_dataset() {
    local source_path=$1
    local dataset_name=$2
    local expected_count=$3
    
    # 创建数据集专用日志文件
    local dataset_log_file="./compression_${dataset_name}.log"
    local decompression_log_file="./decompression_${dataset_name}.log"
    
    echo "===================="
    echo "处理数据集: $dataset_name ($source_path)"
    echo "===================="
    
    # 初始化数据集日志文件
    echo "=== $dataset_name 压缩报告 ===" > "$dataset_log_file"
    echo "开始时间: $(date)" >> "$dataset_log_file"
    echo "源路径: $source_path" >> "$dataset_log_file"
    echo "预期文件数量: $expected_count" >> "$dataset_log_file"
    echo "" >> "$dataset_log_file"
    
    # 检查源路径是否存在
    if [ ! -d "$source_path" ]; then
        echo "警告: 路径 $source_path 不存在，跳过" | tee -a "$dataset_log_file"
        return
    fi
    
    # 数据集级别的统计变量
    dataset_total_original_size=0
    dataset_total_compressed_size=0
    dataset_total_compression_time=0
    dataset_total_decompression_time=0
    dataset_file_count=0
    
    # 创建数据集专用目录
    dataset_output_dir="$OUTPUT_DIR/$dataset_name"
    dataset_decompress_dir="$OUTPUT_DIR/${dataset_name}_decompressed"
    mkdir -p "$dataset_output_dir"
    mkdir -p "$dataset_decompress_dir"
    
    echo "开始处理 $dataset_name 下的备份文件..."
    
    # 获取文件列表并按自然语言排序
    mapfile -t file_list < <(find "$source_path" -type f -printf '%f\t%p\n' | sort -V | cut -f2)
    
    echo "找到 ${#file_list[@]} 个文件，按自然排序处理..."
    echo "文件处理顺序:" >> "$dataset_log_file"
    for i in "${!file_list[@]}"; do
        echo "  $((i+1)). $(basename "${file_list[i]}")" >> "$dataset_log_file"
    done
    echo "" >> "$dataset_log_file"
    
    # 按排序后的顺序处理每个文件
    for file in "${file_list[@]}"; do
        if [ -f "$file" ]; then
            # 获取文件名（不含路径）
            filename=$(basename "$file")
            echo "  处理文件 [$((dataset_file_count + 1))/${#file_list[@]}]: $filename"
            
            # 计算原始大小
            original_size=$(stat -c%s "$file")
            original_size_mib=$(echo "scale=2; $original_size / 1024 / 1024" | bc -l)
            
            # 压缩文件名
            zip_file="$dataset_output_dir/${filename}.zip"
            
            # ========== 压缩阶段 ==========
            echo "    正在压缩..."
            start_time=$(date +%s.%N)
            
            # 压缩单个文件
            zip -q "$zip_file" "$file"
            
            # 结束计时
            end_time=$(date +%s.%N)
            compression_duration=$(echo "$end_time - $start_time" | bc -l)
            
            # 计算压缩后大小
            compressed_size=$(stat -c%s "$zip_file" 2>/dev/null || echo 0)
            compressed_size_mib=$(echo "scale=2; $compressed_size / 1024 / 1024" | bc -l)
            
            # 计算压缩率 (原始大小/压缩大小)
            if [ $compressed_size -gt 0 ]; then
                compression_ratio=$(echo "scale=2; $original_size / $compressed_size" | bc -l)
                compression_ratio_formatted=$(printf "%.2f" $compression_ratio)
            else
                compression_ratio_formatted="0.00"
            fi
            
            # 计算压缩吞吐量 (MiB/s)
            if (( $(echo "$compression_duration > 0" | bc -l) )); then
                compression_throughput=$(echo "scale=2; $original_size_mib / $compression_duration" | bc -l)
                compression_throughput_formatted=$(printf "%.2f" $compression_throughput)
            else
                compression_throughput_formatted="0.00"
            fi
            
            # ========== 解压阶段 ==========
            echo "    正在解压测试..."
            decompressed_file_path="$dataset_decompress_dir/$filename"
            
            # 开始解压计时
            start_time=$(date +%s.%N)
            
            # 解压文件
            unzip -q -o "$zip_file" -d "$dataset_decompress_dir/"
            
            # 结束解压计时
            end_time=$(date +%s.%N)
            decompression_duration=$(echo "$end_time - $start_time" | bc -l)
            
            # 计算解压吞吐量 (MiB/s)
            if (( $(echo "$decompression_duration > 0" | bc -l) )); then
                decompression_throughput=$(echo "scale=2; $original_size_mib / $decompression_duration" | bc -l)
                decompression_throughput_formatted=$(printf "%.2f" $decompression_throughput)
            else
                decompression_throughput_formatted="0.00"
            fi
            
            # 清理解压的文件（节省空间）
            rm -f "$decompressed_file_path"
            
            # 更新数据集统计
            dataset_total_original_size=$((dataset_total_original_size + original_size))
            dataset_total_compressed_size=$((dataset_total_compressed_size + compressed_size))
            dataset_total_compression_time=$(echo "$dataset_total_compression_time + $compression_duration" | bc -l)
            dataset_total_decompression_time=$(echo "$dataset_total_decompression_time + $decompression_duration" | bc -l)
            dataset_file_count=$((dataset_file_count + 1))
            
            # 记录到压缩日志
            echo "文件 #$dataset_file_count: $filename" >> "$dataset_log_file"
            echo "  原始大小: $(printf "%.2f" $original_size_mib) MiB" >> "$dataset_log_file"
            echo "  压缩大小: $(printf "%.2f" $compressed_size_mib) MiB" >> "$dataset_log_file"
            echo "  压缩率: ${compression_ratio_formatted}x" >> "$dataset_log_file"
            echo "  压缩时间: $(printf "%.2f" $compression_duration) 秒" >> "$dataset_log_file"
            echo "  压缩吞吐量: ${compression_throughput_formatted} MiB/s" >> "$dataset_log_file"
            echo "  解压时间: $(printf "%.2f" $decompression_duration) 秒" >> "$dataset_log_file"
            echo "  解压吞吐量: ${decompression_throughput_formatted} MiB/s" >> "$dataset_log_file"
            echo "  压缩文件: $zip_file" >> "$dataset_log_file"
            echo "" >> "$dataset_log_file"
            
            # 控制台输出
            echo "    ✓ $filename 完成 | 大小: $(printf "%.2f" $original_size_mib) → $(printf "%.2f" $compressed_size_mib) MiB"
            echo "      压缩: ${compression_ratio_formatted}x, ${compression_throughput_formatted} MiB/s | 解压: ${decompression_throughput_formatted} MiB/s"
            
        fi
    done
    
    # 计算数据集总体统计
    if [ $dataset_file_count -gt 0 ]; then
        dataset_total_original_mib=$(echo "scale=2; $dataset_total_original_size / 1024 / 1024" | bc -l)
        dataset_total_compressed_mib=$(echo "scale=2; $dataset_total_compressed_size / 1024 / 1024" | bc -l)
        
        if [ $dataset_total_compressed_size -gt 0 ]; then
            dataset_compression_ratio=$(echo "scale=2; $dataset_total_original_size / $dataset_total_compressed_size" | bc -l)
            dataset_compression_ratio_formatted=$(printf "%.2f" $dataset_compression_ratio)
        else
            dataset_compression_ratio_formatted="0.00"
        fi
        
        if (( $(echo "$dataset_total_compression_time > 0" | bc -l) )); then
            dataset_compression_throughput=$(echo "scale=2; $dataset_total_original_mib / $dataset_total_compression_time" | bc -l)
            dataset_compression_throughput_formatted=$(printf "%.2f" $dataset_compression_throughput)
        else
            dataset_compression_throughput_formatted="0.00"
        fi
        
        if (( $(echo "$dataset_total_decompression_time > 0" | bc -l) )); then
            dataset_decompression_throughput=$(echo "scale=2; $dataset_total_original_mib / $dataset_total_decompression_time" | bc -l)
            dataset_decompression_throughput_formatted=$(printf "%.2f" $dataset_decompression_throughput)
        else
            dataset_decompression_throughput_formatted="0.00"
        fi
        
        # 写入数据集总体报告到数据集日志
        echo "=== $dataset_name 总体统计 ===" >> "$dataset_log_file"
        echo "实际处理文件数量: $dataset_file_count" >> "$dataset_log_file"
        echo "预期文件数量: $expected_count" >> "$dataset_log_file"
        echo "总原始大小: $(printf "%.2f" $dataset_total_original_mib) MiB" >> "$dataset_log_file"
        echo "总压缩大小: $(printf "%.2f" $dataset_total_compressed_mib) MiB" >> "$dataset_log_file"
        echo "总体压缩率: ${dataset_compression_ratio_formatted}x" >> "$dataset_log_file"
        echo "总压缩时间: $(printf "%.2f" $dataset_total_compression_time) 秒" >> "$dataset_log_file"
        echo "总解压时间: $(printf "%.2f" $dataset_total_decompression_time) 秒" >> "$dataset_log_file"
        echo "压缩总体吞吐量: ${dataset_compression_throughput_formatted} MiB/s" >> "$dataset_log_file"
        echo "解压总体吞吐量: ${dataset_decompression_throughput_formatted} MiB/s" >> "$dataset_log_file"
        echo "压缩文件保存目录: $dataset_output_dir/" >> "$dataset_log_file"
        echo "完成时间: $(date)" >> "$dataset_log_file"
        
        # 写入主汇总日志
        echo "=== $dataset_name 汇总 ===" >> "$MAIN_LOG_FILE"
        echo "处理文件数: $dataset_file_count/$expected_count" >> "$MAIN_LOG_FILE"
        echo "总原始大小: $(printf "%.2f" $dataset_total_original_mib) MiB" >> "$MAIN_LOG_FILE"
        echo "总压缩大小: $(printf "%.2f" $dataset_total_compressed_mib) MiB" >> "$MAIN_LOG_FILE"
        echo "总体压缩率: ${dataset_compression_ratio_formatted}x" >> "$MAIN_LOG_FILE"
        echo "压缩总体吞吐量: ${dataset_compression_throughput_formatted} MiB/s" >> "$MAIN_LOG_FILE"
        echo "解压总体吞吐量: ${dataset_decompression_throughput_formatted} MiB/s" >> "$MAIN_LOG_FILE"
        echo "详细日志: $dataset_log_file" >> "$MAIN_LOG_FILE"
        echo "" >> "$MAIN_LOG_FILE"
        
        # 控制台数据集总结
        echo ""
        echo "  📊 $dataset_name 数据集总结:"
        echo "    • 处理文件数: $dataset_file_count/$expected_count"
        echo "    • 总原始大小: $(printf "%.2f" $dataset_total_original_mib) MiB"
        echo "    • 总压缩大小: $(printf "%.2f" $dataset_total_compressed_mib) MiB"
        echo "    • 总体压缩率: ${dataset_compression_ratio_formatted}x"
        echo "    • 压缩总体吞吐量: ${dataset_compression_throughput_formatted} MiB/s"
        echo "    • 解压总体吞吐量: ${dataset_decompression_throughput_formatted} MiB/s"
        echo "    • 详细日志: $dataset_log_file"
        echo ""
    else
        echo "警告: $dataset_name 路径下没有找到任何文件" | tee -a "$dataset_log_file"
    fi
    
    # 清理解压目录
    rm -rf "$dataset_decompress_dir"
    rm -rf "$dataset_output_dir"
    
    # 清理缓存
    echo "清理缓存..."
    sudo echo 3 > /proc/sys/vm/drop_caches 2>/dev/null || echo "无法清理缓存（需要sudo权限）"
}

# 主处理循环
echo "开始批量压缩数据集..."
echo ""

for dataset_info in "${datasets[@]}"; do
    IFS=':' read -r path name count <<< "$dataset_info"
    process_dataset "$path" "$name" "$count"
done

echo "===================="
echo "所有数据集处理完成！"
echo "===================="
echo "主汇总报告: $MAIN_LOG_FILE"
echo "各数据集详细报告: ./compression_*.log"
echo "压缩文件保存在: $OUTPUT_DIR/"