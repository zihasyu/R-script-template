# -*- coding: utf-8 -*-
import os
import glob
import re
import codecs
import csv

# 寻找所有匹配的文件
files = glob.glob("Dedup_Skip_*.txt")
if not files:
    print("未找到符合名称模式的文件")
    exit()

# 创建一个目录来存储CSV文件
output_dir = "casecount_ratio_data"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# 存储所有数据集和最大行数
all_datasets = {}
max_length = 0

# 首先处理所有文件并提取数据
for f in files:
    with codecs.open(f, 'r', encoding='utf-8') as fp:
        content = fp.read()
        
    # 使用正则表达式提取每个version小节的数据
    sections = re.findall(r'dedup done.*?-{17}END-{31}', content, re.DOTALL)
    
    ratios = []
    for section in sections:
        # 提取casecount和unique chunk num
        casecount_match = re.search(r'casecount is (\d+)', section)
        unique_chunk_match = re.search(r'unique chunk num: (\d+)', section)
        
        if casecount_match and unique_chunk_match:
            casecount = float(casecount_match.group(1))
            unique_chunk = float(unique_chunk_match.group(1))
            ratio = casecount / unique_chunk
            ratios.append(ratio)
    
    # 更新最大长度
    max_length = max(max_length, len(ratios))
    
    # 从文件名提取数据标签
    label_name = os.path.basename(f)[len("Dedup_Skip_"):-4]
    
    # 存储数据
    all_datasets[label_name] = ratios
    
    # 计算CDF值并保存单独的CSV文件
    x_values = list(range(0, len(ratios)))
    x_values = [i*100/len(ratios) for i in x_values]
    
    # 创建CSV文件名
    csv_filename = os.path.join(output_dir, f"{label_name}.csv")
    
    # 写入CSV文件
    with open(csv_filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['CDF (%)', 'Casecount/Unique Chunk Ratio'])
        for x, y in zip(x_values, ratios):
            writer.writerow([x, y])
    
    print(f"数据已保存到 '{csv_filename}'")

# 创建一个合并的CSV文件，保存所有数据
merged_csv_file = os.path.join(output_dir, "all_data.csv")
with open(merged_csv_file, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    
    # 写入标题行，包括所有数据集
    header = list(all_datasets.keys())
    writer.writerow(header)
    
    # 为每个行创建一行，使用最大长度
    for i in range(max_length):
        row = []
        
        # 添加每个数据集对应的ratio值
        for label, ratios in all_datasets.items():
            # 如果当前数据集有这个索引的数据，就添加它
            if i < len(ratios):
                row.append(ratios[i])
            else:
                # 如果没有，添加空值
                row.append('')
        writer.writerow(row)

print(f"合并数据已保存到 '{merged_csv_file}'")