library(here)
library(readxl)
source(here("MyR", "Bar.R"))


create_grouped_barplot_bw_pattern <- function(
    data_matrix,
    group_labels,
    bar_labels,
    x_label = "Groups",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 16,
    height = 8,
    bar_width = 0.8,
    text_size = 10,
    x_axis_text_size = 14,
    y_axis_text_size = 14,
    x_label_size = 14,
    y_label_size = 14,
    legend_text_size = 12,
    show_legend = TRUE,
    legend_position = "top",
    show_data_labels = FALSE,
    use_arial = TRUE,
    y_max_multiplier = 1.2,
    dodge_width = 0.9,
    group_spacing = 1.0,
    x_text_angle = 0,
    remove_x_axis_space = FALSE,  # 新增参数
    y_axis_margin = 0.5
) {
  library(ggplot2)
  library(dplyr)
  library(ggpattern)
  
  # 数据验证
  num_groups <- length(data_matrix)
  num_bars <- length(bar_labels)
  
  if(num_groups != length(group_labels)) {
    stop("data_matrix 的长度必须与 group_labels 的长度相同")
  }
  
  # 计算统计信息
  summary_list <- list()
  for(i in 1:num_groups) {
    group_data <- data_matrix[[i]]
    for(j in 1:num_bars) {
      bar_data <- group_data[[j]]
      if(length(bar_data) > 0) {
        mean_val <- mean(bar_data, na.rm = TRUE)
        sd_val <- sd(bar_data, na.rm = TRUE)
        n <- length(bar_data)
        se <- sd_val / sqrt(n)
        ci <- 1.96 * se
        
        summary_list[[length(summary_list) + 1]] <- data.frame(
          group = group_labels[i],
          bar = bar_labels[j],
          mean = mean_val,
          sd = sd_val,
          se = se,
          ci = ci,
          ymin = max(0, mean_val - ci),
          ymax = mean_val + ci,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  summary_df <- do.call(rbind, summary_list)
  summary_df$group <- factor(summary_df$group, levels = group_labels)
  summary_df$bar <- factor(summary_df$bar, levels = bar_labels)
  
  # 计算分组位置
  group_positions <- seq(1, by = group_spacing, length.out = num_groups)
  summary_df$group_pos <- group_positions[as.numeric(summary_df$group)]
  
  y_max <- max(summary_df$ymax, na.rm = TRUE) * y_max_multiplier
  
  # 设置字体
  if(use_arial) {
    library(showtext)
    library(extrafont)
    font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
    showtext_auto()
    windowsFonts(Arial = windowsFont("Arial"))
  }
  
  # 创建图形 - 使用黑白图案
  p <- ggplot(summary_df, aes(x = group_pos, y = mean)) +
    geom_col_pattern(
      aes(fill = bar, pattern = bar),
      position = position_dodge(width = dodge_width), 
      width = bar_width, 
      color = "black",
      pattern_fill = "black",
      pattern_colour = "black",
      pattern_density = 0.3,
      pattern_spacing = 0.03,
      pattern_angle = 45
    ) +
    geom_errorbar(
      aes(ymin = ymin, ymax = ymax, group = bar),
      position = position_dodge(width = dodge_width),
      width = 0.2, 
      linewidth = 0.5,  # 修改：使用linewidth替代size
      color = "black"
    )
  
  # 添加数据标签（如果需要）
  if(show_data_labels) {
    p <- p + geom_text(
      aes(label = sprintf("%.1f", mean), group = bar),
      position = position_dodge(width = dodge_width),
      vjust = -0.5,
      size = text_size / 3,
      family = ifelse(use_arial, "Arial", "sans")
    )
  }
  
  p <- p +
    # 填充色：第一个黑色，其余白色
    scale_fill_manual(
      values = setNames(c("black", "white", "white", "white"), bar_labels),
      name = "",
      labels = bar_labels
    ) +
    
    # 图案：第一个无图案，其余有不同图案
    scale_pattern_manual(
      values = setNames(c("none", "stripe", "crosshatch", "circle"), bar_labels),
      name = "",
      labels = bar_labels
    ) +
    
    # 手动合并图例，确保pattern信息正确显示
    guides(
      fill = guide_legend(
        override.aes = list(
          pattern = c("none", "stripe", "crosshatch", "circle"),
          pattern_fill = "black",
          pattern_colour = "black",
          pattern_density = 0.3,
          pattern_spacing = 0.03
        )
      ),
      pattern = "none"  # 隐藏单独的pattern图例
    ) +
    
    # Y轴设置 - 确保从0开始
    scale_y_continuous(
      limits = c(0, y_max),
      expand = if(remove_x_axis_space) {
        expansion(mult = c(0, 0.05))  # Y轴下边距为0，上边距5%
      } else {
        expansion(mult = c(y_axis_margin, 0.05))
      }
    ) +
    
    # X轴设置
    scale_x_continuous(
      breaks = group_positions,
      labels = group_labels,
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    
    labs(
      x = x_label,
      y = y_label
    ) +
    
    theme_classic() +
    theme(
      text = element_text(family = ifelse(use_arial, "Arial", "sans"), size = text_size, color = "black"),
      axis.text.x = element_text(
        size = x_axis_text_size, 
        angle = x_text_angle, 
        hjust = 0.5,      # 水平居中对齐
        vjust = 0.9,      # 垂直居中对齐
        margin = margin(t = 5),
        color = "black"
      ),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.x = element_text(size = x_label_size, color = "black"),
      axis.title.y = element_text(size = y_label_size, color = "black"),
      legend.position = ifelse(show_legend, legend_position, "none"),
      legend.text = element_text(size = legend_text_size, color = "black"),
      legend.key.size = unit(1.2, "cm"),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8),
      axis.ticks.length = unit(0.2, "cm"),
      # 确保X轴与Y=0对齐
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # 导出图形
  if(!is.null(export_name)) {
    ggsave(
      filename = file.path(export_path, export_name),
      plot = p,
      width = width,
      height = height,
      dpi = 300
    )
  }
  
  return(p)
}


# 读取Excel数据
excel_path <- here("Paper/dsn-专利和软著/Restore.xlsx")
compression_data <- read_excel(excel_path)

# 获取列名
column_names <- colnames(compression_data)
num_columns <- length(column_names)

# 参数设置
GROUP_SIZE <- 4  # 每组的方法数量（TL, ML, MO, FineTAR）
INTERVAL <- 8    # 数据集数量

# 确保输出文件夹存在
plot_dir <- here("Paper/dsn-专利和软著")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 设定方法的顺序和颜色
desired_order <- c("Lz4", "mtar", "mtar+Odess", "TarReduce")
fill_colors <- c("#F2BE5C", "#B79AD1", "#75B8BF", "#FF5809")

# 准备分组数据矩阵（先按原始顺序收集所有数据）
all_data_matrix <- list()
all_group_labels <- c()

# 为8个数据集收集数据（按原始顺序）
for(i in 1:INTERVAL) {
  # 计算当前组包含的列索引
  col_indices <- seq(from = i, to = num_columns, by = INTERVAL)
  
  # 如果最终选择的列数超过GROUP_SIZE，只取前GROUP_SIZE个
  if(length(col_indices) > GROUP_SIZE) {
    col_indices <- col_indices[1:GROUP_SIZE]
  }
  
  # 提取当前组的列名和系统名称
  current_col_names <- column_names[col_indices]
  system_names <- gsub("SA_BiSearch_", "", current_col_names)
  
  # 获取数据集名称
  dataset_parts <- strsplit(system_names[length(system_names)], "_")[[1]]
  dataset_name <- dataset_parts[length(dataset_parts)]
  all_group_labels[i] <- dataset_name
  
  # 创建当前组的数据列表
  group_data <- list()
  
  # 提取每列数据
  for(j in 1:length(col_indices)) {
    col_idx <- col_indices[j]
    col_data <- compression_data[[col_idx]]
    valid_data <- col_data[!is.na(col_data)]
    
    if(length(valid_data) > 0) {
      group_data[[j]] <- valid_data
    } else {
      # 如果没有有效数据，创建一个空向量
      group_data[[j]] <- numeric(0)
    }
  }
  
  # 按照desired_order重新排列数据
  original_order <- c("Lz4", "mtar", "mtar+Odess", "TarReduce")
  order_index <- match(desired_order, original_order)
  
  # 重新排列数据
  reordered_data <- group_data[order_index[1:4]]
  all_data_matrix[[i]] <- reordered_data
  
  cat("收集数据集", i, ":", dataset_name, "\n")
}

# ================================
# 自定义分组（数据集）顺序
# ================================
# 您可以在这里自定义8个数据集的显示顺序
# 请根据 all_group_labels 的内容来设置您想要的顺序

# 首先打印所有数据集名称，方便您选择顺序
cat("所有数据集名称：", paste(all_group_labels, collapse = ", "), "\n")

# 自定义数据集显示顺序（用于数据匹配的原始名称）
custom_group_order <- c(
    "linux",
    "WEB",
    "automake",
    "coreutils", 
    "gcc",
    "react",
    "netty",
    "Cpython"
)

# 自定义显示名称（用于图表显示的美化名称）
display_names <- c(
    "Linux",           # linux -> Linux
    "Web",             # WEB保持不变
    "Automake",        # automake -> Automake
    "Coreutils",       # coreutils -> Coreutils
    "Gcc",             # gcc -> GCC
    "React",           # react -> React
    "Netty",           # netty -> Netty
    "CPython"          # Cpython -> CPython
)

# 验证自定义顺序和显示名称长度是否匹配
if(length(custom_group_order) != length(display_names)) {
  stop("custom_group_order 和 display_names 的长度必须相同")
}

# 如果您不确定数据集名称，可以使用原始顺序
if(length(custom_group_order) != length(all_group_labels) || 
   !all(custom_group_order %in% all_group_labels)) {
  warning("自定义顺序与实际数据集不匹配，使用原始顺序")
  custom_group_order <- all_group_labels
  display_names <- all_group_labels  # 使用原始名称作为显示名称
}

# 按照自定义顺序重新排列数据
data_matrix <- list()
group_labels <- c()        # 用于显示的美化标签

for(i in 1:length(custom_group_order)) {
  target_dataset <- custom_group_order[i]
  original_index <- which(all_group_labels == target_dataset)
  
  if(length(original_index) > 0) {
    data_matrix[[i]] <- all_data_matrix[[original_index]]
    group_labels[i] <- display_names[i]  # 使用美化后的显示名称
    cat("重排序：位置", i, "-> 数据集", target_dataset, "-> 显示为", display_names[i], "\n")
  } else {
    warning(paste("找不到数据集:", target_dataset))
  }
}

# 设置柱子标签
bar_labels <- desired_order[1:4]
fill_colors_4 <- fill_colors[1:4]


p_bw <- create_grouped_barplot_bw_pattern(
  data_matrix = data_matrix,
  group_labels = group_labels,
  bar_labels = bar_labels,
  x_label = "",
  y_label = "Speed (MiB/s)",
  export_name = "Restore_bw.pdf",
  export_path = plot_dir,
  width = 20,
  height = 8,
  bar_width = 0.7,
  text_size = 40,
  x_axis_text_size = 40,
  y_axis_text_size = 40,
  x_label_size = 40,
  y_label_size = 50,
  legend_text_size = 40,
  show_legend = TRUE,
  show_data_labels = FALSE,
  use_arial = TRUE,
  y_max_multiplier = 1.15,
  dodge_width = 0.7,
  group_spacing = 0.8,  
  x_text_angle = 15,
  remove_x_axis_space = TRUE,  # 关键修改：让X轴与Y=0贴合
  y_axis_margin = 0.05
)
cat("已生成分组柱状图（自定义分组顺序）: Restore.pdf\n")
print(p)

