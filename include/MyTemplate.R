# 文件名: bar_chart_functions.R

# 辅助函数 - 检查并安装必要的包
check_and_install_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    message("正在安装必要的包: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, repos = "https://cloud.r-project.org/")
  }
  
  # 加载包
  for(pkg in packages) {
    library(pkg, character.only = TRUE)
  }
}

# 基础模板函数 - 通用柱状图生成器
create_grouped_bar_chart <- function(
    # 数据相关参数
  data_list,                     # 列表，每个元素是一组数据
  group_names = NULL,            # 各组名称
  category_labels,               # x轴类别标签
  data_labels = NULL,            # 是否显示数据标签及其内容
  
  # 图表样式参数
  colors = NULL,                 # 各组颜色
  bar_width = 0.35,              # 柱子宽度，保留原脚本值
  
  # 坐标轴参数
  x_limits = NULL,               # x轴范围，默认为NULL允许灵活调整
  y_limits = NULL,               # y轴范围，默认为NULL允许灵活调整
  y_breaks = c(0, 30000, 60000), # y轴刻度位置，保留原脚本值
  y_labels = c(0, "30", "60"),   # y轴刻度标签，保留原脚本值
  x_title = "Bandwidth (bps)",   # x轴标题，保留原脚本值
  y_title = "Time Duration (s)", # y轴标题，保留原脚本值
  y_title_hjust = 1.1,           # y轴标题水平对齐，保留原脚本值
  
  # 文本样式参数 - 保留原脚本的重要样式设置
  font_family = "Arial",         # 字体
  axis_text_size = 42,           # 坐标轴文字大小，保留原脚本值
  axis_title_size = 42,          # 坐标轴标题大小，保留原脚本值
  data_label_size = 13,          # 数据标签大小，保留原脚本值
  data_label_angle = 90,         # 数据标签角度，保留原脚本值
  data_label_nudge_y = 300,      # 数据标签y方向偏移，保留原脚本值
  
  # 输出参数
  export_path = NULL,            # 输出路径，NULL则不保存
  export_width = 10,             # 输出宽度(英寸)，保留原脚本值
  export_height = 5              # 输出高度(英寸)，保留原脚本值
) {
  
  # 检查并安装必要的包（只需要ggplot2）
  check_and_install_packages("ggplot2")
  
  # 参数检查和默认值设置
  num_groups <- length(data_list)
  num_categories <- length(data_list[[1]])
  
  # 检查所有数据组长度是否相同
  if(!all(sapply(data_list, length) == num_categories)) {
    stop("所有数据组必须有相同数量的类别")
  }
  
  # 设置默认组名
  if(is.null(group_names)) {
    group_names <- paste("Group", 1:num_groups)
  }
  
  # 设置默认颜色
  if(is.null(colors)) {
    if(num_groups == 2) {
      # 两组数据时使用原脚本的颜色
      colors <- c("#AD0626", "#B79AD1")
    } else {
      # 多组数据使用彩虹色
      colors <- rainbow(num_groups)
    }
  }
  
  # 创建偏移量
  # 每个类别的中心位置 - 与原脚本一致，从0开始
  category_positions <- 0:(num_categories-1)
  
  # 计算每组内柱子的偏移量 - 使用与原脚本一致的偏移计算
  bar_offsets <- list()
  if(num_groups == 2) {
    # 两组数据时的特殊处理，完全匹配原脚本
    bar_offsets[[1]] <- category_positions - 0.5 * bar_width
    bar_offsets[[2]] <- category_positions + 0.5 * bar_width
  } else {
    # 多组数据的通用处理
    for(g in 1:num_groups) {
      offset <- (g - (num_groups + 1)/2) * bar_width
      bar_offsets[[g]] <- category_positions + offset
    }
  }
  
  # 创建用于绘图的数据框
  plot_data <- data.frame(
    category = rep(1:num_categories, num_groups),
    group = rep(1:num_groups, each = num_categories),
    value = unlist(data_list),
    x_pos = unlist(bar_offsets)
  )
  
  # 创建基础图
  p <- ggplot() +
    theme_classic() +
    theme(text = element_text(family = font_family),
          axis.text.x = element_text(size = axis_text_size, color = "black"),
          axis.text.y = element_text(size = axis_text_size, color = "black"),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size, hjust = y_title_hjust))
  
  # 添加柱状图
  for(g in 1:num_groups) {
    group_data <- plot_data[plot_data$group == g,]
    p <- p + geom_col(data = group_data, 
                      aes(x = x_pos, y = value), 
                      width = bar_width, 
                      fill = colors[g],
                      color = "black", 
                      size = 0.5)
  }
  
  # 添加数据标签
  if(!is.null(data_labels)) {
    if(is.list(data_labels) && length(data_labels) == num_groups) {
      # 不同组使用不同标签
      for(g in 1:num_groups) {
        group_data <- plot_data[plot_data$group == g,]
        p <- p + geom_text(data = group_data,
                           aes(x = x_pos, y = value, label = data_labels[[g]]),
                           hjust = 0, vjust = 0.5,
                           angle = data_label_angle, 
                           size = data_label_size,
                           nudge_y = data_label_nudge_y)
      }
    } else if(length(data_labels) == 1 && data_labels == TRUE) {
      # 使用实际数值作为标签
      p <- p + geom_text(data = plot_data,
                         aes(x = x_pos, y = value, label = round(value, 1)),
                         hjust = 0, vjust = 0.5,
                         angle = data_label_angle, 
                         size = data_label_size,
                         nudge_y = data_label_nudge_y)
    }
  }
  
  # 设置坐标轴
  p <- p + labs(x = x_title, y = y_title) +
    scale_x_continuous(breaks = category_positions, 
                       labels = category_labels)
  
  # 确定y轴下限（重要：确保柱子贴在x轴上）
  y_lower_limit <- 0
  
  # 确定y轴上限
  if(is.null(y_limits)) {
    # 如果未指定上限，使用数据最大值的1.1倍
    y_upper_limit <- max(unlist(data_list)) * 1.1
  } else {
    # 如果指定了y_limits，使用其上限，但保持下限为0
    if(length(y_limits) == 2) {
      y_upper_limit <- y_limits[2]
    } else {
      y_upper_limit <- y_limits
    }
  }
  
  # 设置y轴范围，确保从0开始
  p <- p + coord_cartesian(
    xlim = x_limits,
    ylim = c(y_lower_limit, y_upper_limit)
  )
  
  # 设置y轴刻度
  p <- p + scale_y_continuous(breaks = y_breaks,
                              labels = y_labels,
                              expand = c(0, 0))  # 这个参数确保柱子贴在x轴上
  
  # 保存图表
  if(!is.null(export_path)) {
    ggsave(export_path, plot = p, width = export_width, height = export_height)
  }
  
  return(p)
}

# 预设函数 - 简化版，确保柱子贴在x轴上
create_bandwidth_chart <- function(
    # 必要参数
  COD_values,            # COD数据
  CDC_values,            # CDC数据
  
  # 可选参数
  bandwidth_labels = c("10M", "100M", "500M", "5G"),
  COD_labels = c("37", "4.7", "1.8", "1.1"),
  CDC_labels = c("65", "6.9", "1.8", "0.8"),
  
  # 坐标轴范围 - 允许灵活调整
  x_limits = NULL, 
  y_limits = NULL,
  
  # 输出参数
  export_path = NULL,    # 输出路径，NULL则不保存
  export_name = NULL     # 输出文件名，NULL则不保存
) {
  
  # 完整输出路径
  full_export_path = NULL
  if(!is.null(export_path) && !is.null(export_name)) {
    # 确保路径结尾有分隔符
    if(substr(export_path, nchar(export_path), nchar(export_path)) != "/" && 
       substr(export_path, nchar(export_path), nchar(export_path)) != "\\") {
      export_path <- paste0(export_path, "/")
    }
    full_export_path = paste0(export_path, export_name)
  }
  
  # 调用通用图表函数
  create_grouped_bar_chart(
    # 数据参数
    data_list = list(COD_values, CDC_values),
    group_names = c("COD", "CDC"),
    category_labels = bandwidth_labels,
    data_labels = list(COD_labels, CDC_labels),
    
    # 坐标轴范围 - 可选
    x_limits = x_limits,
    y_limits = y_limits,
    
    # 输出参数
    export_path = full_export_path
    # 其他参数使用基础函数的默认值，已经与原脚本匹配
  )
}