plot_line_comparison <- function(
  data,
  export_path = "./",
  export_name = "line_comparison.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "X_Value",
  y_label = "Y_Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.1,  # 新参数：控制轴范围的扩展系数
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 准备数据 - 转换为长格式
  plot_data <- data.frame(x = rep(seq_len(nrow(data)), ncol(data)))
  plot_data$y <- unlist(data)
  plot_data$group <- factor(rep(1:ncol(data), each = nrow(data)))
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
       geom_line(size = line_size) +
       scale_color_manual(values = colors[1:ncol(data)], guide = "none")
  
  # 基本主题设置
  p <- p + 
    # 白底，没有上边框和右边框
    theme_classic() +
    # 设置坐标轴上字体大小
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    # 设置字体样式
    theme(text = element_text(family = "Arial")) +
    # 设置label字体大小
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    # ylabel位置
    theme(axis.title.y = element_text(hjust = 0.5)) +
    # 设置label内容
    labs(y = y_label, x = x_label)
  
  # 确定x轴和y轴的范围
  if(is.null(x_lim)) {
    x_range <- range(plot_data$x)
    # 计算扩展范围
    x_range_width <- diff(x_range)
    x_range <- c(x_range[1], x_range[2] + x_range_width * (range_extension - 1))
  } else {
    x_range <- x_lim
  }
  
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  # 调整x刻度，确保最大值显示完全
  if(is.null(x_breaks)) {
    # 如果没有提供x_breaks，让ggplot2自动处理
    p <- p + scale_x_continuous(expand = expansion(mult = c(0.02, 0.08)))
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(x_breaks) > x_range[2] * 0.95) {
      x_breaks <- x_breaks[x_breaks < max(x_breaks)]
    }
    p <- p + scale_x_continuous(breaks = x_breaks, labels = x_breaks, 
                               expand = expansion(mult = c(0.02, 0.08)))
  }
  
  # 调整y刻度，确保最大值显示完全
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.02, 0.08)))
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(y_breaks) > y_range[2] * 0.95) {
      y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    }
    p <- p + scale_y_continuous(breaks = y_breaks, labels = y_breaks,
                               expand = expansion(mult = c(0.02, 0.08)))
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

# 同样修改 plot_line_comparison_xcdf 函数
plot_line_comparison_xcdf <- function(
  data,
  export_path = "./",
  export_name = "line_comparison_cdf.pdf",
  x_lim = NULL,
  y_lim = NULL,
  line_size = 2.8,
  axis_text_size = 42,
  x_title_size = 48,
  y_title_size = 45,
  x_breaks = NULL,
  y_breaks = NULL,
  x_label = "Cumulative Progress",
  y_label = "Value",
  plot_width = 10,
  plot_height = 5,
  range_extension = 1.1,  # 新参数：控制轴范围的扩展系数
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 转换输入，确保是列表形式
  if (is.data.frame(data)) {
    data_list <- as.list(data)
  } else if (is.list(data)) {
    data_list <- data
  } else {
    stop("Data must be a data frame or list")
  }
  
  # 创建一个空的数据框来存储转换后的数据
  plot_data <- data.frame()
  
  # 处理每列数据，并归一化x轴
  col_count <- 0
  for (col_name in names(data_list)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break # 确保不超过可用颜色数
    
    # 提取当前列的非NA值
    col_data <- data_list[[col_name]][!is.na(data_list[[col_name]])]
    if (length(col_data) == 0) next # 跳过空列
    
    # 创建归一化的x值（从0到1）
    x_norm <- seq(0, 1, length.out = length(col_data))
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = x_norm,
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 如果没有有效数据，返回错误
  if (nrow(plot_data) == 0) {
    stop("No valid data found to plot")
  }
  
  # 创建基本图表
  p <- ggplot(plot_data, aes(x = x, y = y, color = group)) +
       geom_line(size = line_size) +
       scale_color_manual(values = colors[1:col_count], guide = "none")
  
  # 基本主题设置
  p <- p + 
    # 白底，没有上边框和右边框
    theme_classic() +
    # 设置坐标轴上字体大小
    theme(axis.text = element_text(size = axis_text_size, color = "black")) +
    # 设置字体样式
    theme(text = element_text(family = "Arial")) +
    # 设置label字体大小
    theme(axis.title.x = element_text(size = x_title_size)) +
    theme(axis.title.y = element_text(size = y_title_size)) +
    # ylabel位置
    theme(axis.title.y = element_text(hjust = 0.5)) +
    # 设置label内容
    labs(y = y_label, x = x_label)
  
  # 默认x轴范围为0-1，考虑扩展系数
  if(is.null(x_lim)) {
    x_range <- c(0, 1)
  } else {
    x_range <- x_lim
  }
  
  # y轴范围，考虑扩展系数
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  # 调整x刻度，确保最大值显示完全
  if(is.null(x_breaks)) {
    # 如果没有提供x_breaks，默认使用0, 0.25, 0.5, 0.75, 1.0
    p <- p + scale_x_continuous(
      breaks = seq(0, 1, 0.25),
      labels = seq(0, 1, 0.25),
      expand = expansion(mult = c(0.02, 0.08))
    )
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(x_breaks) > x_range[2] * 0.95) {
      x_breaks <- x_breaks[x_breaks < max(x_breaks)]
    }
    p <- p + scale_x_continuous(
      breaks = x_breaks, 
      labels = x_breaks,
      expand = expansion(mult = c(0.02, 0.08))
    )
  }
  
  # 调整y刻度，确保最大值显示完全
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理
    p <- p + scale_y_continuous(expand = expansion(mult = c(0.02, 0.08)))
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(y_breaks) > y_range[2] * 0.95) {
      y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    }
    p <- p + scale_y_continuous(
      breaks = y_breaks, 
      labels = y_breaks,
      expand = expansion(mult = c(0.02, 0.08))
    )
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

create_legend_pdf <- function(
  series_names,
  export_path = "./",
  export_name = "legend.pdf",
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C"),
  legend_title = NULL,
  legend_position = "horizontal", # "horizontal" 或 "vertical"
  text_size = 36,
  title_size = 40,
  plot_width = 10,
  plot_height = 3
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 尝试加载cowplot
  if(!requireNamespace("cowplot", quietly = TRUE)) {
    install.packages("cowplot")
    library(cowplot)
  } else {
    library(cowplot)
  }
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 确保series_names长度不超过可用颜色数量
  if(length(series_names) > length(colors)) {
    warning("提供的系列名称数量超过可用颜色数量，将只使用前", length(colors), "个系列")
    series_names <- series_names[1:length(colors)]
  }
  
  # 创建一个更可靠的数据框用于生成图例
  dummy_df <- data.frame(
    x = 1:length(series_names),
    y = 1:length(series_names),
    group = factor(series_names, levels = series_names)
  )
  
  # 创建一个包含实际线条的图（而不是使用点）
  p <- ggplot(dummy_df, aes(x, y, color = group)) +
       geom_line(size = 2) +  # 使用实际可见的线条
       scale_color_manual(
         values = colors[1:length(series_names)],
         name = legend_title,
         guide = guide_legend(
           title.position = "top",
           title.hjust = 0.5,
           nrow = if(legend_position == "horizontal") 1 else length(series_names)
         )
       ) +
       theme_void() +
       theme(
         text = element_text(family = "Arial"),
         legend.text = element_text(size = text_size),
         legend.title = element_text(size = title_size),
         legend.position = "bottom",  # 确保图例在可见位置
         legend.box.margin = margin(0, 0, 0, 0)
       )
  
  # 为了确保生成正确的图例，先保存完整图形
  temp_plot_path <- paste(export_path, "temp_plot.pdf", sep = "")
  ggsave(temp_plot_path, plot = p, width = plot_width, height = plot_height)
  
  # 创建一个只有图例的图
  legend_only <- p + theme(legend.position = "bottom") +
                   guides(color = guide_legend(nrow = if(legend_position == "horizontal") 1 else length(series_names)))
  
  # 直接导出图例
  ggsave(paste(export_path, export_name, sep = ""), 
         plot = legend_only, 
         width = plot_width, 
         height = plot_height)
  
  # 删除临时文件
  if(file.exists(temp_plot_path)) {
    file.remove(temp_plot_path)
  }
  
  return(legend_only)
}