# 加载必要的库
library(readxl)
# 设置工作目录为当前文件所在目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
  range_extension = 1.0,  # 控制轴范围的扩展系数
  x_expand_left = 0,      # 新参数：x轴左侧扩展量
  x_expand_right = 0,   # 新参数：x轴右侧扩展量
  y_expand_bottom = 0,    # 新参数：y轴底部扩展量
  y_expand_top = 0.1,     # 新参数：y轴顶部扩展量
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C","#FF5809")
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 数据预处理 - 确保数据格式正确
  if (!is.data.frame(data)) {
    stop("数据必须是数据框")
  }
  
  # 创建长格式数据框用于ggplot
  plot_data <- data.frame()
  col_count <- 0
  
  # 处理每列数据
  for (col_name in names(data)) {
    col_count <- col_count + 1
    if (col_count > length(colors)) break # 确保不超过可用颜色数
    
    # 提取当前列
    col_data <- data[[col_name]]
    
    # 添加到主数据框
    temp_df <- data.frame(
      x = 1:length(col_data),
      y = col_data,
      group = factor(col_count)
    )
    plot_data <- rbind(plot_data, temp_df)
  }
  
  # 检查是否有有效数据
  if (nrow(plot_data) == 0) {
    stop("没有有效的数据可供绘图")
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
  
  # 确定x轴和y轴的范围
  if(is.null(x_lim)) {
    x_range <- range(plot_data$x, na.rm = TRUE)
    # 计算扩展范围
    x_range_width <- diff(x_range)
    x_range <- c(x_range[1], x_range[2] + x_range_width * (range_extension - 1))
  } else {
    x_range <- x_lim
  }
  
  if(is.null(y_lim)) {
    y_range <- range(plot_data$y, na.rm = TRUE)
    # 计算扩展范围
    y_range_width <- diff(y_range)
    y_range <- c(y_range[1], y_range[2] + y_range_width * (range_extension - 1))
  } else {
    y_range <- y_lim
  }
  
  # 调整x刻度，使用add参数来控制扩展
if(is.null(x_breaks)) {
  # 使用自动计算的刻度
  p <- p + scale_x_continuous(
    expand = expansion(add = c(x_expand_left * diff(x_range), 
                             max(0.02, x_expand_right) * diff(x_range))),
    labels = function(breaks) {
      # 给最后一个标签添加额外的空格，使其向左移动
      labels <- as.character(breaks)
      if(length(labels) > 0) {
        labels[length(labels)] <- paste0(labels[length(labels)], "    ")
      }
      return(labels)
    }
  )
} else {
  # 使用自定义刻度，并且确保最后一个刻度标签不会溢出
  p <- p + scale_x_continuous(
    breaks = x_breaks,
    labels = function(breaks) {
      labels <- as.character(breaks)
      if(length(labels) > 0) {
        labels[length(labels)] <- paste0(labels[length(labels)], "  ")
      }
      return(labels)
    },
    expand = expansion(add = c(x_expand_left * diff(x_range), 
                             max(0.02, x_expand_right) * diff(x_range)))
  )
}
  
  # 调整y刻度，使用add参数来控制扩展
  if(is.null(y_breaks)) {
    # 如果没有提供y_breaks，让ggplot2自动处理
    p <- p + scale_y_continuous(
      expand = expansion(add = c(y_expand_bottom * diff(y_range), 
                                y_expand_top * diff(y_range)))
    )
  } else {
    # 检查最大刻度是否接近范围边界，如果太接近就移除
    if(max(y_breaks) > y_range[2] * 0.95) {
      y_breaks <- y_breaks[y_breaks < max(y_breaks)]
    }
    p <- p + scale_y_continuous(
      breaks = y_breaks, 
      labels = y_breaks,
      expand = expansion(add = c(y_expand_bottom * diff(y_range), 
                                y_expand_top * diff(y_range)))
    )
  }
  
  # 应用坐标范围
  if (!is.null(x_lim) || !is.null(y_lim) || range_extension > 1) {
    p <- p + coord_cartesian(xlim = x_range, ylim = y_range)
  }
p <- p + theme(
  # 增加合适的边距
  plot.margin = margin(t = 15, r = 15, b = 10, l = 10, unit = "pt"),
  
  # 确保坐标轴文本有足够空间
  axis.text.x = element_text(
    size = axis_text_size, 
    color = "black",
    margin = margin(t = 5)
  ),
  axis.text.y = element_text(
    size = axis_text_size, 
    color = "black",
    margin = margin(r = 5)
  )
)


  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

drr_data <- read_excel("DCE_ratios.xlsx")

# 使用plot_line_comparison函数绘图
plot_line_comparison(
  data = drr_data,                    # 从Excel读取的DRR表格数据
  x_label = "Backups",                # x轴标签
  y_label = "Ratio",   # y轴标签，使用我们之前讨论的FRR命名
  export_name = "DCE_motivation.pdf"  # 输出文件名
)