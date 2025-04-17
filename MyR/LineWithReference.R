plot_line_with_points_ref <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32,      # 图例文本大小(如果显示)
  ref_line = TRUE,           # 是否显示参考线
  ref_line_y = 0,             # 参考线y值
  ref_line_type = "dashed",   # 参考线类型 ("dashed", "dotted", "solid"等)
  ref_line_color = "#2C3359",  # 参考线颜色
  ref_line_size = 3         # 参考线粗细
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(data[, 1])
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_values <- unlist(data[, -1])  # 提取所有y值
    y_range <- range(y_values)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(data[, 1], n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(unlist(data[, -1]), n = 5)
  }
  
  # 将数据转换为长格式，用于ggplot2
  plot_data <- reshape2::melt(data, id.vars = names(data)[1], 
                             measure.vars = names(data)[-1],
                             variable.name = "series", value.name = "value")
  
  # 如果提供了自定义列名，更新系列名称
  if (!is.null(column_names)) {
    levels(plot_data$series) <- column_names
  }
  
  # 创建基本绘图
  p <- ggplot(plot_data, aes_string(x = names(data)[1], y = "value", 
                                   group = "series", color = "series", shape = "series")) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    geom_line(size = line_size) +
    geom_point(size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 1.1)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 添加参考线
  if (ref_line) {
    p <- p + geom_hline(yintercept = ref_line_y, 
                       linetype = ref_line_type, 
                       color = ref_line_color, 
                       size = ref_line_size)
  }
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_line_with_selected_points_ref <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  num_points = 10,            # 每条线上均匀显示的点数
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32,      # 图例文本大小(如果显示)
  include_endpoints = TRUE,   # 是否始终包含端点
  ref_line = TRUE,           # 是否显示参考线
  ref_line_y = 0,             # 参考线y值
  ref_line_type = "dashed",   # 参考线类型 ("dashed", "dotted", "solid"等)
  ref_line_color = "#2C3359",  # 参考线颜色
  ref_line_size = 3         # 参考线粗细
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  library(reshape2)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(data[, 1])
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_values <- unlist(data[, -1])  # 提取所有y值
    y_range <- range(y_values)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(data[, 1], n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(unlist(data[, -1]), n = 5)
  }
  
  # 将数据转换为长格式，用于ggplot2
  plot_data <- reshape2::melt(data, id.vars = names(data)[1], 
                             measure.vars = names(data)[-1],
                             variable.name = "series", value.name = "value")
  
  # 如果提供了自定义列名，更新系列名称
  if (!is.null(column_names)) {
    levels(plot_data$series) <- column_names
  }
  
  # 创建均匀选择点的函数
  select_points <- function(df, n) {
    # 分割数据按系列
    series_data <- split(df, df$series)
    result <- data.frame()
    
    for (s in names(series_data)) {
      s_data <- series_data[[s]]
      total_points <- nrow(s_data)
      
      # 处理点数不足的情况
      if (total_points <= n) {
        selected <- s_data
      } else {
        # 根据包含端点设置的策略选择点
        if (include_endpoints) {
          if (n >= 2) {
            # 始终包括第一个和最后一个点，其余均匀分布
            interval <- (total_points - 1) / (n - 1)
            indices <- round(seq(1, total_points, by = interval))
            # 确保不超出范围
            indices <- indices[indices <= total_points]
            selected <- s_data[indices, ]
          } else if (n == 1) {
            # 如果只需一个点，取中间点
            selected <- s_data[ceiling(total_points/2), ]
          } else {
            # 如果n为0，不选择任何点
            selected <- s_data[0, ]
          }
        } else {
          # 均匀选择n个点
          interval <- total_points / n
          indices <- round(seq(1, total_points, by = interval))
          # 确保不超出范围
          indices <- indices[indices <= total_points]
          selected <- s_data[indices, ]
        }
      }
      
      result <- rbind(result, selected)
    }
    
    return(result)
  }
  
  # 选择要显示点的数据行
  points_data <- select_points(plot_data, num_points)
  
  # 创建基本绘图
  p <- ggplot() +
    coord_cartesian(xlim = x_lim, ylim = y_lim)
  
  # 添加参考线（如果需要）
  if (ref_line) {
    p <- p + geom_hline(yintercept = ref_line_y, 
                       linetype = ref_line_type, 
                       color = ref_line_color, 
                       size = ref_line_size)
  }
  
  # 继续添加其他图层
  p <- p +
    # 绘制所有线
    geom_line(data = plot_data, 
              aes_string(x = names(data)[1], y = "value", 
                        group = "series", color = "series"),
              size = line_size) +
    # 只在选定的点上绘制符号
    geom_point(data = points_data, 
              aes_string(x = names(data)[1], y = "value", 
                        group = "series", color = "series", shape = "series"),
              size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 1.1)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_with_sampled_points_ref <- function(
  data,                       # 数据框，第一列为x轴数据，其余列为不同线条的y值
  column_names = NULL,        # 列名(用于图例)，如果为NULL则使用数据框的列名
  colors = NULL,              # 颜色列表，如果为NULL则自动生成
  shapes = NULL,              # 形状列表，如果为NULL则自动生成 
  sample_size = 10,           # 每条线均匀抽样的点数
  export_path = "./",         # 导出路径
  export_name = "line_plot.pdf", # 导出文件名
  x_lim = NULL,               # x轴范围，如果为NULL则自动确定
  y_lim = NULL,               # y轴范围，如果为NULL则自动确定
  line_size = 3,              # 线条粗细
  point_size = 9,             # 点大小
  stroke_size = 4.5,          # 点边框粗细
  axis_text_size = 42,        # 坐标轴文本大小
  x_title_size = 48,          # x轴标题大小
  y_title_size = 41,          # y轴标题大小
  x_label = "X",              # x轴标签
  y_label = "Y",              # y轴标签
  x_breaks = NULL,            # x轴刻度，如果为NULL则自动生成
  y_breaks = NULL,            # y轴刻度，如果为NULL则自动生成
  plot_width = 10,            # 导出图形宽度
  plot_height = 5,            # 导出图形高度
  show_legend = FALSE,        # 默认不显示图例
  legend_position = "right",  # 图例位置(如果显示)
  legend_text_size = 32,      # 图例文本大小(如果显示)
  include_endpoints = TRUE,   # 是否始终包含端点
  ref_line = TRUE,           # 是否显示参考线
  ref_line_y = 0,             # 参考线y值
  ref_line_type = "dashed",   # 参考线类型 ("dashed", "dotted", "solid"等)
  ref_line_color = "#2C3359",  # 参考线颜色
  ref_line_size = 3         # 参考线粗细
) {
  # 加载必要的库
  library(ggplot2)
  library(extrafont)
  library(showtext)
  library(reshape2)
  
  # 字体设置
  font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
  showtext_auto()
  windowsFonts(Arial = windowsFont("Arial"))
  
  # 检查数据输入
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  
  # 确定列数(第一列为x轴，其余为y轴数据)
  n_series <- ncol(data) - 1
  if (n_series < 1) {
    stop("Data frame must have at least two columns: x values and at least one y series")
  }
  
  # 设置列名（用于图例）
  if (is.null(column_names)) {
    column_names <- colnames(data)[-1]  # 排除第一列(x轴)的名称
  } else if (length(column_names) != n_series) {
    warning("column_names length does not match number of data series. Using data frame column names.")
    column_names <- colnames(data)[-1]
  }
  
  # 设置颜色
  if (is.null(colors)) {
    # 默认颜色方案
    default_colors <- c("#AD0626", "#2C3359", "#B79AD1", "#75B8BF", "#F2BE5C")
    # 如果系列多于默认颜色，则循环使用
    colors <- rep(default_colors, length.out = n_series)
  } else if (length(colors) < n_series) {
    warning("Not enough colors provided. Recycling colors.")
    colors <- rep(colors, length.out = n_series)
  }
  
  # 设置形状
  if (is.null(shapes)) {
    # 默认形状列表：三角形、菱形、正方形、圆形等
    default_shapes <- c(23, 24, 22, 21, 25)
    shapes <- rep(default_shapes, length.out = n_series)
  } else if (length(shapes) < n_series) {
    warning("Not enough shapes provided. Recycling shapes.")
    shapes <- rep(shapes, length.out = n_series)
  }
  
  # 对数据进行均匀抽样的函数
  sample_data <- function(df, n, include_ends = TRUE) {
    x_col <- names(df)[1]
    result <- data.frame()
    
    # 对每个Y系列进行抽样
    for (i in 2:ncol(df)) {
      y_col <- names(df)[i]
      temp_df <- data.frame(x = df[[x_col]], y = df[[y_col]], series = y_col)
      
      total_points <- nrow(temp_df)
      
      # 处理点数不足的情况
      if (total_points <= n) {
        selected <- temp_df
      } else {
        # 根据包含端点设置的策略选择点
        if (include_ends && n >= 2) {
          # 始终包括第一个和最后一个点，其余均匀分布
          interval <- (total_points - 1) / (n - 1)
          indices <- round(seq(1, total_points, by = interval))
          # 确保不超出范围且无重复
          indices <- unique(indices[indices <= total_points])
          selected <- temp_df[indices, ]
        } else {
          # 均匀选择n个点
          interval <- total_points / n
          indices <- round(seq(interval/2, total_points, by = interval))
          # 确保不超出范围
          indices <- indices[indices <= total_points]
          selected <- temp_df[indices, ]
        }
      }
      
      result <- rbind(result, selected)
    }
    
    return(result)
  }
  
  # 抽样数据
  sampled_data <- sample_data(data, sample_size, include_endpoints)
  
  # 将系列名称转换为因子并赋予自定义名称
  if (!is.null(column_names)) {
    # 创建映射
    name_map <- setNames(column_names, colnames(data)[-1])
    # 应用映射
    sampled_data$series <- factor(sampled_data$series, 
                                levels = colnames(data)[-1], 
                                labels = name_map[colnames(data)[-1]])
  } else {
    sampled_data$series <- factor(sampled_data$series, levels = colnames(data)[-1])
  }
  
  # 设置x轴范围
  if (is.null(x_lim)) {
    x_range <- range(sampled_data$x)
    x_margin <- (x_range[2] - x_range[1]) * 0.05
    x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
  }
  
  # 设置y轴范围
  if (is.null(y_lim)) {
    y_range <- range(sampled_data$y)
    y_margin <- (y_range[2] - y_range[1]) * 0.05
    y_lim <- c(max(0, y_range[1] - y_margin), y_range[2] + y_margin)  # 确保下限不小于0
  }
  
  # 设置x轴刻度
  if (is.null(x_breaks)) {
    x_breaks <- pretty(sampled_data$x, n = 5)
  }
  
  # 设置y轴刻度
  if (is.null(y_breaks)) {
    y_breaks <- pretty(sampled_data$y, n = 5)
  }
  
  # 创建绘图
  p <- ggplot(sampled_data, aes(x = x, y = y, group = series, color = series, shape = series)) +
    coord_cartesian(xlim = x_lim, ylim = y_lim)
  
  # 添加参考线（如果需要）
  if (ref_line) {
    p <- p + geom_hline(yintercept = ref_line_y, 
                       linetype = ref_line_type, 
                       color = ref_line_color, 
                       size = ref_line_size)
  }
  
  # 继续添加其他图层
  p <- p +
    # 绘制线条 - 仅使用抽样点
    geom_line(size = line_size) +
    # 绘制点 - 仅使用抽样点
    geom_point(size = point_size, stroke = stroke_size) +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    theme_classic() +
    theme(axis.text = element_text(size = axis_text_size, color = "black"),
          text = element_text(family = "Arial"),
          axis.title.x = element_text(size = x_title_size),
          axis.title.y = element_text(size = y_title_size, hjust = 0.5)) +
    labs(y = y_label, x = x_label) +
    scale_x_continuous(breaks = x_breaks, labels = x_breaks) +
    scale_y_continuous(breaks = y_breaks, labels = y_breaks)
  
  # 处理图例
  if (show_legend) {
    p <- p + 
      guides(color = guide_legend(title = NULL),
             shape = guide_legend(title = NULL)) +
      theme(legend.position = legend_position,
            legend.text = element_text(size = legend_text_size))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件
  ggsave(paste(export_path, export_name, sep = ""), plot = p, width = plot_width, height = plot_height)
  
  return(p)
}

plot_line_comparison_ref <- function(
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
  range_extension = 1.1,  # 控制轴范围的扩展系数
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C"),
  ref_line = TRUE,        # 是否显示参考线，默认为TRUE
  ref_line_y = 0,         # 参考线y值
  ref_line_type = "dashed", # 参考线类型 ("dashed", "dotted", "solid"等)
  ref_line_color = "#B79AD1", # 参考线颜色
  ref_line_size = 3        # 参考线粗细，默认为3
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
  p <- ggplot(plot_data, aes(x = x, y = y, color = group))
  
  # 添加参考线（如果需要）
  if (ref_line) {
    p <- p + geom_hline(yintercept = ref_line_y, 
                       linetype = ref_line_type, 
                       color = ref_line_color, 
                       size = ref_line_size)
  }
  
  # 添加数据线
  p <- p + geom_line(size = line_size) +
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
plot_line_comparison_xcdf_ref <- function(
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
  range_extension = 1.1,  # 控制轴范围的扩展系数
  colors = c("#AD0626", "#B79AD1", "#75B8BF", "#F2BE5C"),
  ref_line = TRUE,        # 是否显示参考线，默认为TRUE
  ref_line_y = 0,         # 参考线y值
  ref_line_type = "dashed", # 参考线类型 ("dashed", "dotted", "solid"等)
  ref_line_color = "#B79AD1", # 参考线颜色
  ref_line_size = 3        # 参考线粗细，默认为3
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
  p <- ggplot(plot_data, aes(x = x, y = y, color = group))
  
  # 添加参考线（如果需要）
  if (ref_line) {
    p <- p + geom_hline(yintercept = ref_line_y, 
                       linetype = ref_line_type, 
                       color = ref_line_color, 
                       size = ref_line_size)
  }
  
  # 添加数据线
  p <- p + geom_line(size = line_size) +
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



# 创建示例数据
x <- seq(0, 10, length.out = 21)
y1 <- sin(x)
y2 <- cos(x)
df <- data.frame(x = x, y1 = y1)

# 使用默认参考线（在y=0处）
plot_line_with_points_ref(df, ref_line_y = 0.5)

# # 使用自定义参考线
# plot_with_sampled_points_ref(df, 
#                             ref_line = TRUE, 
#                             ref_line_y = 0.5, 
#                             # ref_line_type = "solid", 
#                             ref_line_color = "#AD0626",
#                             ref_line_size = 3)