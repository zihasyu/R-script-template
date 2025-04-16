
create_comparison_barplot <- function(
    data1, 
    data2, 
    labels, 
    fill_colors = c("#AD0626", "#B79AD1"),
    fill_names = c("Group 1", "Group 2"),
    x_label = "Workloads",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 12,
    height = 6,
    bar_width = 0.35,
    text_size = 13,
    axis_text_size = 42,
    legend_text_size = 30,
    show_legend = FALSE,
    legend_position = c(0.85, 0.85),
    show_data_labels = TRUE,
    use_arial = TRUE,
    y_max_multiplier = 1.35 # 新增参数：y轴上限倍数
) {
  # 加载必要的包
  library(ggplot2)
  
  # 字体设置
  if(use_arial) {
    # 尝试使用showtext包处理字体
    if(requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      
      # 检查字体是否已加载
      if(requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        
        # 检查Arial是否已添加
        if(!("Arial" %in% sysfonts::font_families())) {
          tryCatch({
            sysfonts::font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
          }, error = function(e) {
            warning("无法加载Arial字体: ", e$message)
          })
        }
        showtext::showtext_auto()
      } else {
        warning("sysfonts包不可用，无法加载Arial字体")
      }
    } else if(requireNamespace("extrafont", quietly = TRUE)) {
      # 尝试使用extrafont包
      library(extrafont)
      tryCatch({
        windowsFonts(Arial = windowsFont("Arial"))
      }, error = function(e) {
        warning("无法通过extrafont加载Arial字体: ", e$message)
      })
    } else {
      warning("showtext和extrafont包均不可用，使用系统默认字体")
    }
  }
  
  # 数据准备
  n_bars <- length(data1)
  x_positions <- 0:(n_bars-1)
  x1_positions <- x_positions - 0.5 * bar_width
  x2_positions <- x_positions + 0.5 * bar_width
  y_max <- max(c(data1, data2)) * y_max_multiplier
  # 绘图
  p <- ggplot() + 
    scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +  # 设置y轴上限
    # 柱状图
    geom_col(aes(x = x1_positions, y = data1, fill = fill_names[1]), width = bar_width, 
             color = "black", size = 0.5) +
    geom_col(aes(x = x2_positions, y = data2, fill = fill_names[2]), width = bar_width, 
             color = "black", size = 0.5) +
    # 填充颜色
    scale_fill_manual(name = "", values = setNames(fill_colors, fill_names)) +
    # 主题设置
    theme_classic()
  
  # 添加字体设置（仅当Arial可用时）
  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }
  
  # 其他主题设置
  p <- p + theme(
    axis.text.x = element_text(size = axis_text_size, color = "black"),
    axis.text.y = element_text(size = axis_text_size, color = "black"),
    axis.title.x = element_text(size = axis_text_size),
    axis.title.y = element_text(size = axis_text_size, hjust = 0.5)
  ) +
    # 标签设置
    labs(y = paste(" ", y_label, " "), x = x_label) +
    # x轴刻度设置
    scale_x_continuous(breaks = x_positions, labels = labels)
  
  # 添加数据标签(如果需要)
  if(show_data_labels) {
    p <- p +
      geom_text(aes(x = x2_positions, y = data2), hjust = 0,
                vjust = 0.5, label = round(data2, 2), angle = 90, 
                size = text_size, nudge_y = max(data2) * 0.05) +
      geom_text(aes(x = x1_positions, y = data1), hjust = 0,
                vjust = 0.5, label = round(data1, 2), angle = 90, 
                size = text_size, nudge_y = max(data1) * 0.05)
  }
  
  # 图例设置
  if(show_legend) {
    p <- p + theme(
      legend.position = legend_position,
      legend.text = element_text(size = legend_text_size),
      legend.margin = margin(b = 10)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # 保存文件(如果提供了文件名)
  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height)
  }
  
  return(p)
}
