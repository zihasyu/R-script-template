library(ggplot2)
library(dplyr)

# 复用 PerfBarGroup.R / RestoreBarGroup.R 的绘图函数
create_grouped_barplot_with_ci <- function(
    data_matrix,
    group_labels,
    bar_labels,
    fill_colors,
    x_label = "Groups",
    y_label = "Value",
    export_name = NULL,
    export_path = "./",
    width = 16,
    height = 8,
    bar_width = 0.8,
    text_size =12,
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
    remove_x_axis_space = FALSE,
    y_axis_margin = 0.5,
    y_max_fixed = NULL,
    y_breaks_max = NULL,
    data_label_angle = 0
) {
  num_groups <- length(group_labels)
  num_bars <- length(bar_labels)

  if(length(data_matrix) != num_groups) {
    stop(paste("data_matrix必须包含", num_groups, "个分组"))
  }
  if(any(sapply(data_matrix, length) != num_bars)) {
    stop(paste("每个分组必须包含", num_bars, "个柱子的数据"))
  }

  summary_list <- list()
  for(i in 1:num_groups) {
    for(j in 1:num_bars) {
      data_vec <- data_matrix[[i]][[j]]
      n <- sum(!is.na(data_vec))
      mean_val <- mean(data_vec, na.rm = TRUE)
      sd_val <- sd(data_vec, na.rm = TRUE)
      se <- sd_val / sqrt(n)
      ci95 <- if (n > 1) se * qt(0.975, df = n - 1) else 0

      summary_list[[length(summary_list) + 1]] <- data.frame(
        group = group_labels[i],
        bar = bar_labels[j],
        mean = mean_val,
        ymin = mean_val - ci95,
        ymax = mean_val + ci95,
        n = n
      )
    }
  }

  summary_df <- do.call(rbind, summary_list)
  summary_df$group <- factor(summary_df$group, levels = group_labels)
  summary_df$bar <- factor(summary_df$bar, levels = bar_labels)

  group_positions <- (0:(length(group_labels)-1)) * group_spacing
  names(group_positions) <- group_labels
  summary_df$group_pos <- group_positions[as.character(summary_df$group)]

  if(!is.null(y_max_fixed)) {
    y_max <- y_max_fixed
  } else {
    y_max <- max(summary_df$ymax, na.rm = TRUE) * y_max_multiplier
  }

  # 字体设置
  if(use_arial) {
    if(requireNamespace("showtext", quietly = TRUE)) {
      library(showtext)
      if(requireNamespace("sysfonts", quietly = TRUE)) {
        library(sysfonts)
        if(!("Arial" %in% sysfonts::font_families())) {
          tryCatch({
            sysfonts::font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
          }, error = function(e) {
            warning("无法加载Arial字体: ", e$message)
          })
        }
        showtext::showtext_auto()
      }
    } else if(requireNamespace("extrafont", quietly = TRUE)) {
      library(extrafont)
      tryCatch({
        windowsFonts(Arial = windowsFont("Arial"))
      }, error = function(e) {
        warning("无法通过extrafont加载Arial字体: ", e$message)
      })
    }
  }

  y_breaks <- if(!is.null(y_breaks_max)) seq(0, y_breaks_max, 20) else waiver()

  p <- ggplot(summary_df, aes(x = group_pos, y = mean, fill = bar)) +
    geom_col(position = position_dodge(width = dodge_width),
             width = bar_width, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  position = position_dodge(width = dodge_width),
                  width = 0.2, size = 0.5) +
    scale_fill_manual(values = setNames(fill_colors, bar_labels),
                      name = "", labels = bar_labels) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0, 0)),
                       limits = c(0, y_max),
                       breaks = y_breaks) +
    scale_x_continuous(breaks = group_positions, labels = group_labels,
                       expand = expansion(mult = c(y_axis_margin, 0.05), add = c(0, 0))) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()

  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }

  if(remove_x_axis_space) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.y = element_text(size = y_label_size, hjust = 0.5),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
    )
  } else {
    hjust_val <- if(x_text_angle == 0) 0.5 else if(x_text_angle == 45) 1 else 0.5
    vjust_val <- if(x_text_angle == 0) 0.5 else if(x_text_angle == 45) 1 else 0.5

    p <- p + theme(
      axis.text.x = element_text(size = x_axis_text_size, color = "black",
                                angle = x_text_angle, hjust = hjust_val, vjust = vjust_val),
      axis.text.y = element_text(size = y_axis_text_size, color = "black"),
      axis.title.x = element_text(size = x_label_size),
      axis.title.y = element_text(size = y_label_size, hjust = 0.5)
    )
  }

  if(show_data_labels) {
    p <- p + geom_text(aes(label = round(mean, 1)),
                       position = position_dodge(width = dodge_width),
                       vjust = if(data_label_angle == 90) 0.5 else -0.5,
                       hjust = if(data_label_angle == 90) -0.1 else 0.5,
                       angle = data_label_angle,
                       size = text_size)
  }

  if(show_legend) {
    p <- p + theme(
      legend.position = legend_position,
      legend.text = element_text(size = legend_text_size),
      legend.margin = margin(b = 10)
    )
  } else {
    p <- p + theme(legend.position = "none")
  }

  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height, dpi = 300)
  }

  return(p)
}

# ============================================================
# 数据定义
# ============================================================
# 原始数据（百分比数值，去掉%号）
#              mtar+Odess(DCC)  TarReduce(DCC)  mtar+Odess(DCE)  TarReduce(DCE)
# Linux        48.2             75.0            95.3             95.1
# Web          64.5             83.3            93.9             89.4
# Automake     41.9             83.1            96.0             93.1
# Coreutils    40.7             78.6            96.1             93.6
# Gcc          39.3             79.8            95.4             92.9
# React        31.8             67.9            95.8             92.9
# Netty        42.5             67.8            96.3             95.1
# CPython      49.2             65.5            96.6             96.3

datasets <- c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython")

dcc_data <- list(
  c(48.2, 75.0),   # Linux
  c(64.5, 83.3),   # Web
  c(41.9, 83.1),   # Automake
  c(40.7, 78.6),   # Coreutils
  c(39.3, 79.8),   # Gcc
  c(31.8, 67.9),   # React
  c(42.5, 67.8),   # Netty
  c(49.2, 65.5)    # CPython
)

dce_data <- list(
  c(95.3, 95.1),   # Linux
  c(93.9, 89.4),   # Web
  c(96.0, 93.1),   # Automake
  c(96.1, 93.6),   # Coreutils
  c(95.4, 92.9),   # Gcc
  c(95.8, 92.9),   # React
  c(96.3, 95.1),   # Netty
  c(96.6, 96.3)    # CPython
)

# 将每个数值包装成单元素列表（与原函数的 data_matrix 结构一致：每个值是向量）
wrap_as_matrix <- function(raw_list) {
  lapply(raw_list, function(x) {
    list(x[1], x[2])
  })
}

dcc_matrix <- wrap_as_matrix(dcc_data)
dce_matrix <- wrap_as_matrix(dce_data)

# 柱子标签和颜色
bar_labels <- c("MO", "FineTAR")
# MO 使用 #75B8BF（原 PerfBarGroup 中 MO 的颜色），FineTAR 使用 #FF5809（原 FineTAR 的颜色）
fill_colors <- c("#75B8BF", "#FF5809")

# 输出目录
plot_dir <- "./"

# ============================================================
# 图1: DCC
# ============================================================
p_dcc <- create_grouped_barplot_with_ci(
  data_matrix = dcc_matrix,
  group_labels = datasets,
  bar_labels = bar_labels,
  fill_colors = fill_colors,
  x_label = "",
  y_label = "DCC (%)",
  export_name = "DCC_BarGroup.pdf",
  export_path = plot_dir,
  width = 20,
  height = 6,
  bar_width = 0.5,

  x_axis_text_size = 40,
  y_axis_text_size = 40,
  x_label_size = 50,
  y_label_size = 50,
  legend_text_size = 40,
  show_legend = FALSE,
  show_data_labels = TRUE,
  use_arial = TRUE,
  y_max_fixed = 112,
  y_breaks_max = 100,
  dodge_width = 0.5,
  group_spacing = 0.8,
  x_text_angle = 20,
  y_axis_margin = 0.02,
  data_label_angle = 90
)
cat("已生成 DCC 图: DCC_BarGroup.pdf\n")
print(p_dcc)

# ============================================================
# 图2: DCE
# ============================================================
p_dce <- create_grouped_barplot_with_ci(
  data_matrix = dce_matrix,
  group_labels = datasets,
  bar_labels = bar_labels,
  fill_colors = fill_colors,
  x_label = "",
  y_label = "DCE (%)",
  export_name = "DCE_BarGroup.pdf",
  export_path = plot_dir,
  width = 20,
  height =6,
  bar_width = 0.5,

  x_axis_text_size = 40,
  y_axis_text_size = 40,
  x_label_size = 50,
  y_label_size = 50,
  legend_text_size = 40,
  show_legend = FALSE,
  show_data_labels = TRUE,
  use_arial = TRUE,
  y_max_fixed = 125,
  y_breaks_max = 100,
  dodge_width = 0.5,
  group_spacing = 0.8,
  x_text_angle = 20,
  y_axis_margin = 0.02,
  data_label_angle = 90
)
cat("已生成 DCE 图: DCE_BarGroup.pdf\n")
print(p_dce)
