library(here)

# --- 预计算数据的分组柱状图函数 ---
# 与 create_grouped_barplot_with_ci 视觉风格一致，但接受预计算的 mean 和 CI95
create_grouped_barplot_precomputed <- function(
    summary_df,                 # 数据框：group, bar, mean, ymin, ymax
    group_labels,               # 分组的标签（显示顺序）
    bar_labels,                 # 柱子的标签
    fill_colors,                # 柱子的颜色
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
    remove_x_axis_space = FALSE,
    y_axis_margin = 0.5
) {
  library(ggplot2)
  library(dplyr)

  summary_df$group <- factor(summary_df$group, levels = group_labels)
  summary_df$bar <- factor(summary_df$bar, levels = bar_labels)

  # 创建分组位置
  group_positions <- (0:(length(group_labels)-1)) * group_spacing
  names(group_positions) <- group_labels
  summary_df$group_pos <- group_positions[as.character(summary_df$group)]

  # 计算y轴上限
  y_max <- max(summary_df$ymax, na.rm = TRUE) * y_max_multiplier

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

  # 创建图形
  p <- ggplot(summary_df, aes(x = group_pos, y = mean, fill = bar)) +
    geom_col(position = position_dodge(width = dodge_width),
             width = bar_width, color = "black", size = 0.3) +
    geom_errorbar(aes(ymin = ymin, ymax = ymax),
                  position = position_dodge(width = dodge_width),
                  width = 0.2, size = 0.5) +
    scale_fill_manual(values = setNames(fill_colors, bar_labels),
                      name = "", labels = bar_labels) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05), add = c(0, 0)),
                       limits = c(0, y_max)) +
    scale_x_continuous(breaks = group_positions, labels = group_labels,
                       expand = expansion(mult = c(y_axis_margin, 0.05), add = c(0, 0))) +
    labs(x = x_label, y = paste(" ", y_label, " ")) +
    theme_classic()

  # 字体应用
  if(use_arial) {
    p <- p + theme(text = element_text(family = "Arial"))
  }

  # 主题设置
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

  # 数据标签
  if(show_data_labels) {
    p <- p + geom_text(aes(label = round(mean, 2)),
                       position = position_dodge(width = dodge_width),
                       vjust = -0.5, size = text_size)
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

  # 保存文件
  if(!is.null(export_name)) {
    ggsave(paste0(export_path, export_name), plot = p, width = width, height = height, dpi = 300)
  }

  return(p)
}

# ============================================================
# 数据处理和绘图
# ============================================================

# 读取CSV数据
csv_path <- here("Paper/fast27-one-shot/fast27-throughput/commit_throughput.csv")
raw_data <- read.csv(csv_path, stringsAsFactors = FALSE)

# 确保输出文件夹存在
plot_dir <- here("Paper/fast27-one-shot/fast27-throughput/PerfBar")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

# 方法的显示顺序和颜色
desired_order <- c("TarReduce(LZ4)", "TarReduce(deflate)", "Depth1", "Depth50")
# fill_colors <- c("#F2BE5C", "#B79AD1", "#75B8BF", "#FF5809")
fill_colors <- c("#FF5809", "#3d90d1", "#eedece", "#30384c")

# 自定义数据集显示顺序
custom_group_order <- c("Linux", "WEB", "Automake", "Coreutils", "GCC", "React", "Netty", "Cpython")
display_names <- c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython")

# 验证
if(length(custom_group_order) != length(display_names)) {
  stop("custom_group_order 和 display_names 的长度必须相同")
}

# 构建预计算数据的 summary_df
summary_list <- list()
for(i in seq_along(custom_group_order)) {
  dataset_key <- custom_group_order[i]
  dataset_display <- display_names[i]
  subset_data <- raw_data[raw_data$Dataset == dataset_key, ]

  for(j in seq_along(desired_order)) {
    method <- desired_order[j]
    row <- subset_data[subset_data$Method == method, ]

    if(nrow(row) > 0) {
      summary_list[[length(summary_list) + 1]] <- data.frame(
        group = dataset_display,
        bar = method,
        mean = row$Mean,
        ymin = row$Mean - row$CI95,
        ymax = row$Mean + row$CI95,
        stringsAsFactors = FALSE
      )
    } else {
      warning(paste("找不到数据:", dataset_key, method))
    }
  }
}

summary_df <- do.call(rbind, summary_list)

# 创建分组柱状图
p <- create_grouped_barplot_precomputed(
  summary_df = summary_df,
  group_labels = display_names,
  bar_labels = desired_order,
  fill_colors = fill_colors,
  x_label = "",
  y_label = "Speed (MiB/s)",
  export_name = "fast27_commit_throughput.pdf",
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
  show_legend = FALSE,
  show_data_labels = FALSE,
  use_arial = TRUE,
  y_max_multiplier = 1.15,
  dodge_width = 0.7,
  group_spacing = 0.8,
  x_text_angle = 20,
  y_axis_margin = 0.02
)
cat("已生成分组柱状图: fast27_commit_throughput.pdf\n")
print(p)
