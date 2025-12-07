library(here)
library(ggplot2)
library(ggpattern)
library(showtext)
library(extrafont)

# 设置字体
font_add('Arial', 'C:/Windows/Fonts/arial.ttf')
showtext_auto()
windowsFonts(Arial = windowsFont("Arial"))

# 创建数据框
drr_data <- data.frame(
  Dataset = rep(c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython"), each = 4),
  Method = rep(c("Lz4", "mtar", "mtar+Odess", "TarReduce"), times = 8),
  DRR = c(
    # Linux
    3.4, 20.7, 55.1, 84.1,
    # Web
    2.5, 68.6, 254.4, 246.3,
    # Automake
    3.0, 7.0, 18.8, 23.8,
    # Coreutils
    3.4, 6.1, 12.1, 18.3,
    # Gcc
    3.3, 14.4, 27.1, 35.6,
    # React
    1.4, 16.8, 24.7, 27.6,
    # Netty
    4.7, 19.1, 45.6, 63.0,
    # CPython
    2.5, 19.5, 54.0, 61.1
  )
)

# 设置因子顺序
drr_data$Dataset <- factor(drr_data$Dataset, levels = c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython"))
drr_data$Method <- factor(drr_data$Method, levels = c("Lz4", "mtar", "mtar+Odess", "TarReduce"))

# 计算分组位置
group_spacing <- 0.8
num_groups <- 8
group_positions <- seq(1, by = group_spacing, length.out = num_groups)
drr_data$group_pos <- group_positions[as.numeric(drr_data$Dataset)]

# 计算Y轴最大值
y_max <- max(drr_data$DRR) * 1.15

# 创建黑白图案柱状图
p <- ggplot(drr_data, aes(x = group_pos, y = DRR)) +
  geom_col_pattern(
    aes(fill = Method, pattern = Method),
    position = position_dodge(width = 0.7), 
    width = 0.7, 
    color = "black",
    pattern_fill = "black",
    pattern_colour = "black",
    pattern_density = 0.3,
    pattern_spacing = 0.03,
    pattern_angle = 45
  ) +
  
  # 填充色：第一个黑色，其余白色
  scale_fill_manual(
    values = c("Lz4" = "black", "mtar" = "white", "mtar+Odess" = "white", "TarReduce" = "white"),
    name = "",
    labels = c("Lz4", "mtar", "mtar+Odess", "TarReduce")
  ) +
  
  # 图案：第一个无图案，其余有不同图案
  scale_pattern_manual(
    values = c("Lz4" = "none", "mtar" = "stripe", "mtar+Odess" = "crosshatch", "TarReduce" = "circle"),
    name = "",
    labels = c("Lz4", "mtar", "mtar+Odess", "TarReduce")
  ) +
  
  # 手动合并图例
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
    pattern = "none"
  ) +
  
  # Y轴设置
  scale_y_continuous(
    limits = c(0, y_max),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # X轴设置
  scale_x_continuous(
    breaks = group_positions,
    labels = levels(drr_data$Dataset),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  labs(
    x = "",
    y = "DRR"
  ) +
  
  theme_classic() +
  theme(
    text = element_text(family = "Arial", size = 40, color = "black"),
    axis.text.x = element_text(
      size = 40, 
      angle = 15, 
      hjust = 0.5,      # 水平居中对齐
      vjust = 0.9,      # 垂直居中对齐
      margin = margin(t = 5),
      color = "black"
    ),
    axis.text.y = element_text(size = 40, color = "black"),
    axis.title.x = element_text(size = 40, color = "black"),
    axis.title.y = element_text(size = 50, color = "black"),
    legend.position = "top",
    legend.text = element_text(size = 40, color = "black"),
    legend.key.size = unit(1.2, "cm"),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# 保存图形
plot_dir <- here("Paper/dsn-专利和软著")
if(!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}

ggsave(
  filename = file.path(plot_dir, "DRR_comparison_bw.pdf"),
  plot = p,
  width = 20,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(plot_dir, "DRR_comparison_bw.png"),
  plot = p,
  width = 20,
  height = 8,
  dpi = 300
)

cat("已生成DRR对比图：DRR_comparison_bw.pdf 和 DRR_comparison_bw.png\n")
print(p)