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
err_data <- data.frame(
  Dataset = rep(c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython"), each = 2),
  Method = rep(c("mtar+Odess", "TarReduce"), times = 8),
  ERR = c(
    # Linux
    45.7, 68.8,
    # Web
    128.8, 219.3,
    # Automake
    17.5, 20.6,
    # Coreutils
    11.6, 16.9,
    # Gcc
    24.7, 30.1,
    # React
    22.5, 26.3,
    # Netty
    39.2, 48.1,
    # CPython
    44.7, 55.3
  )
)

# 设置因子顺序
err_data$Dataset <- factor(err_data$Dataset, levels = c("Linux", "Web", "Automake", "Coreutils", "Gcc", "React", "Netty", "CPython"))
err_data$Method <- factor(err_data$Method, levels = c("mtar+Odess", "TarReduce"))

# 计算分组位置
group_spacing <- 0.8
num_groups <- 8
group_positions <- seq(1, by = group_spacing, length.out = num_groups)
err_data$group_pos <- group_positions[as.numeric(err_data$Dataset)]

# 计算Y轴最大值
y_max <- max(err_data$ERR) * 1.15

# 创建黑白图案柱状图
p <- ggplot(err_data, aes(x = group_pos, y = ERR)) +
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
  
  # 填充色：两个都是白色
  scale_fill_manual(
    values = c("mtar+Odess" = "white", "TarReduce" = "white"),
    name = "",
    labels = c("mtar+Odess", "TarReduce")
  ) +
  
  # 图案：使用不同图案区分
  scale_pattern_manual(
    values = c("mtar+Odess" = "stripe", "TarReduce" = "crosshatch"),
    name = "",
    labels = c("mtar+Odess", "TarReduce")
  ) +
  
  # 手动合并图例
  guides(
    fill = guide_legend(
      override.aes = list(
        pattern = c("stripe", "crosshatch"),
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
    labels = levels(err_data$Dataset),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  
  labs(
    x = "",
    y = "ERR"
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
  filename = file.path(plot_dir, "ERR_comparison_bw.pdf"),
  plot = p,
  width = 20,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(plot_dir, "ERR_comparison_bw.png"),
  plot = p,
  width = 20,
  height = 8,
  dpi = 300
)

cat("已生成ERR对比图：ERR_comparison_bw.pdf 和 ERR_comparison_bw.png\n")
print(p)