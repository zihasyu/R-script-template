# 安装并加载必要的库
library(ggplot2)
library(cowplot)
library(patternplot)
library(ggpattern)

# 加载必要的库
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
force_scientific <- function(x) {
  format(x, scientific = TRUE, digits = 2)
}

# 定义数据
heights <- c(24.806, 35.0356, 55.8196, 51.628, 23.9654, 31.523, 2.58709, 1.9253,24.806, 35.0356, 55.8196, 51.628, 23.9654, 31.523, 2.58709, 1.9253)

# 创建数据框
df1 <- data.frame(
  group = rep(c('Source','WEB', 'Docker','Log'), each = 4),
  category = rep(c('NTrans', 'Finesse', 'Odess', 'Basic'), times = 4, levels = c('NTrans', 'Finesse', 'Odess', 'Basic')),
  height = c(heights)
)

# 找到所有数据框中的最大高度并增加一些空间
df1$group <- factor(df1$group, levels = c('Source','WEB', 'Docker','Log'))
df1$category <- factor(df1$category, levels = c('NTrans', 'Finesse', 'Odess', 'Basic'))
# 为每个类别分配绝对角度
angle_mapping <- c(NTrans = 0, Finesse = 0, Odess = 45, Basic = 90)
df1$angle <- angle_mapping[df1$category]

max_height <- max(df1$height)
ylim_max <- max_height + 0.01 * max_height

# 创建柱状图
p1 <- ggplot(df1, aes(x = group, y = height, fill = category, pattern = category, pattern_angle = angle)) + 
  geom_bar_pattern(stat = 'identity', position = position_dodge(width = 0.9), 
                   colour = "black", width = 0.9,
                   pattern_density = 0.1,
                   pattern_fill = "black", 
                   pattern_color = "black",
                   pattern_spacing = 0.02) + 
  scale_fill_manual(values = c("#f5deb3", "#1f77b4", "#ac8cf4", "#ff7f0e")) +
  scale_pattern_manual(values = c("none", "stripe", "stripe", "stripe")) +
  labs(y = 'DCR') +
  scale_y_continuous(
    limits = c(0, max_height),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    axis.title.x = element_text(size = 26),
    axis.title.y = element_text(size = 24),
    axis.text = element_text(size = 20, color = "black"),
    aspect.ratio = 0.6,
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # legend.position = "none",
    
    legend.text = element_text(size = 19),
    legend.key.height = unit(1.8, "line"),
    legend.key.width = unit(2, "line"),
    legend.margin = margin(t = 0, unit = 'cm'),
    legend.title = element_blank()
  ) +
  guides(pattern_angle = "none", fill = guide_legend(override.aes = list(pattern_angle = angle_mapping))) +
  theme(axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5))

ggsave('p3-1-2.pdf', plot = p1, width = 10, height = 6)
print(p1)
