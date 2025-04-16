library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 定义数据
heights <- c(
  1, 3.603833638, 1.55096045, 3.628551317, 3.66412315,
  1, 1.026278374, 1.711722312, 1.792371427, 1.912977399,
  1, 1.183556405, 3.775310095, 3.599009658, 3.719517576,
  1, 3.43214565, 1.599743103, 3.528817156, 3.54210879,
  1, 1.440685179, 1.792211937, 1.931375678, 2.128896691
)

group_order <- c('CentOS
', 'LKT', 'WEB', 'vmdk','chromium')

# 创建数据框
df1 <- data.frame(
  group = factor(rep(group_order, each = 5), levels = group_order),
  category = factor(rep(c(' perChunk',' FP_only', ' SF_only', 'FTCG', 'FTCG+SCS'), times = 5),
                    levels = c(' perChunk',' FP_only', ' SF_only', 'FTCG', 'FTCG+SCS')),
  height = heights
)

max_height <- max(df1$height)

# 创建柱状图
p1 <- ggplot(df1, aes(x = group, y = height, fill = category)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), 
           colour = "black", width = 0.8) + 
  scale_fill_manual(values = c("#f5deb3", "#4a90e2", "#ac8cf4", "#ff7f0e","#FF5733")) +
  labs(y = 'Grouped Efficiency') +
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
    aspect.ratio = 0.5,
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.text = element_text(size = 19),
    legend.key.height = unit(1.0, "line"),
    legend.key.width = unit(1.0, "line"),
    legend.margin = margin(t = 0, unit = 'cm'),
    legend.title = element_blank()
  ) +
  theme(axis.title.x = element_blank())

ggsave('./GE_4.png', plot = p1, width = 10, height = 5)
print(p1)
