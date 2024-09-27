library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 创建示例数据
data <- data.frame(
  category = rep(c('Source', 'WEB', 'Docker',  'Log'), each = 4),
  part = rep(c('Deduplication', 'Local Compression', 'Feature-based Delta', 'Locality Delta'), times = 4),
  value = c(1, 1, 1,1, 1, 1, 1, 1,1, 1,1, 1, 1,1, 1, 1)
)
# 计算每个类别的总和
total_values <- aggregate(value ~ category, data = data, sum)
max_height <- max(total_values$value)
# 定义颜色
colors <- c( 'Deduplication' = '#f5deb3', 'Local Compression' = '#ac8cf4', 
             'Feature-based Delta' = '#ff7f0e', 'Locality Delta' = '#4a90e2')

# 创建堆叠柱状图
p <- ggplot(data, aes(x = category, y = value, fill = part)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = colors) +
  labs(y = 'Compression ratio', x = 'Category') +
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
ggsave('../plot/8-impact.pdf', plot = p, width = 10, height = 5)
print(p)