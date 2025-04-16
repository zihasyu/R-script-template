library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 创建示例数据
data <- data.frame(
  category = rep(c('Linux', 'Chromium', 'WEB', 'Log'), each = 6),
  part = rep(c('Deduplication', 'Local Compression', 'Locality Match','Locality Delta','Feature-based Match','Feature-based Delta'), times = 4),
  value = c(85.2564,4.59822,0.00973535,64.6234,33.9497,42.8017,739.354,5.55645,0.0282299,69.6156,10.1979,24.1337,514.128,1.40798,0.0102741,6.56336,9.03805,95.9007,56.1908,9.68207,0.0281502,101.856,11.2657,0.395646)
)
# 计算每个类别的总和
total_values <- aggregate(value ~ category, data = data, sum)
max_height <- max(total_values$value)
# 定义颜色
colors <- c( 'Deduplication' = '#f5deb3', 'Local Compression' = '#ac8cf4', 'Locality Match'='#FF5733',
             'Locality Delta' = '#ff7f0e', 'Feature-based Match' = '#4a90e2','Feature-based Delta'='#00F5FF')

# 创建堆叠柱状图
p <- ggplot(data, aes(x = category, y = value, fill = part)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = colors) +
  labs(y = 'Time (s)', x = 'Category') +
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
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + # 设置图例为两行
  theme(axis.title.x = element_blank())
ggsave('../plot/10-TimeBreakdown.pdf', plot = p, width = 10, height = 5)
print(p)