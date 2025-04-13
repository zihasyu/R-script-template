library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 创建示例数据
data <- data.frame(
  category = rep(c('Linux', 'Chromium', 'Web', 'Log', 'Automake', 'bash', 'coreutils', 'fdisk', 'glibc', 'smalltalk', 'GNU GCC'), each = 3),
  part = rep(c('Deduplication', 'Local Compression', 'Delta Compression'), times = 11),
  value = c(
    6.721606, 13.042394, 70.857606,
    14.674222, 78.148178, 44.551822,
    20.624037, 32.436963, 213.463037,
    1.000018, 5.740692, 2.959308,
    2.157962, 2.546908, 20.153092,
    1.712497, 1.865493, 7.734507,
    2.414382, 4.888178, 11.711822,
    2.181158, 3.785262, 6.414738,
    4.893038, 9.838962, 39.361038,
    3.88995, 6.82345, 25.17655,
    3.363368, 7.553632, 24.246368
  )
)

# 固定 category 的顺序
data$category <- factor(data$category, levels = c('Linux', 'Chromium', 'Web', 'Log', 'Automake', 'bash', 
                                                  'coreutils', 'fdisk', 'glibc', 'smalltalk', 'GNU GCC'))

# 固定 part 的顺序
data$part <- factor(data$part, levels = c('Delta Compression', 'Local Compression', 'Deduplication'))

# 计算每个类别的总和
total_values <- aggregate(value ~ category, data = data, sum)
max_height <- max(total_values$value)

# 定义颜色
colors <- c('Delta Compression' = '#ff7f0e','Local Compression' = '#ac8cf4', 
            'Deduplication' = '#f5deb3' )

# 创建堆叠柱状图
p <- ggplot(data, aes(x = category, y = value, fill = part)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = colors) +
  labs(y = 'Compression Ratio', x = 'Category') +
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
    aspect.ratio = 0.33,
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
ggsave('./plot/impact.pdf', plot = p, width = 15, height = 5)
print(p)