library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 定义数据
heights <- c(
  52.2385,70.3703,73.6354,51.1926,101.594,75.5342,86.0136,95.3816,82.8394,148.287,127.821,133.472,141.81,127.305,206.824,60.9146,55.7637,119.431,50.0585,70.221
)

group_order <- c('Linux', 'Chromium', 'WEB', 'Log')
# 创建数据框
df1 <- data.frame(
  group = factor(rep(group_order, each = 5), levels = group_order),
  category = factor(rep(c('NTrans', 'Finesse', 'Odess', 'Palantir','BiSearch'), times = 4),
                    levels = c('NTrans', 'Finesse', 'Odess', 'Palantir','BiSearch')),
  height = heights
)

max_height <- max(df1$height)

# 创建柱状图
p1 <- ggplot(df1, aes(x = group, y = height, fill = category)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.8), 
           colour = "black", width = 0.8) + 
  scale_fill_manual(values = c("#f5deb3", "#4a90e2", "#ac8cf4", "#ff7f0e","#FF5733")) +
  labs(y = 'System Throughput(MiB/s)') +
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

ggsave('../plot/7-ST.pdf', plot = p1, width = 10, height = 5)
print(p1)