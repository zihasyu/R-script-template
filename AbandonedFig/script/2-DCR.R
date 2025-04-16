library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 定义数据
heights <- c(
  24.806,55.8196,23.9654,9.65933,26.9041,75.639,78.8404,73.7776,35.1477,24.4157,16.8225,24.247,14.3198,7.15452,15.197,17.9285,16.2301,16.2427,13.1618,14.2966
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

ggsave('../plot/2-DCR.pdf', plot = p1, width = 10, height = 5)
print(p1)