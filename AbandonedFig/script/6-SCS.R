library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# 定义数据
heights <- c(
191.171,255.963,2774.03,1557.98,6133.12,286.174,381.97,4216.39,1552.11,112726,811.596,1098.75,12005.2,1556.73,48991.9,108.877,145.951,1612.74,1613.42,2956.42
  )

# 创建数据框
df1 <- data.frame(
  group = rep(c('Linux', 'Chromium', 'WEB', 'Log'), each = 5),
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
  labs(y = 'Speed (MiB/s)') +
  # scale_y_continuous(
  #   limits = c(0, max_height),
  #   expand = expansion(mult = c(0, 0.1))
  # ) +
  scale_y_log10(
  limits = c(NA, NA),
  expand = expansion(mult = c(0, 0.1)),
  breaks = scales::trans_breaks("log10", function(x) 10^x),
  labels = scales::trans_format("log10", scales::math_format(10^.x))
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

ggsave('../plot/6-SCS.pdf', plot = p1, width = 10, height = 5)
print(p1)