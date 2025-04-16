library(ggplot2)
library(ggpattern)

# 创建简化数据框
df_test <- data.frame(
  group = c('A', 'B', 'C', 'D'),
  category = factor(c('NTrans', 'Finesse', 'Odess', 'Basic'), levels = c('NTrans', 'Finesse', 'Odess', 'Basic')),
  height = c(10, 15, 20, 25)
)

# 为每个类别分配绝对角度
angle_mapping <- c(NTrans = 0, Finesse = 0, Odess = 45, Basic = 90)
df_test$angle <- angle_mapping[df_test$category]

# 创建图表
p_test <- ggplot(df_test, aes(x = group, y = height, fill = category, pattern = category, pattern_angle = angle)) + 
  geom_bar_pattern(stat = 'identity', position = 'dodge', 
                   colour = "black", width = 0.9,
                   pattern_density = 0.1,
                   pattern_fill = "black", 
                   pattern_color = "black",
                   pattern_spacing = 0.02) + 
  scale_fill_manual(values = c("#F5DEB3", "#4682B4", "#9370DB", "#FFA500")) +
  scale_pattern_manual(values = c("none", "stripe", "stripe", "stripe")) +
  guides(pattern_angle = "none", fill = guide_legend(override.aes = list(pattern_angle = df_test$angle))) +  # 同步图例
  labs(y = 'Height') +
  theme_bw() +
  theme(legend.position = "right")

print(p_test)