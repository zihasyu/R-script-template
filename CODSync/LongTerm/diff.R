rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-diff.pdf"
username <- Sys.info()[["user"]]

exportPath = sprintf("%s%s%s", exportPath1, username, exportPath2)


library(ggplot2)
library(ggbreak)
library(readxl)
library(extrafont)
library(showtext)  
library(gg.gap)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()
COD = c(3290.3974, 
        408.3006,
        153.34445,
        94.44085
)

CDC = c(7054.36375,
        743.07045,
        189.3873,
        72.74485
)

OB = c(31340.95874,
       6201.4796,
       3789.55584,
       3518.67078
)

barWidth = 0.4
x3_offset = c(0, 1.4, 2.8, 4.2)
x1_offset = numeric(4)
x2_offset = numeric(4)
for(i in 1:4) {
  x1_offset[i] = x3_offset[i] - 1 * barWidth
  x2_offset[i] = x3_offset[i] + 1 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
p1 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.5, 4.8), ylim = c(84, 1800)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 1500),
                     labels = c(0, "1.5")) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = COD), width = 0.4, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x3_offset,y = CDC), width = 0.4, fill = "#B79AD1", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = OB), width = 0.4, fill = "#F2BE5C", 
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 42, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Time Duration (s)", x = "Bandwidth (bps)") +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("10M", "100M", "500M", "5G")) +
  theme(axis.title.y = element_text(hjust = 0.33)) +
  # 标签
  geom_text(aes(x = x3_offset, y = CDC), hjust = 0,
            vjust = 0.5, label=round(CDC/1000, 2), angle = 90, size = 13, nudge_y = 10) +
  geom_text(aes(x = x1_offset, y = COD), hjust = 0,
            vjust = 0.5, label=round(COD/1000, 2), angle = 90, size = 13, nudge_y = 10)

p2 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.5, 4.8), ylim = c(3000, 48000)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(10000, 40000),
                     labels = c("10", "40")) +
  # 柱状图
  geom_col(aes(x = -0.4,y = 3290.3974), width = 0.4, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = 0,y = 7054.36375), width = 0.4, fill = "#B79AD1", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = OB), width = 0.4, fill = "#F2BE5C", 
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 42, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = NULL, x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("10Mbps", "100Mbps", "500Mbps", "5Gbps")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  # 标签
  geom_text(aes(x = x2_offset, y = OB), hjust = 0,
            vjust = 0.5, label=c("31", "6.2", "3.8", "3.5")
            , angle = 90, size = 13, nudge_y = 100) +
  geom_text(aes(x = -0.4, y = 3290.3974), hjust = 0,
            vjust = 0.5, label="3.3", angle = 90, size = 13, nudge_y = 100) +
  geom_text(aes(x = 0, y = 7054.36375), hjust = 0,
            vjust = 0.5, label="7.1", angle = 90, size = 13, nudge_y = 100)

p2/p1


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
