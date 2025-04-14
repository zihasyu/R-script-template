rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-linux.pdf"
username <- Sys.info()[["user"]]

exportPath = sprintf("%s%s%s", exportPath1, username, exportPath2)

# install.packages("showtext")

library(ggplot2)
library(ggbreak)
library(readxl)
library(extrafont)
library(showtext)  
library(gg.gap)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()
COD = c(37441.2813, 
        4700.4758,
        1816.5276,
        1118.77845
)

CDC = c(64766.7393,
        6925.21205,
        1847.4916,
        787.54015
)

barWidth = 0.35
x3_offset = c(0, 1, 2, 3)
x1_offset = numeric(4)
x2_offset = numeric(4)
for(i in 1:4) {
  x1_offset[i] = x3_offset[i] - 0.5 * barWidth
  x2_offset[i] = x3_offset[i] + 0.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.4, 3.4), ylim = c(3460, 73000)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 30000, 60000),
                     labels = c(0, "30", "60")) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = COD), width = 0.35, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = CDC), width = 0.35, fill = "#B79AD1", 
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
  theme(axis.title.y = element_text(hjust = 1.1)) +
  # 标签
  geom_text(aes(x = x2_offset, y = CDC), hjust = 0,
            vjust = 0.5, label=c("65", "6.9", "1.8", "0.8"), angle = 90, size = 13, nudge_y = 300) +
  geom_text(aes(x = x1_offset, y = COD), hjust = 0,
            vjust = 0.5, label=c("37", "4.7", "1.8", "1.1"), angle = 90, size = 13, nudge_y = 300)


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
