rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-syn-size.pdf"
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

CDC = c(1.258496651,
        1.298695136,
        1.162004662,
        1.146714968,
        0.99233968,
        0.841816436,
        0.770852271,
        0.736969256,
        0.814390962
)
OB = c(71.78838998,
       68.93499407,
       56.41666667,
       59.53268566,
       31.36156708,
       26.38999405,
       21.14055475,
       24.19397072,
       -0
)

barWidth = 0.4
x3_offset = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
x1_offset = numeric(9)
x2_offset = numeric(9)
for(i in 1:9) {
  x1_offset[i] = x3_offset[i] - 0.5 * barWidth
  x2_offset[i] = x3_offset[i] + 0.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
p1 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.1, 8.25), ylim = c(0.122, 2.6)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 2),
                     labels = c(0, 2)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = CDC), width = 0.4, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = OB), width = 0.4, fill = "#2C3359", 
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 22, color = "black")) +
  theme(axis.text.y = element_text(size = 23, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.title.y = element_text(size = 22)) +
  # 设置label内容
  labs(y = "Time Normalization", x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("16k", "128k", "256k", "512k", "1M", "4M", "8M", "16M", "64M")) +
  theme(axis.title.y = element_text(hjust = 0.2)) +
  # 标签
  geom_text(aes(x = x1_offset, y = CDC), hjust = 0,
            vjust = 0.5, label=round(CDC, 2), angle = 90, size = 7.5, nudge_y = 0.05) +
  geom_text(aes(x = 8.25, y = 0), hjust = 0,
            vjust = 0.5, label="NULL", angle = 90, size = 7.5, nudge_y = 0.1)

p2 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.1, 8.25), ylim = c(20, 150)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(30, 120),
                     labels = c(30, 120)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = CDC), width = 0.4, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = OB), width = 0.4, fill = "#2C3359", 
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 22, color = "black")) +
  theme(axis.text.y = element_text(size = 22, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.title.y = element_text(size = 22)) +
  # 设置label内容
  labs(y = NULL, x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("16k", "128k", "256k", "512k", "1M", "4M", "8M", "16M", "64M")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  # 标签
  geom_text(aes(x = x2_offset, y = OB), hjust = 0,
            vjust = 0.5, label=c("71.8", "68.9", "56.4", "59.5", "31.4", "26.4", "21.1", "24.2", " ")
            , angle = 90, size = 7.5, nudge_y = 2)

p2/p1


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 8, height = 3.2)
