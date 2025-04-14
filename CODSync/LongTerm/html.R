rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-html.pdf"
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
COD = c(328.12015, 
        35.81405,
        15.5971,
        11.2979
)

CDC = c(597.9901,
        67.74595,
        21.32045,
        11.3225
)

OB = c(3848.1329,
       654.2464,
       377.9585,
       331.5933
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
  coord_cartesian(xlim =c(-0.5, 4.8), ylim = c(9.5, 210)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 200),
                     labels = c(0, "0.2")) +
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
            vjust = 0.5, label=c("", "0.04", "0.02", "0.01"), angle = 90, size = 13, nudge_y = 1.5) +
  geom_text(aes(x = x1_offset, y = COD), hjust = 0,
            vjust = 0.5, label=c("", "0.07", "0.02", "0.01"), angle = 90, size = 13, nudge_y = 1.5)

p2 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.5, 4.8), ylim = c(300, 6300)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(1000, 6000),
                     labels = c("1.0", "6.0")) +
  # 柱状图
  # geom_col(aes(x = x1_offset,y = CDC), width = 0.4, fill = "#B79AD1", 
  #          color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = OB), width = 0.4, fill = "#F2BE5C", 
           color = "black", size = 0.5) +
  geom_col(aes(x = -0.4,y = 328.12015), width = 0.4, fill = "#AD0626", 
           color = "black", size = 0.5) +
  geom_col(aes(x = 0,y = 597.9901), width = 0.4, fill = "#B79AD1", 
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
            vjust = 0.5, label=c("3.8", "0.65", "0.38", "0.33")
            , angle = 90, size = 13, nudge_y = 100) +
  geom_text(aes(x = -0.4, y = 328.12015), hjust = 0,
            vjust = 0.5, label="0.33"
            , angle = 90, size = 13, nudge_y = 100) +
  geom_text(aes(x = 0, y = 597.9901), hjust = 0,
            vjust = 0.5, label="0.60"
            , angle = 90, size = 13, nudge_y = 100)

p2/p1


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
