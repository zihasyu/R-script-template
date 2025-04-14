rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-client.pdf"
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
init = c(
  2.260041745,
  2.196678636,
  2.736592613,
  2.691426393
)

barWidth = 0.5
x3_offset = c(0, 1, 2, 3)
x1_offset = numeric(4)
x2_offset = numeric(4)
for(i in 1:3) {
  x1_offset[i] = x3_offset[i] - 0.5 * barWidth
  x2_offset[i] = x3_offset[i] + 0.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.2, 3.3), ylim = c(0.14, 3.2)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(1.5, 3),
                     labels = c(1.5, 3)) +
  # 柱状图
  geom_col(aes(x = x3_offset,y = init), width = barWidth, fill = "#CD0000",
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 40, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Overhead (%)", x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MdKuber", "HtmlCv", "TarDiffu", "TarLinux")) +
  theme(axis.title.y = element_text(hjust = 1.1)) +
  # 标签
  geom_text(aes(x = x3_offset, y = init), hjust = 0.5,
            vjust = 0.5, label=c("2.26", "2.20", "2.74", "2.69"), angle = 0, size = 13, nudge_y = 0.3)


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
