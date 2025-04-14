rm(list = ls())

exp = "LongTerm"
exportPath = "C:/Users/YangJingyuan/Desktop/"
exportName = "exp-longterm-500m.pdf"

root = "D:/RProject/CODSync"
setwd(sprintf("%s/%s", root, exp))

library(ggplot2)
library(readxl)
library(extrafont)
library(showtext)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

CODSync = c(6.4, 
            149.8, 37.4)
CDC = c(6.1,
        188.4, 44.1)
Obsync0 = c(372.6, 
            3743, 764.2)
Obsync50 = c(620.4, 
             9239, 2192)

barWidth = 0.4
x3_offset = c(0, 1.8, 3.6)
x1_offset = numeric(3)
x2_offset = numeric(3)
x4_offset = numeric(3)
x5_offset = numeric(3)
for(i in 1:3) {
  x1_offset[i] = x3_offset[i] - 1.5 * barWidth
  x2_offset[i] = x3_offset[i] - 0.5 * barWidth
  x4_offset[i] = x3_offset[i] + 0.5 * barWidth
  x5_offset[i] = x3_offset[i] + 1.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.7, 4.4), ylim = c(2.2, 14000000)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = CODSync, fill = "CODSync"), width = 0.4, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = CDC, fill = "CDC"), width = 0.4, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x4_offset,y = Obsync0, fill = "Obsync0"), width = 0.4, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x5_offset,y = Obsync50, fill = "Obsync50"), width = 0.4,
           color = "black", size = 0.5) +
  
  # 白底，没有上边框和右边框
  theme_classic() +
  # 对数轴
  # scale_y_log10() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 39, color = "black")) +
  theme(axis.text.y = element_text(size = 39, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Time Duration", x = "Dataset Names") +
  # 隐藏x轴label
  # theme(axis.title.x = element_blank()) +
  # ylabel位置
  # theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MD", "Diffuser", "SYN-4m")) +
  scale_y_continuous(breaks = c(10, 1000, 100000, 10000000),
                     labels = c(expression(10^1), expression(10^3), expression(10^5), expression(10^7)),
                     trans = 'log2') +
  # 设置文本，hjust和vjust设置对齐基准点
  geom_text(aes(x = x1_offset, y = round(CODSync, 2)), hjust = 0,
            vjust = 0.5, label=CODSync, angle = 90, size = 12, nudge_y = 0.15) +
  geom_text(aes(x = x2_offset, y = round(CDC, 2)), hjust = 0,
            vjust = 0.5, label=CDC, angle = 90, size = 12, nudge_y = 0.15) +
  geom_text(aes(x = x4_offset, y = round(Obsync0, 2)), hjust = 0,
            vjust = 0.5, label=Obsync0, angle = 90, size = 12, nudge_y = 0.15) +
  geom_text(aes(x = x5_offset, y = round(Obsync50, 2)), hjust = 0,
            vjust = 0.5, label=Obsync50, angle = 90, size = 12, nudge_y = 0.15) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 图例
  scale_fill_manual(name=NULL, 
                    values=c("CODSync" = "#F8CBAD",
                             "CDC" = "#B4C7E7",
                             "Obsync0" = "#C5E0B4",
                             "Obsync50" = "#7030A0"),
                    limits=c("CODSync",
                             "CDC",
                             "Obsync0",
                             "Obsync50"))

# 图例位置
# theme(legend.position = c(0.3, 0.88)) +
# 图例背景设置为空
# theme(legend.background = element_rect(fill = "transparent")) +
# 图例大小
# theme(legend.text = element_text(size = 25)) +
# 图例每行元素个数
# guides(fill=guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
#                          byrow = T))#默认F，表示升序填充，反之则降序


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
