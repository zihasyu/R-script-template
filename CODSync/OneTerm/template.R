rm(list = ls())

exp = "OneTerm"
exportPath = "C:/Users/杨劲远/Desktop/"
exportName = "exp5.pdf"

root = "D:/RProject/CODSync"
setwd(sprintf("%s/%s", root, exp))

library(ggplot2)
library(readxl)
library(extrafont)
library(showtext)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

ShieldReduce = c(29.82, 100.62, 
  15.83, 178.38)
DEBE = c(5.77, 13.36, 
  8.64, 161.92)
ForwardDelta = c(29.90, 97.62, 
  15.82, 177.46)
SecureMeGA = c(12.24, 16.24, 
  14.59, 167.39)

barWidth = 0.4
x3_offset = c(0, 1.8, 3.6, 5.4)
x1_offset = numeric(4)
x2_offset = numeric(4)
x4_offset = numeric(4)
x5_offset = numeric(4)
for(i in 1:4) {
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
  coord_cartesian(xlim =c(-0.7, 6), ylim = c(12.2, 260)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = ShieldReduce, fill = "ShieldReduce"), width = 0.4, 
    color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = DEBE, fill = "DEBE"), width = 0.4, 
    color = "black", size = 0.5) +
  geom_col(aes(x = x4_offset,y = ForwardDelta, fill = "ForwardDelta"), width = 0.4, 
    color = "black", size = 0.5) +
  geom_col(aes(x = x5_offset,y = SecureMeGA, fill = "SecureMeGA"), width = 0.4,
    color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 22, color = "black")) +
  theme(axis.text.y = element_text(size = 27, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 32)) +
  theme(axis.title.y = element_text(size = 30)) +
  # 设置label内容
  labs(y = "Reduction Ratio", x = "Snapshot") +
  # 隐藏x轴label
  theme(axis.title.x = element_blank()) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("ShieldReduce", "DEBE", "ForwardDelta", "SecureMeGA")) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250),
                     labels = c(0, 50, 100, 150, 200, 250)) +
  # 设置文本，hjust和vjust设置对齐基准点
  geom_text(aes(x = x1_offset, y = round(ShieldReduce, 2)), hjust = 0,
            vjust = 0.4, label=ShieldReduce, angle = 90, size = 9.5, nudge_y = 3) +
  geom_text(aes(x = x2_offset, y = round(DEBE, 2)), hjust = 0,
            vjust = 0.4, label=DEBE, angle = 90, size = 9.5, nudge_y = 3) +
  geom_text(aes(x = x4_offset, y = round(ForwardDelta, 2)), hjust = 0,
            vjust = 0.4, label=ForwardDelta, angle = 90, size = 9.5, nudge_y = 3) +
  geom_text(aes(x = x5_offset, y = round(SecureMeGA, 2)), hjust = 0,
            vjust = 0.4, label=SecureMeGA, angle = 90, size = 9.5, nudge_y = 3) +
  # 图例
  scale_fill_manual(name=NULL, 
                    values=c("ShieldReduce" = "#AD0626",
                             "DEBE" = "#B79AD1",
                             "SecureMeGA" = "#F2BE5C",
                             "ForwardDelta" = "#75B8BF"),
                    limits=c("ShieldReduce",
                             "DEBE",
                             "SecureMeGA",
                             "ForwardDelta")) +
  # 图例位置
  theme(legend.position = c(0.3, 0.88)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 25)) +
  # 图例每行元素个数
  guides(fill=guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = T))#默认F，表示升序填充，反之则降序
  

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
