rm(list = ls())

exp = "LongTerm"
exportPath = "C:/Users/杨劲远/Desktop/"
exportName = "exp-longterm-500m.pdf"

root = "D:/RProject/CODSync"
setwd(sprintf("%s/%s", root, exp))

library(ggplot2)
library(ggbreak)
library(readxl)
library(extrafont)
library(showtext)  
library(gg.gap)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

CODSync = c(99.6, 
            3256.8, 968)
CDC = c(42.2,
        7061.7, 1596.2)
Obsync0 = c(3826.7, 
            31146.3, 7643.3)
Obsync50 = c(4309.2, 
             65016.4, 15312.7)

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
p1 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.6, 4.3), ylim = c(4.5, 100)) +
  
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 80),
                     labels = c(0, 80)) +
  
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
  
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 42, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 45)) +
  theme(axis.title.y = element_text(size = 41)) +
  
  # 设置label内容
  labs(y = "Time Duration (s)", x = "Threshold") +
  
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MD", "Diffuser", "SYN-4m")) +
  theme(axis.title.y = element_text(hjust = 0.32)) +
  
  # 设置文本，hjust和vjust设置对齐基准点
  # geom_text(aes(x = x1_offset, y = round(CODSync, 1)), hjust = 0,
  #           vjust = 0.4, label=CODSync, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x2_offset, y = round(CDC, 1)), hjust = 0,
  #           vjust = 0.4, label=CDC, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x4_offset, y = round(Obsync0, 1)), hjust = 0,
  #           vjust = 0.4, label=Obsync0, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x5_offset, y = round(Obsync50, 1)), hjust = 0,
  #           vjust = 0.4, label=Obsync50, angle = 90, size = 13, nudge_y = 0.1) +
  
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




p2 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.6, 4.3), ylim = c(200, 9000)) +
  
  # 设置y坐标标签
  scale_y_continuous(breaks = c(300, 9000),
                     labels = c(300, 9000)) +
  
  # 柱状图
  # geom_col(aes(x = x1_offset,y = CODSync, fill = "CODSync"), width = 0.4, 
  #          color = "black", size = 0.5) +
  # geom_col(aes(x = x2_offset,y = CDC, fill = "CDC"), width = 0.4, 
  #          color = "black", size = 0.5) +
  geom_col(aes(x = x4_offset,y = Obsync0, fill = "Obsync0"), width = 0.4, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x5_offset,y = Obsync50, fill = "Obsync50"), width = 0.4,
           color = "black", size = 0.5) +
  
  # 白底，没有上边框和右边框
  theme_classic() +
  
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 42, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 45)) +
  theme(axis.title.y = element_text(size = 41)) +
  
  # 设置label内容
  labs(y = NULL, x = NULL) +
  
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MD", "Diffuser", "SYN-4m")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  
  # 设置文本，hjust和vjust设置对齐基准点
  # geom_text(aes(x = x1_offset, y = round(CODSync, 1)), hjust = 0,
  #          vjust = 0.4, label=CODSync, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x2_offset, y = round(CDC, 1)), hjust = 0,
  #           vjust = 0.4, label=CDC, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x4_offset, y = round(Obsync0, 1)), hjust = 0,
  #          vjust = 0.4, label=Obsync0, angle = 90, size = 13, nudge_y = 0.1) +
  # geom_text(aes(x = x5_offset, y = round(Obsync50, 1)), hjust = 0,
  #           vjust = 0.4, label=Obsync50, angle = 90, size = 13, nudge_y = 0.1) +
  
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

p2/p1





# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
