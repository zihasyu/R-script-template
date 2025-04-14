rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-100mbps.pdf"
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

COD = c(
  59.57960145,
  90.48992924,
  69.86191467,
  62.18909518
)

COD_Finesse = c (
  45.40740755,
  40.32289743,
  36.96209426,
  33.44839537
)

DSync = c(
  35.12387967,
  17.77481577,
  22.06110097,
  21.24810943
)

Rsyncrypto = c (
  13.73616761,
  11.11084612,
  7.458526628,
  7.887034152
)

ObliviSync = c(
  0.546134835,
  3.122671898,
  3.593956938
)

barWidth = 0.17
x3_offset = c(0, 1, 2, 3)
x1_offset = numeric(4)
x2_offset = numeric(4)
x4_offset = numeric(4)
x5_offset = numeric(3)
for(i in 1:4) {
  x1_offset[i] = x3_offset[i] - 2 * barWidth
  x2_offset[i] = x3_offset[i] - 1 * barWidth
  x4_offset[i] = x3_offset[i] + 1 * barWidth
}
for(i in 1:3) {
  x5_offset[i] = x3_offset[i] + 2 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.3, 3.15), ylim = c(6.9, 145)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = COD, fill = "CODSync"), width = barWidth,
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = DSync, fill = "Dsync"), width = barWidth,
           color = "black", size = 0.5) +
  geom_col(aes(x = x3_offset,y = COD_Finesse, fill = "Base"), width = barWidth,
           color = "black", size = 0.5) +
  geom_col(aes(x = x4_offset,y = Rsyncrypto, fill = "Rsyncrypto"), width = barWidth,
           color = "black", size = 0.5) +
  geom_col(aes(x = x5_offset,y = ObliviSync, fill = "ObliviSync"), width = barWidth,
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 21, color = "black")) +
  theme(axis.text.y = element_text(size = 21, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 21)) +
  theme(axis.title.y = element_text(size = 21)) +
  # 设置label内容
  labs(y = "Speed (MiB/s)", x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MD", "HTML", "Diffusers", "Linux")) +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 标签
  geom_text(aes(x = x1_offset, y = COD), hjust = 0,
            vjust = 0.5, label=round(COD,1), angle = 90, size = 7, nudge_y = 2) +
  geom_text(aes(x = x2_offset, y = DSync), hjust = 0,
            vjust = 0.5, label=round(DSync,1), angle = 90, size = 7, nudge_y = 2) +
  geom_text(aes(x = x3_offset, y = COD_Finesse), hjust = 0,
            vjust = 0.5, label=round(COD_Finesse,1), angle = 90, size = 7, nudge_y = 2) +
  geom_text(aes(x = x4_offset, y = Rsyncrypto), hjust = 0,
            vjust = 0.5, label=round(Rsyncrypto,1), angle = 90, size = 7, nudge_y = 2) +
  geom_text(aes(x = x5_offset, y = ObliviSync), hjust = 0,
            vjust = 0.5, label=round(ObliviSync,1), angle = 90, size = 7, nudge_y = 2) +
  # 图例
  scale_fill_manual(name=NULL, 
                     values=c("CODSync" = "#CD0000",
                              "Base" = "#CDAD00",
                              "Rsyncrypto" = "#8DB6CD",
                              "ObliviSync" = "#0000CD",
                              "Dsync" = "#00CD00"),
                     limits=c("CODSync",
                              "Dsync",
                              "Base",
                              "Rsyncrypto",
                              "ObliviSync"
                              ))+
  # 图例位置
  theme(legend.position = c(0.43, 0.92)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  # 图例每行元素个数
  guides(fill=guide_legend(ncol = 5, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                           byrow = T))#默认F，表示升序填充，反之则降序


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 3)