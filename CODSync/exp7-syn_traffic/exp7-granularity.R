rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp7-granularity.pdf"
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
  103.41362,
  101.2035704,
  42.93517113,
  20.74541569,
  18.15239429
)

COD_Finesse = c (
  106.0768318,
  105.9715843,
  89.98399258,
  50.45956612,
  24.34767246
)

DSync = c(
  100.5210876,
  100.5125046,
  94.85626221,
  56.19928837,
  26.6721487
)

Rsyncrypto = c (
  100.6408691,
  100.6408691,
  100.6397247,
  89.76767063,
  49.72093105
)

ObliviSync = c(
  200,
  200,
  200,
  200,
  200
)

#x轴数据
xline1 <- numeric(5)
for (i in 1:5){
  xline1[i] = i
}
xline2 <- numeric(4)
for (i in 1:4){
  xline2[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(0.6, 5.7), ylim = c(0, 110)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_point(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto",shape="Rsyncrypto"),
             size = 10, stroke=4) +
  geom_line(aes(x=xline1,y=DSync,color="Plain"), size=4) +
  geom_point(aes(x=xline1,y=DSync,color="Plain",shape="Plain"),
             size = 10, stroke=4) +
  geom_line(aes(x=xline1,y=COD,color="SyncGuard"), size=4) +
  geom_point(aes(x=xline1,y=COD,color="SyncGuard",shape="SyncGuard"),
             size = 10, stroke=4) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Traffic (%)", x = "b") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("8B", "32B", "128B", "512B", "2K")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("SyncGuard" = "#CD0000",
                              "Rsyncrypto" = "#8DB6CD",
                              "Plain" = "#00CD00"),
                     limits=c("SyncGuard",
                              "Rsyncrypto",
                              "Plain"))+
  scale_shape_manual(name=NULL, 
                     values=c("SyncGuard" = 24,
                              "Rsyncrypto" = 22,
                              "Plain" = 20),
                     limits=c("SyncGuard",
                              "Rsyncrypto",
                              "Plain")) +
  # 图例位置
  theme(legend.position = c(0.1, 0.73)) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5)))

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
