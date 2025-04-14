rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp7-amount.pdf"
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
  6.417908669,
  23.39384079,
  43.69512081,
  64.33576107,
  82.23686218
)

COD_Finesse = c (
  15.70827961,
  51.743927,
  74.67944622,
  88.96029472,
  96.60758972
)

DSync = c(
  13.99896145,
  58.61539841,
  84.83128548,
  94.37720776,
  97.61753082
)

Rsyncrypto = c (
  35.07406712,
  88.44459057,
  98.93701077,
  100.207901,
  100.6409645
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
  labs(y = "Traffic (%)", x = "p") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5),
                     labels = c("1%", "5%", "10%", "15%", "20%")) +
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
