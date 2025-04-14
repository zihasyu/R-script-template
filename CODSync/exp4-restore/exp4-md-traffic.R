rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-md-traffic.pdf"
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
  137.5722198,
  134.2933383,
  131.79803,
  96.15752033,
  88.89336506,
  84.91028497,
  79.42208244,
  73.39182045,
  68.98773685,
  65.29722529,
  60.38777794,
  51.14743898,
  43.48521502,
  38.64457213,
  31.41033184,
  22.88934847,
  17.91448703,
  13.86878491,
  9.325297504
)

COD_Finesse = c (
  
)

COD_noGC = c (
  
)

DSync = c(
  85.29279787,
  80.57560413,
  80.57560413,
  70.71223583,
  70.71223583,
  70.71223583,
  70.71223583,
  70.71223583,
  70.71223583,
  69.25318246,
  62.2347289,
  37.01879259,
  30.47259688,
  30.47259688,
  16.68179035,
  16.68179035,
  8.949365902,
  8.32314562,
  8.32314562
)

Rsyncrypto = c (
  94.59117189,
  94.60074468,
  93.79543338,
  85.40846913,
  85.42043512,
  84.61073629,
  84.62509548,
  85.4539399,
  84.24298145,
  84.66099346,
  69.96535845,
  43.83103221,
  43.43356182,
  38.54565325,
  16.07312019,
  13.29960253,
  13.45595817,
  13.43761031,
  13.5078108
)

ObliviSync = c(
  
)

#x轴数据
xline1 <- numeric(19)
for (i in 1:19){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(4, 140)) +
  # 折线 + 散点
  # geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=3) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=4) +
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
  labs(y = "Traffic (%)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 19),
                     labels = c("1", "10", "19")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("CODSync" = "#CD0000",
                              "Feature Based" = "#CDAD00",
                              "noGC" = "#D8E7CA",
                              "Rsyncrypto" = "#8DB6CD",
                              "ObliviSync" = "#0000CD",
                              "Dsync" = "#00CD00"),
                     limits=c("CODSync",
                              "noGC",
                              "Feature Based",
                              "Rsyncrypto",
                              "ObliviSync",
                              "Dsync"))+
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
