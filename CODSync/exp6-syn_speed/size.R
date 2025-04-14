rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp6-size.pdf"
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
  4.407616361,
  16.63649915,
  51.73198692,
  120.7292044,
  188.5333987,
  229.872507,
  225.2864306
)

COD_Finesse = c (
  4.30844317,
  15.6273441,
  42.72773885,
  87.95384182,
  126.394287,
  137.719706,
  130.5362675
)

COD_noGC = c (

)

DSync = c(
  4.014439135,
  14.86325803,
  43.61783795,
  85.57833841,
  123.1148046,
  142.8959289,
  150.1287354
)

Rsyncrypto = c (
  1.891142795,
  5.068115472,
  8.474748639,
  10.86951796,
  11.84936612,
  12.11751628,
  12.16417007
)

ObliviSync = c(
  0.053994934,
  0.21433397,
  0.860783726,
  3.489402684,
  6.931568094,
  8.58959864
)

#x轴数据
xline1 <- numeric(7)
for (i in 1:7){
  xline1[i] = i
}
xline2 <- numeric(6)
for (i in 1:6){
  xline2[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1, 7.2), ylim = c(7, 250)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_line(aes(x=xline2,y=ObliviSync,color="ObliviSync"), size=4) +
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
  labs(y = "Speed (MiB/s)", x = NULL) +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 4, 7),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 100, 200),
                     labels = c(0, 100, 200)) +
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
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 6.6, height = 4)
