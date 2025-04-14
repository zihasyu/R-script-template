rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp3-md.pdf"
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
  9.688021898,
  7.301055951,
  50.41340826,
  10.38033721,
  5.827016386,
  7.537408839,
  8.094951953,
  5.84947958,
  4.876188735,
  6.206502275,
  11.05844051,
  9.089345368,
  5.706425216,
  8.203711151,
  9.400688883,
  5.556412383,
  4.52365719,
  4.949278399,
  9.662539064
)

COD_Finesse = c (
  15.3838766,
  13.04045166,
  52.94117647,
  15.04185862,
  10.98934901,
  15.3939121,
  10.56166272,
  11.97666396,
  11.53839677,
  16.26891693,
  27.14909141,
  13.81689134,
  10.93321508,
  16.85434315,
  11.42859562,
  10.99527726,
  9.973482222,
  10.2744898,
  11.67801109
)

COD_noGC = c (
  
)

DSync = c(
  21.2739506,
  8.410750938,
  50.12305651,
  12.28624591,
  11.80863527,
  16.79528786,
  16.76201922,
  19.86952049,
  20.7052178,
  24.25667024,
  49.79565448,
  12.8248943,
  7.787566104,
  24.57286302,
  11.52174511,
  12.90834212,
  6.064321554,
  6.934010296,
  8.32314562
)

Rsyncrypto = c (
  19.21437223,
  22.46978794,
  54.25153926,
  14.44060926,
  8.429580968,
  28.60229914,
  13.36088711,
  23.94473404,
  17.38626276,
  38.76299912,
  61.90722483,
  10.4425624,
  13.03862455,
  33.3720362,
  10.90674678,
  9.393588242,
  10.72375635,
  12.13087408,
  13.5078108
)

ObliviSync = c(
  2237.9528,
  2146.862604,
  1174.371768,
  1142.647295,
  1121.885616,
  1085.465547,
  1058.969788,
  1029.865911,
  1009.039317,
  990.8631744,
  968.4601353,
  949.2897817,
  925.3617673,
  906.8804622,
  887.9483613,
  874.1666424,
  862.8674996,
  849.0992386,
  836.4835139
)

#x轴数据
xline1 <- numeric(19)
for (i in 1:19){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
p1 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(1.2, 62)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 50),
                     labels = c(0, 50)) +
  # 折线图
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=3) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=4) +
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
  # 设置x坐标标签
  scale_x_continuous(breaks = c(1, 9, 19),
                     labels = c(2, 10, 20)) +
  # 设置label内容
  labs(y = "Traffic (%)", x = "#-th Version") +
  theme(axis.title.y = element_text(hjust = 0.2)) +
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
  theme(legend.position = "none")

p2 <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(800, 2400)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(1000, 2000),
                     labels = c(1000, 2000)) +
  # 折线图
  geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
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
  theme(axis.text.x = element_blank()) +
  theme(axis.line.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank()) +
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
  theme(legend.position = "none")

p2/p1

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
