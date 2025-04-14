rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp3-html.pdf"
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
  2.299236438,
  2.949918031,
  2.964074234,
  28.34695637,
  4.523062831,
  76.36119018,
  4.658646373,
  4.46979183,
  4.897440074,
  13.01918079,
  13.05397814,
  11.50694072,
  7.497225591,
  10.62403664,
  8.606866282,
  36.49548045,
  13.15542365,
  8.678613036,
  12.83297846
)

COD_Finesse = c (
  10.15418572,
  28.57990406,
  17.26178052,
  89.10039713,
  9.654260607,
  91.25745488,
  24.02090286,
  12.927248,
  18.11883155,
  45.99190561,
  28.89768134,
  23.90063564,
  18.64289756,
  13.69047133,
  15.36273845,
  48.53828952,
  27.65210305,
  21.17505995,
  18.80250277
)

COD_noGC = c (
  
)

DSync = c(
  23.94440416,
  53.61517848,
  47.35825941,
  96.0178992,
  30.59078743,
  97.23377464,
  51.11245356,
  38.78560808,
  58.845996,
  86.63941128,
  75.94883442,
  66.98273369,
  59.64457048,
  55.35880936,
  55.75021663,
  84.16177748,
  60.90045491,
  54.63786567,
  49.28846436
)

Rsyncrypto = c (
  18.19975097,
  45.91264963,
  28.97106201,
  96.56019125,
  14.18599471,
  98.72066203,
  45.22561681,
  26.01610189,
  39.85424614,
  67.96457105,
  50.12807478,
  51.86526055,
  36.45111853,
  37.65255405,
  35.70444598,
  72.63888188,
  46.66289602,
  38.81084782,
  29.73644038
)

ObliviSync = c(
  242.3465582,
  239.500842,
  237.8964086,
  210.7419347,
  209.0226036,
  206.6283787,
  205.9984726,
  205.152606,
  202.7513991,
  191.5933436,
  187.3681728,
  184.6181204,
  182.8235075,
  180.1338838,
  179.2158827,
  172.0725278,
  170.056357,
  167.9179127,
  166.3079845
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 240)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
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
  labs(y = "Traffic (%)", x = "#-th Version") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 9, 19),
                     labels = c("2", "10", "20")) +
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
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
