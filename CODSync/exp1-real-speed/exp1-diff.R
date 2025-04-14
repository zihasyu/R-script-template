rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-diff.pdf"
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
  153.7309709,
  312.9016571,
  150.9174573,
  362.4279916,
  322.2248146,
  303.2451596,
  136.4792587,
  351.4423949,
  320.3565196,
  135.0274307,
  313.0528455,
  340.5842218,
  218.453952,
  280.5265677,
  313.3616655,
  206.7268956,
  261.1203782,
  359.489004,
  343.7848211
)

COD_Finesse = c (
  126.0387132,
  74.06884156,
  68.09880969,
  72.7505192,
  76.60185931,
  77.74571842,
  58.14975266,
  71.11619424,
  71.78651553,
  47.54055112,
  90.77399249,
  86.55368521,
  74.09755007,
  82.48579864,
  78.53370294,
  59.30835747,
  83.62391978,
  77.86619481,
  77.77294345
)

COD_noGC = c (
  151.9568508,
  308.1666839,
  197.1060416,
  330.0955093,
  334.5121594,
  304.0571129,
  178.17247,
  320.4905742,
  316.3563203,
  132.7811369,
  343.6654384,
  319.8616239,
  219.5122988,
  271.3174838,
  344.4814864,
  166.7741094,
  265.6236225,
  355.4236641,
  360.5451728
)

DSync = c(
  148.6031623,
  140.7791506,
  151.2017172,
  145.7815758,
  147.7562355,
  148.8610097,
  144.9447996,
  151.138818,
  151.0481406,
  144.6497421,
  151.6519048,
  151.2203185,
  153.1764248,
  151.1364533,
  151.1510565,
  152.5429938,
  150.4237775,
  149.3333405,
  152.3597657
)

Rsyncrypto = c (
  12.54012928,
  14.08179318,
  12.65338043,
  14.09274909,
  14.1324482,
  14.22833224,
  11.62127642,
  13.91567952,
  13.79190981,
  11.11490877,
  13.92152247,
  15.73375145,
  14.9064757,
  16.13032323,
  16.16522525,
  13.99906001,
  15.98672393,
  15.8754215,
  16.06909679
)

ObliviSync = c(
  6.654987906,
  7.425166349,
  7.360631305,
  6.798452841,
  7.175765081,
  6.035192055,
  5.810594398,
  6.444289035,
  6.035166489,
  6.201630529,
  6.279358356,
  6.288346684,
  5.918952035,
  5.952031694,
  5.945652095,
  5.795143707,
  5.640017549,
  5.652050609,
  5.73983929
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 350)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
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
  labs(y = "Speed (MiB/s)", x = "#-th Version") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 9, 19),
                     labels = c("2", "10", "20")) +
  scale_y_continuous(breaks = c(0, 150, 300),
                     labels = c(0, 150, 300)) +
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
