rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-100-html.pdf"
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
  90.30594704,
  167.804891,
  129.1279006,
  140.6444261,
  20.48895734,
  104.996633,
  12.78940371,
  117.5258425,
  139.8886545,
  157.614395,
  54.03792425,
  69.20494974,
  71.53914547,
  97.87083205,
  77.20232814,
  96.51654218,
  26.67968752,
  70.02564476,
  80.7442315,
  73.50782893
)

COD_Finesse = c (
  
)

COD_noGC = c (
  
)

DSync = c(
  14.87674657,
  33.72280377,
  18.07366365,
  19.92614656,
  10.92622672,
  28.60277457,
  10.78199502,
  18.92847018,
  23.75014394,
  16.80559644,
  12.00012417,
  13.48206834,
  15.07330996,
  16.68254515,
  17.77599804,
  17.65293288,
  12.3434826,
  16.42041051,
  17.99309254,
  19.67778384
)

Rsyncrypto = c (
  
)

ObliviSync = c(
)

#x轴数据
xline1 <- numeric(20)
for (i in 1:20){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(5, 170)) +
  # 折线 + 散点
  # geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=3) +
  # geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=3) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=3) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=3) +
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
  labs(y = "Speed (MiB/s)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 20),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 75, 150),
                     labels = c(0, 75, 150)) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("CODSync" = "#DF9E9B",
                              "Feature Based" = "#99BADF",
                              "noGC" = "#D8E7CA",
                              "Rsyncrypto" = "#99CDCE",
                              "ObliviSync" = "#999ACD",
                              "Dsync" = "#FFD0E9"),
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
