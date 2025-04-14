rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-diff-traffic.pdf"
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
  102.1228952,
  99.89568372,
  99.85368706,
  96.52010991,
  96.46270478,
  96.42907906,
  96.41882752,
  88.31865771,
  88.14514011,
  88.08282048,
  77.70629411,
  76.75278371,
  76.56414856,
  68.75391706,
  64.76136715,
  53.2521779,
  34.413987,
  25.4521723,
  16.81549747
)

COD_Finesse = c (

)

COD_noGC = c (
  
)

DSync = c(
  96.01346941,
  95.62408429,
  95.60777188,
  94.08311251,
  94.04560436,
  94.08720273,
  94.01811571,
  91.98329707,
  92.04338976,
  91.98682969,
  73.76401828,
  73.78504431,
  73.76410486,
  66.35205194,
  66.64169241,
  66.31978732,
  37.57193044,
  35.45285527,
  35.09465768
)

Rsyncrypto = c (
  98.7344239,
  98.63964916,
  98.68930676,
  97.83222312,
  97.78975891,
  97.98826116,
  97.91048111,
  97.18392789,
  97.19810687,
  97.26903641,
  86.06773604,
  86.12046217,
  86.33291136,
  79.79479823,
  80.22949444,
  79.96724912,
  61.20288658,
  61.62957897,
  61.29597813
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(4, 105)) +
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
