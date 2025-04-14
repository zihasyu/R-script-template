rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-100-diff.pdf"
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
  36.47284837,
  45.21530705,
  121.1837178,
  28.63485437,
  89.1241889,
  105.4898311,
  116.3746397,
  24.49887285,
  90.41891743,
  102.0428746,
  31.51639024,
  45.8912109,
  97.39058364,
  46.20216903,
  73.67229007,
  51.90758532,
  36.11984799,
  84.52098406,
  92.45634224,
  57.19642651
)

COD_Finesse = c (
  
)

COD_noGC = c (
  
)

DSync = c(
  14.1027838,
  15.50302992,
  23.79742074,
  15.35847257,
  25.68077098,
  25.27458258,
  25.23723014,
  13.36485582,
  24.77304212,
  24.68578018,
  12.31599741,
  24.78983416,
  25.51794592,
  19.4625861,
  26.30773292,
  26.73431369,
  15.90173818,
  26.46919359,
  28.32756909,
  27.61713941
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
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(5, 120)) +
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
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)) +
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
