rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp3-linux.pdf"
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
  10.06819486,
  6.243982401,
  5.887934526,
  25.69556647,
  32.41652868,
  39.73850825,
  5.226617997,
  18.80505903,
  10.3164722,
  16.69690294,
  5.392946763,
  16.69695975,
  32.8923303,
  42.26132601,
  5.013128743,
  14.32090439,
  7.467914285,
  18.5505475,
  32.52780062
)

COD_Finesse = c (
  43.52245011,
  32.56886745,
  30.00975761,
  30.16325124,
  45.34945885,
  54.20151788,
  29.82297448,
  33.18111552,
  33.67715667,
  29.67351759,
  29.55440883,
  29.41686472,
  44.72328587,
  55.05467531,
  30.45224068,
  30.61878312,
  29.91307737,
  29.23593434,
  44.93507136
)

COD_noGC = c (
  
)

DSync = c(
  43.42927129,
  46.13455749,
  42.18976108,
  42.54357156,
  58.61431262,
  66.60662197,
  41.99165616,
  46.03662693,
  46.73413983,
  42.14401557,
  41.75107419,
  42.49859912,
  58.64288391,
  67.42240391,
  43.68360808,
  43.56615847,
  42.79262511,
  41.68205271,
  58.86164781
)

Rsyncrypto = c (
  54.25606427,
  69.63828316,
  67.35622034,
  52.8105339,
  67.56166108,
  75.24440828,
  67.45978032,
  69.3268368,
  70.6024685,
  67.74881647,
  67.46739778,
  67.83613501,
  67.85302901,
  75.84665239,
  69.01110398,
  68.67984459,
  68.18580822,
  67.33697748,
  67.94141705
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(4, 90)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
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
  scale_y_continuous(breaks = c(0, 40, 80),
                     labels = c(0, 40, 80)
                     ) +
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
