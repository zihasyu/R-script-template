rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-md.pdf"
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
  43.20395924,
  45.02619563,
  72.3926978,
  75.57518948,
  74.56865924,
  80.53004325,
  93.91486852,
  88.55450931,
  86.12136957,
  84.64844428,
  89.59366805,
  85.38351474,
  94.3188685,
  95.30519965,
  110.1948744,
  96.12997039,
  100.6012575,
  97.63470585,
  113.0211131
)

COD_Finesse = c (
  44.38410215,
  47.13341703,
  53.49578186,
  74.51346906,
  80.75709791,
  71.39861088,
  70.94770871,
  69.96938891,
  79.49000684,
  73.5048126,
  74.6982929,
  89.53838616,
  93.70547894,
  84.33507916,
  83.84080522,
  96.69878678,
  97.32743959,
  100.5952286,
  99.54045158
)

COD_noGC = c (
  
)

DSync = c(
  36.37256397,
  38.67131538,
  58.75584667,
  57.90014516,
  58.46877456,
  60.3217348,
  59.97548447,
  60.54560839,
  60.72559193,
  59.06195037,
  60.56999702,
  63.6507017,
  65.01178739,
  64.55978688,
  67.21525611,
  66.52786552,
  67.96226283,
  68.48196769,
  68.64661634
)

Rsyncrypto = c (
  12.10267869,
  12.74616883,
  12.91452351,
  17.79631926,
  20.17396694,
  16.64147427,
  18.94101961,
  16.85120604,
  18.69889925,
  14.85535871,
  12.51695011,
  20.58826395,
  19.61089621,
  15.80157288,
  20.28023043,
  20.82212359,
  20.67278185,
  20.84107344,
  20.64252417
)

ObliviSync = c(
  0.602437524,
  0.613701004,
  1.139251252,
  1.175035673,
  1.194627623,
  1.230862553,
  1.267765304,
  1.305441167,
  1.336730086,
  1.351077448,
  1.386358878,
  1.413245567,
  1.436009841,
  1.47806544,
  0.869751816,
  1.200883538,
  1.555167653,
  1.266248396,
  1.254992571
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(4, 110)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=3) +
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
