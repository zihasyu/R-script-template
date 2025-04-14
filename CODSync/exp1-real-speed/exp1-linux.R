rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-linux.pdf"
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
  193.3465557,
  287.805327,
  312.9876285,
  317.2078458,
  107.132477,
  107.9172689,
  347.4643296,
  313.6428708,
  318.0925819,
  360.8899088,
  356.1187457,
  352.5830807,
  114.7136173,
  119.7058783,
  330.7184147,
  334.6314179,
  334.8418897,
  350.6741405,
  104.6261471
)

COD_Finesse = c (
  129.9999843,
  65.77666617,
  66.30556695,
  62.6807833,
  125.4285946,
  103.1456798,
  66.57416051,
  65.55065541,
  65.49386911,
  67.21568854,
  66.72979577,
  66.30314519,
  129.1301696,
  95.11881684,
  66.81094482,
  66.11323691,
  66.2828927,
  67.25104955,
  127.0737243
)

COD_noGC = c (
  257.6340806,
  271.13602,
  282.5949742,
  282.515198,
  268.5032138,
  235.1044492,
  282.6141411,
  274.7394759,
  268.9681392,
  259.2744487,
  276.4323515,
  280.230296,
  265.6495753,
  229.9200782,
  278.5012425,
  278.499468,
  260.7974183,
  281.5028135,
  243.7683002
)

DSync = c(
  158.287221,
  157.5546416,
  158.6883802,
  158.6537898,
  153.6814906,
  155.190855,
  158.4220527,
  155.7522174,
  156.3920537,
  158.5141582,
  156.8872143,
  158.1569466,
  155.3482278,
  153.9807505,
  157.652201,
  158.1131572,
  158.3174895,
  158.2610574,
  150.9201545
)

Rsyncrypto = c (
  17.20333504,
  14.75765886,
  15.09642091,
  17.43711525,
  15.10140544,
  14.11283613,
  14.93865841,
  14.78606318,
  14.58465452,
  14.93415254,
  14.87182979,
  14.86021248,
  14.79949658,
  13.96884703,
  14.7336725,
  14.73467438,
  14.8067005,
  14.90087139,
  14.84077205
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 355)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
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
