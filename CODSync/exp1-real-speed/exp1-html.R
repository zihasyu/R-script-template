rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-html.pdf"
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
  266.9543982,
  191.6100427,
  213.1368385,
  51.24513467,
  246.6289021,
  114.6595945,
  184.2957458,
  235.4221594,
  229.8983689,
  117.6001063,
  179.3604127,
  216.6464674,
  230.0036688,
  240.7993012,
  255.7377133,
  155.6338497,
  193.9592135,
  214.3004198,
  232.3109341
)

COD_Finesse = c (
  218.961118,
  143.4581916,
  162.6273444,
  64.5904863,
  185.8652518,
  61.57847486,
  115.7740264,
  156.582723,
  132.6470371,
  87.8906796,
  114.4008515,
  128.0289877,
  155.7447644,
  148.6425172,
  144.3970394,
  82.01965893,
  112.7756344,
  122.3354724,
  151.1738632
)

COD_noGC = c (
  
)

DSync = c(
  101.5585102,
  101.1654411,
  101.2345183,
  107.2590433,
  106.914835,
  104.6854053,
  105.0168724,
  106.6554382,
  104.5112774,
  106.6814105,
  108.6263822,
  107.3760809,
  108.5322908,
  108.4421822,
  106.8478643,
  111.7328129,
  111.7310188,
  111.1269042,
  112.4755033
)

Rsyncrypto = c (
  23.25251903,
  17.36964775,
  21.23462235,
  11.55247842,
  26.30298802,
  11.00892566,
  17.18300435,
  21.65446542,
  17.76857757,
  13.38124485,
  15.95314783,
  15.91461335,
  18.96192237,
  17.99064212,
  19.09613703,
  13.04528063,
  16.54168477,
  18.43554929,
  19.99614375
)

ObliviSync = c(
  5.756045949,
  5.755988194,
  4.566724736,
  6.562649428,
  6.552874253,
  5.419704339,
  5.401840998,
  4.551417448,
  5.46657444,
  5.786092359,
  7.42579469,
  7.498380532,
  6.109752724,
  6.206976202,
  7.736163426,
  6.45876841,
  6.555231863,
  8.222047672,
  6.688550937
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 310)) +
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
