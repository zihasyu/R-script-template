rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-linux.pdf"
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
  49.58590291,
  51.01259168,
  49.37853182,
  50.63125138,
  50.34726705,
  49.44090851,
  50.33254698,
  55.65469291,
  56.25467526,
  57.69972695,
  56.86147065,
  58.19127823,
  60.4622718,
  59.2578506,
  70.62894867,
  70.51712158,
  96.75776107,
  124.9787497,
  275.2807008
)

COD_Finesse = c (
  
)

COD_noGC = c (
)

DSync = c(
  145.4267813,
  143.7980895,
  142.9955298,
  142.9619542,
  143.1787414,
  145.1137713,
  143.8422239,
  145.1290656,
  144.3534206,
  142.8688096,
  143.0889163,
  143.5658243,
  144.6279702,
  144.5732975,
  144.2860955,
  144.0339225,
  144.3101218,
  145.0820894,
  145.7082144
)

Rsyncrypto = c (
  13.80074485,
  13.87874924,
  13.92982734,
  13.84372159,
  13.8555834,
  13.93365834,
  14.1080048,
  14.12775578,
  14.26240121,
  14.33961331,
  14.52417451,
  14.40214477,
  14.59154541,
  14.59081817,
  15.19149585,
  15.47192662,
  15.63544909,
  15.73339067,
  15.82477525
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
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(7, 330)) +
  # 折线 + 散点
  # geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  # geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
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
  labs(y = "Speed (MiB/s)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 19),
                     labels = c("1", "10", "19")) +
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
