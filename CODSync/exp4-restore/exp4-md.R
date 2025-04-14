rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-md.pdf"
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
  4.045593003,
  4.338376371,
  4.752533626,
  5.246351296,
  5.786073144,
  6.403715465,
  7.003587227,
  7.744356496,
  8.772786852,
  9.790516629,
  11.03071047,
  12.6296082,
  13.87762218,
  16.65861926,
  19.21794063,
  23.02344411,
  30.61097003,
  43.6753187,
  73.9960896
)

COD_Finesse = c (

)

COD_noGC = c (
)

DSync = c(
  68.10692323,
  67.04884035,
  66.56166719,
  63.40727822,
  63.68934357,
  63.41064146,
  62.17233916,
  61.70382841,
  62.20630781,
  61.8186945,
  61.7819547,
  63.96366097,
  64.32330707,
  63.18942986,
  64.51770547,
  64.49508111,
  65.31076093,
  64.90652461,
  65.19500592
)

Rsyncrypto = c (
  10.29796814,
  11.5977146,
  11.52231803,
  10.73219642,
  12.09511153,
  12.14617116,
  12.01602991,
  11.83820195,
  12.05832899,
  11.80261353,
  13.08474105,
  16.53123179,
  16.6004419,
  17.3513331,
  22.48919868,
  23.58113131,
  23.4054628,
  23.3417126,
  23.3162187
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(3, 85)) +
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
  scale_y_continuous(breaks = c(0, 40, 80),
                     labels = c(0, 40, 80)) +
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
