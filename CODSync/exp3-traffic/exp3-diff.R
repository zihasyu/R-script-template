rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp3-diff.pdf"
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
  17.10975149,
  5.382903783,
  16.16737719,
  18.47548911,
  7.308389208,
  5.905408233,
  21.18348247,
  19.30382072,
  7.892216137,
  27.20486812,
  9.656978293,
  20.60713952,
  19.32719147,
  12.08864223,
  8.420075315,
  35.1879642,
  8.744896846,
  8.972354485,
  7.773332907
)

COD_Finesse = c (
  52.24145597,
  29.10600046,
  47.2325661,
  28.66844404,
  28.52884888,
  28.97484768,
  56.24572524,
  28.92292759,
  29.59913007,
  67.55065971,
  30.52227501,
  28.57512465,
  40.52132091,
  28.26455477,
  26.37534469,
  48.67833002,
  28.05263344,
  25.76951817,
  26.44886036
)

COD_noGC = c (
  16.64871114,
  4.917768305,
  33.97362426,
  8.70592159,
  6.727988625,
  5.279119674,
  40.02823123,
  8.12983922,
  6.983234779,
  26.97173321,
  21.2183424,
  7.522865585,
  19.25492511,
  11.11551341,
  18.73104053,
  24.57576276,
  8.926222976,
  8.606587882,
  16.74774314
)

DSync = c(
  67.46674471,
  41.41168177,
  68.31788759,
  38.05571982,
  38.75303685,
  38.81737071,
  79.15345022,
  39.80771365,
  39.94077842,
  86.71199128,
  39.83836277,
  38.47244593,
  52.60746632,
  37.09347073,
  36.37186455,
  65.87556412,
  36.81236811,
  34.10169676,
  35.09465768
)

Rsyncrypto = c (
  83.35396847,
  69.57729125,
  82.43837512,
  67.51301594,
  66.95504821,
  66.84893063,
  89.58778465,
  67.53536699,
  68.46995338,
  95.50414762,
  66.26627943,
  66.77418729,
  74.22377712,
  62.85181533,
  62.45781071,
  79.81326632,
  61.658477,
  61.67226441,
  61.29597813
)

ObliviSync = c(
  154.3829421,
  154.3829421,
  148.6658323,
  148.6621111,
  143.5322387,
  143.5214616,
  142.5328182,
  137.6211961,
  142.4746917,
  142.9917961,
  142.1111978,
  142.1081163,
  146.5386679,
  145.9324651,
  145.9353536,
  146.1265407,
  145.8845318,
  145.8845318,
  148.8952725
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
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 160)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
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
  scale_y_continuous(breaks = c(0, 75, 150),
                     labels = c(0, 75, 150)) +
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
