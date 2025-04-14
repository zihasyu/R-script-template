rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-restore-md.pdf"
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

COD = c(2.49568202,
       2.895892253,
       3.972799628,
       4.326610751,
       4.682353172,
       5.010635511,
       5.627287384,
       6.082449312,
       6.566920267,
       7.390309868,
       8.025341948,
       8.877399473,
       10.18660929,
       12.04296472,
       13.76601318,
       15.90520539,
       19.27252942,
       23.99829369,
       30.06349805,
       44.0281742,
       83.48289106
)

CDC = c(124.37210897,
  125.3775564,
        130.4391708,
        126.2048034,
        131.4069799,
        133.0893404,
        132.4626039,
        133.7220358,
        132.7936684,
        134.3230337,
        134.5876724,
        134.2476137,
        133.4980458,
        139.1705471,
        140.7268982,
        137.0564632,
        144.2939046,
        138.9279489,
        142.445636,
        142.871228,
        145.5234327
)



#x轴数据
xline1 <- numeric(21)
for (i in 1:21){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 20.7), ylim = c(7, 160)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=3) +
  # geom_point(aes(x=xline1,y=COD,color="CODSync",shape="CODSync"),
  #            size = 6, stroke=2) +
  geom_line(aes(x=xline1,y=CDC,color="Plain"), size=3) +
  # geom_point(aes(x=xline1,y=CDC,color="Plain",shape="Plain"),
  #            size = 6, stroke=2) +
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
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 11, 21),
                     labels = c("0", "10", "20")) +
  scale_y_continuous(breaks = c(0, 75, 150),
                     labels = c(0, 75, 150)) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("CODSync" = "#AD0626",
                              "Plain" = "#B79AD1"),
                     limits=c("CODSync",
                              "Plain"))+
  scale_shape_manual(name=NULL,
                     values=c("CODSync" = 24,
                              "Plain" = 23),
                     limits=c("CODSync",
                              "Plain")) +
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
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
