rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-restore-linux.pdf"
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

COD = c(46.53702844,
  47.18161764,
        49.59804431,
        54.97053555,
        52.36252172,
        56.34324171,
        54.21236156,
        58.08449734,
        56.55583578,
        55.48304647,
        54.70931181,
        53.06558641,
        56.89578673,
        55.8028992,
        61.33548681,
        59.78483316,
        65.17933924,
        69.64246598,
        86.21311421,
        106.3792583,
        181.0715209
)

CDC = c(424.67023937,
  426.7049956,
        429.2099486,
        429.1280732,
        432.363778,
        433.5531689,
        434.1423841,
        439.6439621,
        436.905327,
        440.2426051,
        443.7178414,
        449.3397545,
        449.7463703,
        448.1872727,
        453.8813258,
        457.2148971,
        462.8813486,
        464.4153273,
        470.5040322,
        474.1934575,
        475.2775572
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
  coord_cartesian(xlim =c(1.55, 20.7), ylim = c(7, 510)) +
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
  scale_y_continuous(breaks = c(0, 250, 500),
                     labels = c(0, 250, 500)) +
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
