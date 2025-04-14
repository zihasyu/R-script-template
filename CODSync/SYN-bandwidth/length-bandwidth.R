rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-syn-length-bandwidth.pdf"
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

COD = c(4347886,
        4339552,
        4336701,
        4337390,
        4340061,
        4244587,
        2502984,
        1800966,
        1100806,
        870068,
        824267,
        761322,
        636662
)

CDC = c(4278064,
        4279264,
        4277584,
        4277744,
        4279024,
        4270809,
        4076060,
        3597956,
        2833189,
        1933587,
        1233784,
        837651,
        596731
)

OB = c(4194304,
       4194304,
       4194304,
       4194304,
       4194304,
       4194304,
       4194304,
       4194304,
       8388608,
       12582912,
       25165824,
       46137344
)

#x轴数据
xline1 <- numeric(13)
for (i in 1:13){
  xline1[i] = i
}

xline2 <- numeric(12)
for (i in 1:12){
  xline2[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.15, 13.3), ylim = c(12, 4.35*1024*1024)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=CDC,color="Plain"), size=2.3) + 
  geom_point(aes(x=xline1,y=CDC,color="Plain",shape="Plain"),
             size = 6.8, stroke=2.5) +
  # geom_line(aes(x=xline2,y=OB,color="ObliviSync"), size=2.3) + 
  # geom_point(aes(x=xline2,y=OB,color="ObliviSync",shape="ObliviSync"),
  #            size = 6.8, stroke=2.5) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=2.3) +
  geom_point(aes(x=xline1,y=COD,color="CODSync",shape="CODSync"),
             size = 6.8, stroke=2.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text = element_text(size = 25, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 25)) +
  theme(axis.title.y = element_text(size = 25)) +
  # 设置label内容
  labs(y = "Traffic (MiB)", x = NULL) +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 7, 13),
                     labels = c("1Byte", "64Bytes", "4KiB")) +
  scale_y_continuous(breaks = c(2*1024*1024, 4*1024*1024),
                     labels = c("2", "4")) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("CODSync" = "#AD0626",
                              "Plain" = "#B79AD1",
                              "ObliviSync" = "#F2BE5C"),
                     limits=c("CODSync",
                              "Plain",
                              "ObliviSync"))+
  scale_shape_manual(name=NULL, 
                     values=c("CODSync" = 24,
                              "Plain" = 23,
                              "ObliviSync" = 22),
                     limits=c("CODSync",
                              "Plain",
                              "ObliviSync")) +
  # 图例位置
  # theme(legend.position = c(0.17, 0.73)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(1, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 7)))

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 8, height = 2.7)
