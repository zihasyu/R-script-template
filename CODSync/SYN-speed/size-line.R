rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-syn-size.pdf"
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

COD = c(4.457917261,
        8.528930131,
        16.06270882,
        30.33980583,
        50.22097228,
        80.80155139,
        119.3744777,
        166.2648599,
        186.7152126,
        213.7722791,
        226.5230133,
        210.4640074,
        215.424386
)

CDC = c(3.08003154,
        6.227580709,
        12.28139124,
        22.83522104,
        41.7920428,
        71.79781735,
        110.2779003,
        166.2648599,
        217.2968275,
        254.9719531,
        281.9880155,
        257.8628009,
        268.5408581
)

OB = c(0.053994934,
       0.086469765,
       0.21433397,
       0.430202264,
       0.860783726,
       1.382965188,
       3.489402684,
       6.961267508,
       6.931568094,
       9.297093264,
       8.58959864,
       7.67870015
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
  coord_cartesian(xlim =c(1.15, 13.3), ylim = c(12, 300)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=CDC,color="Plain"), size=2.3) + 
  geom_point(aes(x=xline1,y=CDC,color="Plain",shape="Plain"),
             size = 6.8, stroke=2.5) +
  geom_line(aes(x=xline2,y=OB,color="ObliviSync"), size=2.3) + 
  geom_point(aes(x=xline2,y=OB,color="ObliviSync",shape="ObliviSync"),
             size = 6.8, stroke=2.5) +
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
  labs(y = "Speed (MiB/s)", x = NULL) +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 7, 13),
                     labels = c("16KiB", "1MiB", "64MiB")) +
  scale_y_continuous(breaks = c(150, 300),
                     labels = c(150, 300)) +
  # 设置隐藏legend
  # theme(legend.position = "none") +
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
  theme(legend.position = c(0.17, 0.73)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(1, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 7)))

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 8, height = 2.7)
