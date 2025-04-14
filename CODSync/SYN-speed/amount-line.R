rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-syn-amount.pdf"
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

COD = c(367.0061474,
        274.9329851,
        193.7702853,
        166.583375,
        150.2178158,
        138.9660923,
        140.8847563,
        148.5994502,
        144.5191127,
        156.9427551,
        153.7515375,
        171.2402072,
        182.0498817
)

CDC = c(288.5378345,
        251.8415916,
        214.2589319,
        195.9151687,
        182.7401891,
        171.3429,
        165.337081,
        163.345312,
        163.1587535,
        154.9246679,
        152.8584531,
        149.5271205,
        153.4154106
)

OB = c(6.954368908,
       6.966771981,
       6.981777561,
       6.11309532,
       6.967548642,
       6.913119372,
       6.194481955,
       6.847298398,
       6.215368742,
       6.906649722,
       6.165342146,
       6.930991582,
       6.937170051
)

#x轴数据
xline1 <- numeric(13)
for (i in 1:13){
  xline1[i] = i
}

xline2 <- numeric(13)
for (i in 1:13){
  xline2[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.15, 13.3), ylim = c(12, 410)) +
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
                     labels = c("1%", "15%", "30%")) +
  scale_y_continuous(breaks = c(200, 400),
                     labels = c(200, 400)) +
  
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
  # 设置隐藏legend
  theme(legend.position = "none") +
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
