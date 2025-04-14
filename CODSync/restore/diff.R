rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-restore-diff.pdf"
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

COD = c(27.17604375,
  27.39408406,
        27.6310847,
        29.07670526,
        28.34923522,
        30.41112846,
        33.56374441,
        28.61326795,
        31.12649938,
        33.0222105,
        32.52449416,
        29.58010293,
        38.81690489,
        42.81216847,
        38.70495488,
        37.82801714,
        43.05120191,
        55.98403324,
        62.17680389,
        90.72699385,
        149.4115052
)

CDC = c(238.57643082,
  240.6639449,
        244.4169756,
        241.1983131,
        245.2882198,
        246.7565289,
        246.2990902,
        245.5776887,
        245.4555402,
        250.7909104,
        252.3002425,
        250.6526093,
        272.9796751,
        270.8946188,
        262.9274939,
        277.9972638,
        281.6924081,
        281.6970023,
        305.909743,
        302.1054012,
        307.6917733
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
  coord_cartesian(xlim =c(1.55, 20.7), ylim = c(7, 340)) +
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
  scale_y_continuous(breaks = c(0, 150, 300),
                     labels = c(0, 150, 300)) +
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
