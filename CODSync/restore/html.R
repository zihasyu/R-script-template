rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-restore-html.pdf"
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

COD = c(14.80583127,
  15.16873632,
        15.94010127,
        17.13413548,
        17.99994013,
        19.2609291,
        19.94039032,
        21.75364495,
        24.61690412,
        26.23583311,
        28.41949168,
        30.07439918,
        33.34863149,
        35.48233385,
        42.44767216,
        45.54901144,
        50.651153,
        59.2771411,
        86.38018963,
        114.1459826,
        205.5525169
        
)

CDC = c(252.24371075,
  256.4967474,
        266.8260484,
        264.8865639,
        261.0909683,
        256.2507991,
        262.8601093,
        258.3981521,
        265.0033054,
        277.4129181,
        266.0880628,
        267.7468552,
        266.4713051,
        271.3719959,
        275.6957818,
        276.6789371,
        275.5063001,
        278.7956416,
        279.8336242,
        301.0225282,
        325.1108408
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
