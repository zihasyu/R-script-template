rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-restore.pdf"
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

MD = c(2.895892253,
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

HTML = c(15.16873632,
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

Diff = c(27.39408406,
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

Linux = c(47.18161764,
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



#x轴数据
xline1 <- numeric(20)
for (i in 1:20){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 19.5), ylim = c(7, 220)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Linux,color="Linux"), size=2) +
  geom_point(aes(x=xline1,y=Linux,color="Linux",shape="Linux"),
             size = 6, stroke=2) +
  geom_line(aes(x=xline1,y=Diff,color="Diffuses"), size=2) +
  geom_point(aes(x=xline1,y=Diff,color="Diffuses",shape="Diffuses"),
             size = 6, stroke=2) +
  geom_line(aes(x=xline1,y=HTML,color="HTML"), size=2) +
  geom_point(aes(x=xline1,y=HTML,color="HTML",shape="HTML"),
             size = 6, stroke=2) +
  geom_line(aes(x=xline1,y=MD,color="MD"), size=2) + 
  geom_point(aes(x=xline1,y=MD,color="MD",shape="MD"),
             size = 6, stroke=2) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text = element_text(size = 23, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 23)) +
  theme(axis.title.y = element_text(size = 23)) +
  # 设置label内容
  labs(y = "Speed (MiB/s)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 11, 20),
                     labels = c("1", "11", "20")) +
  scale_y_continuous(breaks = c(0, 100, 200),
                     labels = c(0, 100, 200)) +
  # 设置隐藏legend
  # theme(legend.position = "none") +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("MD" = "#AD0626",
                              "HTML" = "#B79AD1",
                              "Diffuses" = "#F2BE5C",
                              "Linux" = "#75B8BF"),
                     limits=c("MD",
                              "HTML",
                              "Diffuses",
                              "Linux"))+
  scale_shape_manual(name=NULL,
                     values=c("MD" = 24,
                              "HTML" = 23,
                              "Diffuses" = 22,
                              "Linux" = 21),
                     limits=c("MD",
                              "HTML",
                              "Diffuses",
                              "Linux")) +
  # 图例位置
  theme(legend.position = c(0.1, 0.73)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5)))

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 3.2)
