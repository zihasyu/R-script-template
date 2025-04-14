rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-linux.pdf"
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

Data =c(
  211815992,
  215444056,
  217366128,
  218970048,
  218353944,
  218218536,
  217451736,
  221386280,
  221306248,
  222901808,
  226448976,
  225678856,
  225312760,
  226087256,
  224574440,
  228735848,
  229219080,
  230681112,
  231702592,
  230411184
)

Garbage = c(
  0.1,
  84704441,
  102023204,
  111659836,
  147421858,
  180583810,
  248256760,
  328681699,
  357259970,
  373123483,
  390235992,
  425935586,
  0.1,
  17016468,
  89590583,
  178872540,
  204993299,
  0.1,
  21715008,
  64134290
)

Deletion = c(
  0.1,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  2012624,
  917640,
  917640,
  917640,
  917640,
  917640,
  899640,
  899640
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
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(0.3, 20)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=log(Deletion)+1,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=log10(Garbage)+1,color="CODSync"), size=4) +
  geom_line(aes(x=xline1,y=log(Data)+1,color="ObliviSync"), size=4) +
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
  labs(y = "Size (byte)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 20),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 1, 5, 9),
                     labels = c(0, expression(10^0), expression(10^4), expression(10^8))) +
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
