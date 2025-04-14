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

Garbage = c(
  0,
  28.03287224,
  31.7431808,
  33.56751868,
  40.08333258,
  45.05415803,
  53.07794525,
  59.53509274,
  61.53513428,
  62.39127548,
  63.07378549,
  65.1649248,
  0,
  6.973351727,
  28.43399321,
  43.78485769,
  47.11081517,
  0,
  8.538551299,
  21.70768341
)

Deletion = c(
  0,
  0.666076427,
  0.626201541,
  0.605041133,
  0.54722331,
  0.502132942,
  0.43030428,
  0.364552565,
  0.346658172,
  0.33653786,
  0.325300119,
  0.307916258,
  0.872261372,
  0.376049041,
  0.291237858,
  0.224622163,
  0.210888691,
  0.385140911,
  0.353747155,
  0.304503259
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
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(0, 63)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Deletion*60,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=Garbage,color="CODSync"), size=4) +
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
  labs(y = "Overhead (%)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 20),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 30, 60),
                     labels = c(0, 30, 60),
                     sec.axis = sec_axis(
                       trans = ~ ./60,                      # 辅助轴变换关系（主轴值除以 10）
                       name = NULL,                  # 辅助轴名称
                       breaks = seq(0, 1, by = 0.5)          # 自定义刻度：0 到 25，每隔 5
                     )) +
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
