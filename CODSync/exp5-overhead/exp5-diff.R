rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-diff.pdf"
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
  11.58969436,
  18.86050321,
  21.5460143,
  34.24542497,
  37.57636722,
  39.84845037,
  41.40670956,
  50.81975886,
  52.55613841,
  53.88006111,
  55.05052723,
  0,
  5.104083252,
  13.01807463,
  20.05685831,
  30.41728171,
  34.67291326,
  0,
  6.388311257
)

Deletion = c(
  0,
  0.751356469,
  0.654258737,
  0.630432054,
  0.49453853,
  0.46880574,
  0.45073444,
  0.437559705,
  0.354485486,
  0.341412032,
  0.330950376,
  0.292662026,
  0.66076193,
  0.300987448,
  0.252584027,
  0.231067579,
  0.201960473,
  0.170725478,
  0.263867858,
  0.243031988
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
