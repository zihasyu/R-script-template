rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp8-linux.pdf"
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

Traffic = c(
  23.98096806,
  20.06943719,
  18.41143096,
  16.68392936,
  16.33932269,
  15.39270989,
  15.45664848
)

Restore = c (
  7337.052632,
  13751.21053,
  19950.78947,
  27210.42105,
  31800.57895,
  37099.42105,
  40111.42105
)

#x轴数据
xline1 <- numeric(7)
for (i in 1:7){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(0.7, 7.3), ylim = c(0, 26)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Traffic,color="Traffic"), size=4) +
  geom_point(aes(x=xline1,y=Traffic,color="Traffic",shape="Traffic"),
             size = 10, stroke=4) +
  geom_line(aes(x=xline1,y=(Restore/44000*26),color="Restore"), size=4) +
  geom_point(aes(x=xline1,y=(Restore/44000*26),color="Restore",shape="Restore"),
             size = 10, stroke=4) +
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
  labs(y = "Traffic (%)", x = "T") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  theme(axis.title.y.right = element_text(hjust = 0)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 3, 5, 7),
                     labels = c(1, 3, 5, 7)) +
  scale_y_continuous(breaks = c(0, 10, 20),
                     labels = c(0, 10, 20),
                     sec.axis = sec_axis(
                       trans = ~ .*44000/26,
                       name = "#Decomp.",
                       breaks = c(0, 20000, 40000),
                       labels = c("0", "20K", "40K")
                     )) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("Traffic" = "#CD0000",
                              "Restore" = "#0000CD"),
                     limits=c("Traffic",
                              "Restore"))+
  scale_shape_manual(name=NULL, 
                     values=c("Traffic" = 24,
                              "Restore" = 23),
                     limits=c("Traffic",
                              "Restore")) +
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
