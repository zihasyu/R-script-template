rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-100-linux.pdf"
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

COD = c(
  21.0382657,
  69.5549374,
  105.12412,
  53.22202534,
  55.60918176,
  26.20528466,
  22.3264703,
  64.0389071,
  84.66883742,
  77.43136885,
  55.2022711,
  64.6082318,
  105.0662945,
  26.34302295,
  21.69540305,
  70.6283086,
  106.8139133,
  97.32165954,
  50.41228908,
  24.74022501
)

COD_Finesse = c (
  
)

COD_noGC = c (
  
)

DSync = c(
  15.15872429,
  23.18871243,
  21.98120568,
  23.7863963,
  23.54255173,
  17.73032861,
  15.77711724,
  23.88727727,
  22.02391854,
  21.7502198,
  23.79941013,
  24.00575515,
  23.63292213,
  17.71985346,
  15.60370671,
  23.06512395,
  23.12239413,
  23.48844939,
  24.04021318,
  17.65790857
)

Rsyncrypto = c (
  
)

ObliviSync = c(
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
  coord_cartesian(xlim =c(1.55, 19.7), ylim = c(5, 110)) +
  # 折线 + 散点
  # geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=3) +
  # geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=3) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=3) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=3) +
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
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 20),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c(0, 50, 100)) +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("CODSync" = "#DF9E9B",
                              "Feature Based" = "#99BADF",
                              "noGC" = "#D8E7CA",
                              "Rsyncrypto" = "#99CDCE",
                              "ObliviSync" = "#999ACD",
                              "Dsync" = "#FFD0E9"),
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
