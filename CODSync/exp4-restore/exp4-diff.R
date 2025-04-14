rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-diff.pdf"
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
  46.83861747,
  46.6497787,
  47.01977227,
  48.01801576,
  49.50255496,
  47.41335828,
  50.69342813,
  52.6335028,
  53.90614631,
  56.89603385,
  63.8517245,
  63.74497321,
  67.05966326,
  67.26626773,
  70.39270229,
  77.21715822,
  107.5752051,
  182.7598409,
  251.9860904
)

COD_Finesse = c (
  
)

COD_noGC = c (
)

DSync = c(
  154.3159203,
  154.0815685,
  151.5415648,
  148.7441016,
  149.1782818,
  151.0045506,
  149.3942164,
  145.2293977,
  147.2112688,
  149.0003713,
  147.784387,
  147.3738867,
  147.6547419,
  143.2101512,
  143.8188527,
  143.7578849,
  141.9842053,
  142.511372,
  142.2983891
)

Rsyncrypto = c (
  19.00537334,
  18.29381307,
  18.22977939,
  17.20601809,
  17.2395346,
  17.18468833,
  17.21238732,
  16.66017237,
  16.63405001,
  16.48290859,
  17.0209715,
  16.88221561,
  16.83006436,
  16.37561445,
  16.25312348,
  16.1521192,
  17.39001752,
  17.25363117,
  17.26813632
)

ObliviSync = c(
  
)

#x轴数据
xline1 <- numeric(19)
for (i in 1:19){
  xline1[i] = i
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.55, 18.7), ylim = c(7, 270)) +
  # 折线 + 散点
  # geom_line(aes(x=xline1,y=COD_Finesse,color="Feature Based"), size=4) +
  # geom_line(aes(x=xline1,y=COD_noGC,color="noGC"), size=4) +
  geom_line(aes(x=xline1,y=Rsyncrypto,color="Rsyncrypto"), size=4) +
  # geom_line(aes(x=xline1,y=ObliviSync,color="ObliviSync"), size=4) +
  geom_line(aes(x=xline1,y=DSync,color="Dsync"), size=4) +
  geom_line(aes(x=xline1,y=COD,color="CODSync"), size=4) +
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
  scale_x_continuous(breaks = c(1, 10, 19),
                     labels = c("1", "10", "19")) +
  scale_y_continuous(breaks = c(0, 100, 200),
                     labels = c(0, 100, 200)) +
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
