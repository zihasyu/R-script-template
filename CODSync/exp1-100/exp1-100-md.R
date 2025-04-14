rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp1-100-md.pdf"
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
  32.33934523,
  33.22207795,
  36.12221743,
  18.48416145,
  50.71925205,
  61.79247613,
  56.44133526,
  58.74426146,
  68.52506666,
  69.64452989,
  68.6311511,
  50.23434805,
  57.46183626,
  76.03577385,
  63.42715904,
  59.59474089,
  78.19184194,
  82.12056595,
  82.09959145,
  62.32955284
)

COD_Finesse = c (
  
)

COD_noGC = c (
  
)

DSync = c(
  29.08008538,
  22.87073312,
  31.47801952,
  17.05835786,
  37.75336241,
  38.50102448,
  33.44941162,
  33.58575936,
  30.92357307,
  30.50532236,
  28.06120053,
  17.36384392,
  39.41477983,
  48.30190121,
  28.16410809,
  41.90168605,
  40.18077442,
  53.34652061,
  51.63620832,
  48.90092132
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
