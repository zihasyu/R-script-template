rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-metadata-overall.pdf"
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

MD = c(2.133004693,
       2.090288866,
       2.194037894,
       2.141588481,
       1.937544974,
       2.002893189,
       2.101310109,
       2.107627728,
       2.001646157,
       2.090029391,
       2.151691354,
       2.214988023,
       2.395806876,
       2.449777746,
       2.481572427,
       2.59114634,
       2.443903895,
       2.479335398,
       2.537806863,
       2.526464104,
       2.388412133
)

HTML = c(1.169085787,
         1.609714032,
         1.691333878,
         1.956980909,
         2.099053928,
         2.443504755,
         2.477789843,
         1.612707759,
         1.867112621,
         1.990139302,
         2.194622402,
         2.42612497,
         2.584903705,
         2.546612257,
         2.650879486,
         2.587143221,
         2.579769643,
         2.284288012,
         2.433000057,
         2.50297258,
         2.422512203
)

Diff = c(1.695038648,
         2.213596882,
         2.638043829,
         2.994595359,
         2.367177722,
         2.514408275,
         2.742783528,
         3.089491303,
         2.439575884,
         2.61062896,
         2.858048865,
         3.176990149,
         2.695986751,
         2.90473822,
         3.052112526,
         3.105423132,
         2.67806333,
         2.79561549,
         2.98498326,
         3.104882333,
         2.806260418
)

Linux = c(1.711931301,
          2.163050373,
          2.760134277,
          3.29836119,
          2.94306915,
          2.591969508,
          2.221628448,
          2.516733398,
          2.518563072,
          2.925692409,
          3.284096084,
          2.879986842,
          2.693919625,
          3.032496121,
          2.343075534,
          2.526667996,
          2.634918759,
          3.062733753,
          3.405567361,
          2.824531314,
          2.180827745
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
  coord_cartesian(xlim =c(1.55, 20.5), ylim = c(0.1, 4.1)) +
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
  labs(y = "Meta. Overhead (%)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.9)) +
  # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 11, 21),
                     labels = c("1", "11", "21")) +
  scale_y_continuous(breaks = c(0, 2, 4),
                     labels = c("0%", "2%", "4%")) +
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
  theme(legend.position = c(0.28, 0.9)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 21)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  # 图例每行元素个数
  guides(color=guide_legend(ncol = 4, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = T))#默认F，表示升序填充，反之则降序

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 3.2)
