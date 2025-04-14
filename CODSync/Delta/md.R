rm(list = ls())

exp = "exp7"
exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp4-web.pdf"
username <- Sys.info()[["user"]]

root = "D:/RProject/tmp"
setwd(sprintf("%s/%s", root, exp))
exportPath = sprintf("%s%s%s", exportPath1, username, exportPath2)

library(ggplot2)
library(readxl)
library(extrafont)
# library(showtext)

# font_add('Arial','C:/Windows/Fonts/arial.ttf')
# showtext_auto()

timeOn= c(8.532468077,
          8.653943974,
          8.671632436,
          8.870031667,
          10.44025449,
          12.36790256,
          14.56786026,
          17.04673205,
          20.81616154,
          23.44258077,
          25.51552821
)

timeOff= c(26.00453205,
           26.33536474,
           25.50069628,
           25.39726231,
           21.85908,
           19.46728487,
           15.66588628,
           14.47314032,
           10.04770846,
           9.012287885,
           7.786040577
)

xline = c(0,
          0.01,
          0.02,
          0.03,
          0.04,
          0.05,
          0.06,
          0.07,
          0.08,
          0.09,
          0.10)


# 禁止科学计数法
options(scipen = 999)

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
# 绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(0.0, 0.1)) +
  # 折线
  geom_line(aes(x=xline,y=timeOn), color="#AD0626", size=3) + 
  geom_point(aes(x=xline,y=timeOn), 
             size = 9, stroke=4.5, color="#AD0626", shape = 23) +
  geom_line(aes(x=xline,y=timeOff), color="#2C3359", size=3) + 
  geom_point(aes(x=xline,y=timeOff), 
             size = 9, stroke=4.5, color="#2C3359", shape=24) +
  # 散点
  # geom_point(aes(x=xline,y=timeOn), color = "#AD0626", 
  #   size = 12) +
  # geom_point(aes(x=xline,y=timeOff), color = "#2C3359", 
  #   size = 14, shape = "X") +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text = element_text(size = 45, color = "black")) +
  # 设置字体样式
  # theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 45)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(x = "Threshold t") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(0, 0.05, 0.1),
                     labels = c(0, 0.05, 0.1)) +
  # 设置Y轴以及双Y轴
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(0, 30, 15),
                     name = "Time Duration (s)", 
                     sec.axis = sec_axis(trans = ~.*20,
                                         breaks = seq(0, 600, 300)))

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10.3, height = 5)
