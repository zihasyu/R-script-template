rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp-real-storage-overall.pdf"
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

MD = c(102.6111532,
       108.3109081,
       112.1547138,
       113.9121355,
       111.9906595,
       118.7746718,
       121.8783936,
       124.9126566,
       129.4374284,
       131.2149032,
       132.9410378,
       136.2501122,
       143.6809901,
       149.444364,
       150.9320237,
       155.6331054,
       161.3410563,
       163.9733559,
       165.9529428,
       167.8495296,
       174.5967959
)

HTML = c(101.4503737,
         104.0051218,
         112.4020558,
         113.7051475,
         115.8077342,
         141.4349209,
         146.641304,
         226.653326,
         229.8946753,
         232.4484955,
         232.3741609,
         231.8747288,
         236.8041434,
         244.220714,
         248.8838723,
         104.3330354,
         111.4239067,
         143.2724067,
         152.3981722,
         102.7426206,
         113.0024068
)

Diff = c(102.1057807,
         115.7276118,
         126.6897234,
         131.5093948,
         156.0847006,
         164.6906621,
         171.3204185,
         176.4889065,
         209.0626987,
         217.1018809,
         223.8914706,
         230.3680638,
         101.0309684,
         108.457196,
         118.5202352,
         129.0484297,
         147.6932473,
         157.4982323,
         101.6394438,
         110.159349,
         126.3483365
)

Linux = c(102.1262282,
          142.3296915,
          150.9917435,
          155.9735018,
          172.4451731,
          187.4905719,
          218.9243541,
          254.7184602,
          268.0167318,
          275.2167706,
          281.2719696,
          297.0508022,
          104.2485801,
          110.7872418,
          143.1713007,
          182.7030185,
          194.4225247,
          106.0391969,
          113.0966662,
          131.4454437,
          169.2430489
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
  coord_cartesian(xlim =c(1.85, 20.3), ylim = c(95, 330)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Linux,color="Linux"), size=2) +
  geom_point(aes(x=c(13, 18),y=c(104.2485801, 106.0391969)),
             size = 13, stroke=2 ,shape="\u2605", color="#75B8BF") +
  geom_line(aes(x=xline1,y=Diff,color="Diffuses"), size=2) +
  geom_point(aes(x=c(13, 19),y=c(101.0309684, 101.6394438)),
             size = 13, stroke=2 ,shape="\u2605", color="#F2BE5C") +
  geom_line(aes(x=xline1,y=HTML,color="HTML"), size=2) +
  geom_point(aes(x=c(16, 20),y=c(104.3330354, 102.7426206)),
             size = 13, stroke=2 ,shape="\u2605", color="#B79AD1") +
  geom_line(aes(x=xline1,y=MD,color="MD"), size=2) + 
  # geom_point(aes(x=xline1,y=MD,color="MD",shape=24),
  #            size = 6, stroke=2, shape=24) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text = element_text(size = 23, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 23)) +
  theme(axis.title.y = element_text(size = 21.5)) +
  # 设置label内容
  labs(y = "Storage Overhead (%)", x = "#-th Versions") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 1)) +
  # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 11, 21),
                     labels = c("1", "11", "21")) +
  scale_y_continuous(breaks = c(100, 200, 300),
                     labels = c("100%", "200%", "300%")) +
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
  # scale_shape_manual(name=NULL,
  #                    values=c("MD" = 24,
  #                             "HTML" = 23,
  #                             "Diffuses" = 22,
  #                             "Linux" = 21),
  #                    limits=c("MD",
  #                             "HTML",
  #                             "Diffuses",
  #                             "Linux")) +
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
