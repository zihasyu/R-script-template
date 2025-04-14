rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-garbage.pdf"
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

MD = c(
  0,
  4.10503087,
  5.862528338,
  6.482711747,
  11.94044519,
  14.18774437,
  16.37224234,
  19.47223396,
  20.56145329,
  21.59520164,
  23.50892262,
  27.42716548,
  30.25726314,
  30.96206193,
  33.02968577,
  35.5392352,
  36.5756445,
  37.31660229,
  38.05179786,
  40.56446097
  
)

HTML = c(
  0,
  7.282602756,
  7.797014722,
  8.860544767,
  18.92406568,
  20.65305737,
  49.79022537,
  50.47811506,
  51.12286781,
  51.46386475,
  51.51633221,
  53.19642801,
  54.79094315,
  55.66038125,
  56.91030145,
  58.11176407,
  0,
  11.18618558,
  16.16744676,
  23.38448194
)

Diff = c(
  0,
  9.698810595,
  12.72922876,
  17.22336825,
  31.73764096,
  34.35742329,
  36.14851157,
  40.88645723,
  48.15139773,
  49.77101009,
  51.43076652,
  53.00300707,
  57.04597456,
  56.71416981,
  0,
  6.780255523,
  23.62240046,
  27.84284079,
  31.87863201,
  34.66954736
)

Linux = c(
  0,
  7.481395548,
  10.82591955,
  13.79143227,
  30.44315927,
  42.65650648,
  52.02916434,
  52.69667128,
  56.16197748,
  57.14467264,
  59.85520854,
  60.30207795,
  0,
  25.12836549,
  41.73049768,
  42.82236867,
  46.79574779,
  0,
  16.69847679,
  33.95321302
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
  coord_cartesian(xlim =c(1.85, 19.7), ylim = c(0, 105)) +
  # 折线 + 散点
  geom_line(aes(x=xline1,y=Linux,color="TarLinux"), size=4) +
  geom_line(aes(x=xline1,y=Diff,color="TarDiffu"), size=4) +
  geom_line(aes(x=xline1,y=HTML,color="HtmlCv"), size=4) +
  geom_line(aes(x=xline1,y=MD,color="MdKuber"), size=4) + 
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
  labs(y = "Overhead (%)", x = "#-th Version") +
  # ylabel位置
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = c(1, 10, 20),
                     labels = c("1", "10", "20")) +
  scale_y_continuous(breaks = c(0, 50, 100),
                     labels = c("0", "50", "100")) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 图例
  scale_color_manual(name=NULL, 
                     values=c("MdKuber" = "#CD0000",
                              "HtmlCv" = "#00CD00",
                              "TarDiffu" = "#0000CD",
                              "TarLinux" = "#8DB6CD"),
                     limits=c("MdKuber",
                              "HtmlCv",
                              "TarDiffu",
                              "TarLinux"))+
  # scale_shape_manual(name=NULL,
  #                    values=c("MD" = 24,
  #                             "HtmlCv" = 23,
  #                             "Diffuses" = 22,
  #                             "Linux" = 21),
  #                    limits=c("MD",
  #                             "HTML",
  #                             "Diffuses",
  #                             "Linux")) +
  # 图例位置
  theme(legend.position = c(0.38, 0.84)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 42)) +
  theme(legend.key.width = unit(1, "cm")) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  theme(
    legend.spacing.x = unit(2, "cm")          # 增加图例元素之间的水平间距
  ) +
  # 图例每行元素个数
  guides(color=guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = T))#默认F，表示升序填充，反之则降序

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
