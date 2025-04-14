rm(list = ls())

exp = "OneTerm"
exportPath = "C:/Users/YangJingyuan/Desktop/"
exportName = "exp-breakdown.pdf"

root = "D:/RProject/CODSync"
setwd(sprintf("%s/%s", root, exp))



library(ggplot2)
library(ggpattern)
library(readxl)
library(extrafont)
library(showtext)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

ChunkRedunce = c(17.21859986,
                 40.50298056,
                 17.05476339,
                 23.55830663,
                 4.835950863,
                 14.12005457,
                 29.32578795,
                 34.81502353
)

DeltaCompression = c(28.8349348,
                     36.20517838,
                     75.23031699,
                     70.25896937,
                     5.084745763,
                     15.66166439,
                     46.52025634,
                     56.95325775
)

Encryption = c(0.763555251,
               3.163559276,
               2.106938726,
               1.754431152,
               0.388742031,
               0.818553888,
               2.293595173,
               2.344731614
)

Deletion = c(51.60432395,
             9.804134732,
             1.250596082,
             1.108371965,
             88.10449386,
             67.33969986,
             19.19574261,
             1.945077789
)

Version = c(1.578586136,
            10.32414705,
            4.357384808,
            3.319920879,
            1.586067486,
            2.060027285,
            2.664617922,
            3.94190931
)

Other = c(0,
          0,
          0,
          0,
          0,
          0,
          0,
          0
)

data <- data.frame(
  category = rep(
    c("MD", 
      "PPT",
      "HTML", 
      "Diffuser", 
      "SYN-16k", 
      "SYN-128k", 
      "SYN-1m", 
      "SYN-16m"), 
    times = 6),
  
  subcategory = rep(
    c("DeltaCompression",   
      "ChunkRedunce",
      "Encryption",
      "Deletion",
      "Version",
      "Other"),                
    each = 8),
  
  value = c(DeltaCompression,   
            ChunkRedunce,
            Encryption,
            Deletion,
            Version,
            Other)
)

data$category <- factor(data$category, levels = c(
  "MD", 
  "PPT", 
  "HTML", 
  "Diffuser", 
  "SYN-16k", 
  "SYN-128k", 
  "SYN-1m", 
  "SYN-16m"))

data$subcategory <- factor(data$subcategory, levels = c(
  "ChunkRedunce",    # 最上面
  "DeltaCompression",
  "Version",
  "Deletion",
  "Encryption",
  "Other"                # 最下面
))

x_offset = c(1, 2, 3, 4, 5, 6)
total = c(95, 95, 95, 95, 95, 95)

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.1, 7.9), ylim = c(4.5, 98)) +
  # 柱状图
  geom_col_pattern(data = data, 
           aes(x = category, y = value, fill = subcategory, pattern = subcategory), 
           position = "stack",
           width = 0.8, 
           color = "black", 
           size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  theme(axis.text.y = element_text(size = 22, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 22)) +
  theme(axis.title.y = element_text(size = 22)) +
  # 设置label内容
  labs(y = "Time Duration (%)", x = "Dataset Names") +
  # 设置颜色
  scale_fill_manual(values = c(
    "ChunkRedunce" = "#F8CBAD",
    "DeltaCompression" = "#B4C7E7", 
    "Version" = "#C5E0B4", 
    "Deletion" = "#FFE699", 
    "Encryption" = "#7030A0", 
    "Other" = "#E7E6E6" 
  )) +
  # 设置填充模式
  scale_pattern_manual(values = c(
    "ChunkRedunce" = "none", 
    "DeltaCompression" = "none", 
    "Encryption" = "none", 
    "Deletion" = "none", 
    "Version" = "none", 
    "Other" = "none"
  ))
  # 设置文字
  # geom_text(aes(x = x_offset, y = total), hjust = 0.5,
  #           vjust = 0.5, label=total, angle = 0, size = 8.2, nudge_y = 2)


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 3.5)
