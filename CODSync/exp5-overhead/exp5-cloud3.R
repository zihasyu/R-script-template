rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-cloud.pdf"
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

FileRecipe = c(
  1.66486608,
  1.485269977,
  1.956807871,
  1.678876268
)

DeletionRecipe = c(
  0.384337243,
  0.306961715,
  0.368725742,
  0.395859179
)

GarbageChunk = c(
  23.41975105,
  29.67669651,
  25.86072916,
  35.4295049
)

data <- data.frame(
  category = rep(
    c("MdKuber", 
      "HtmlCv", 
      "TarDiffu",
      "TarLinux"), 
    times = 3),
  
  subcategory = rep(
    c("Garbage Chunk",   
      "Deletion Recipe",
      "File Recipe"),                
    each = 4),
  
  value = c(GarbageChunk,   
            DeletionRecipe,
            FileRecipe)
)

data$category <- factor(data$category, levels = c(
  "MdKuber", 
  "HtmlCv", 
  "TarDiffu",
  "TarLinux"))

data$subcategory <- factor(data$subcategory, levels = c(
  "Garbage Chunk",    # 最上面
  "Deletion Recipe",
  "File Recipe"                # 最下面
))

x_offset = c(1, 2, 3, 4)
total = c(
  25.46895437,
  31.4689282,
  28.18626277,
  37.50424034
)

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(1.2, 4), ylim = c(2.05, 44)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(20, 40),
                     labels = c(20, 40)) +
  # 柱状图
  geom_col(data = data, 
                   aes(x = category, y = value, fill = subcategory), 
                   width = 0.5, 
                   color = "black", 
                   size = 0) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 40, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Overhead (%)", x = NULL) +
  # 设置隐藏legend
  theme(legend.position = "none") +
  # 设置颜色
  scale_fill_manual(name=NULL, 
                    values = c(
    "Garbage Chunk" = "#0000CD",
    "Deletion Recipe" = "#00CD00", 
    "File Recipe" = "#CD0000" ),
  limits=c("Garbage Chunk",
           "Deletion Recipe",
           "File Recipe")) +
  theme(axis.title.y = element_text(hjust = 1.1)) +
  # 设置文字
  geom_text(aes(x = x_offset, y = total), hjust = 0.5,
          vjust = 0.5, label=round(total, 1), angle = 0, size = 13, nudge_y = 4)
  # # 图例位置
  # theme(legend.position = c(0.46, 0.85)) +
  # # 图例背景设置为空
  # theme(legend.background = element_rect(fill = "transparent")) +
  # # 图例大小
  # theme(legend.text = element_text(size = 33)) +
  # theme(legend.key.height = unit(0.6, "cm")) +
  # guides(fill = guide_legend(override.aes = list(size = 5))) +
  # # 图例每行元素个数
  # guides(fill = guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
  #                            byrow = T))+ #默认F，表示升序填充，反之则降序



# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
