rm(list = ls())

exportPath1 = "C:/Users/"
exportPath2 = "/Desktop/"
exportName = "exp5-meta.pdf"
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
Deletion = c(
  0.370802385,
  0.31687657,
  0.400928498,
  0.476835
)

File = c(
  1.678968513,
  1.397561383,
  1.789016013,
  1.725924192
)

barWidth = 0.35
x3_offset = c(0, 1, 2, 3)
x1_offset = numeric(4)
x2_offset = numeric(4)
for(i in 1:4) {
  x1_offset[i] = x3_offset[i] - 0.5 * barWidth
  x2_offset[i] = x3_offset[i] + 0.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.3, 3.3), ylim = c(0.142, 3)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 1, 2),
                     labels = c(0, 1, 2)) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = Deletion, fill = "Deletion recipe"), width = barWidth,
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = File, fill = "Recipe chunks"), width = barWidth,
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 39, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Overhead (%)", x = "Dataset") +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("MdKuber", "HtmlCv", "TarDiffu", "TarLinux")) +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 标签
  geom_text(aes(x = x1_offset, y = Deletion), hjust = 0.5,
            vjust = 0.5, label=round(Deletion,1), angle = 90, size = 13, nudge_y = 0.32) +
  geom_text(aes(x = x2_offset, y = File), hjust = 0.5,
            vjust = 0.5, label=round(File,1), angle = 90, size = 13, nudge_y = 0.32) +
  # 图例
  scale_fill_manual(name=NULL, 
                     values=c("Deletion recipe" = "#CD0000",
                              "Recipe chunks" = "#0000CD"),
                     limits=c("Deletion recipe",
                              "Recipe chunks"))+
  # 图例位置
  theme(legend.position = c(0.5, 0.9)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 42)) +
  theme(legend.key.width = unit(1, "cm")) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  theme(
    legend.spacing.x = unit(2, "cm")          # 增加图例元素之间的水平间距
  ) +
  # 图例每行元素个数
  guides(fill=guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = T))#默认F，表示升序填充，反之则降序


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 5)
