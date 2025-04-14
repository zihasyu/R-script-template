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
init = c(
  36.57446499,
  46.862899,
  37.53103103
  
)

second = c(
  19.29223836,
  27.55958557,
  19.74775969
)

barWidth = 0.4
x3_offset = c(0, 1, 2)
x1_offset = numeric(3)
x2_offset = numeric(3)
for(i in 1:3) {
  x1_offset[i] = x3_offset[i] - 0.5 * barWidth
  x2_offset[i] = x3_offset[i] + 0.5 * barWidth
}

# 设置字体
windowsFonts(Arial=windowsFont("Arial"))
#绘图
ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.4, 2.4), ylim = c(3, 66)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 25, 50),
                     labels = c(0, "25", "50")) +
  # 柱状图
  geom_col(aes(x = x1_offset,y = init, fill = "First GC"), width = 0.4, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset,y = second, fill = "Second GC"), width = 0.4, 
           color = "black", size = 0.5) +
  # 白底，没有上边框和右边框
  theme_classic() +
  # 设置坐标轴上字体大小
  theme(axis.text.x = element_text(size = 42, color = "black")) +
  theme(axis.text.y = element_text(size = 42, color = "black")) +
  # 设置字体样式
  theme(text = element_text(family = "Arial")) +
  # 设置label字体大小
  theme(axis.title.x = element_text(size = 42)) +
  theme(axis.title.y = element_text(size = 42)) +
  # 设置label内容
  labs(y = "Overhead (%)", x = NULL) +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = c("HTML", "Diffusers", "Linux")) +
  theme(axis.title.y = element_text(hjust = 1.1)) +
  # 标签
  geom_text(aes(x = x1_offset, y = init), hjust = 0.5,
            vjust = 0.5, label=round(init, 1), angle = 0, size = 13, nudge_y = 6) +
  geom_text(aes(x = x2_offset, y = second), hjust = 0.5,
            vjust = 0.5, label=round(second, 1), angle = 0, size = 13, nudge_y = 6) +
  # 图例
  scale_fill_manual(name=NULL, 
                     values=c("First GC" = "#CD0000",
                              "Second GC" = "#0000CD"),
                     limits=c("First GC",
                              "Second GC"))+
  # 图例位置
  theme(legend.position = c(0.325, 0.92)) +
  # 图例背景设置为空
  theme(legend.background = element_rect(fill = "transparent")) +
  # 图例大小
  theme(legend.text = element_text(size = 33)) +
  theme(legend.key.height = unit(0.5, "cm")) +
  guides(fill = guide_legend(override.aes = list(size = 5))) +
  # 图例每行元素个数
  guides(fill = guide_legend(ncol = 2, #根据ncol或者nrow设置图例显示行数或列数（设置一个即可）
                            byrow = T)) #默认F，表示升序填充，反之则降序


# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = last_plot(), width = 10, height = 4)
