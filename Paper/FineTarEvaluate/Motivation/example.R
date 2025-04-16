rm(list = ls())
# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

exportName = "pic2-ucp.pdf"
exportPath = "./"

library(ggplot2)
library(ggbreak)
library(readxl)
library(extrafont)
library(showtext)  
library(gg.gap)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

OffsetChunk = c(0.211018,
                0.473857,
                0.031107,
                0.178961
)

ModifiedChunk= c(
                 0.21847,
                 0.0581643,
                 0.0334536,
                 0.130792
)

Workloads=c("Linux",
            "Web",
            "Chro.",
            "Auto."
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

# 创建一个空数据框，用于生成图例
legend_data <- data.frame(
  x = c(0, 0),
  y = c(0, 0),
  fill = c("Off. C.", "Mix. Mod.")
)

#绘图
p <- ggplot() + 
  # 坐标轴显示范围
  coord_cartesian(xlim =c(-0.4, 3.4), ylim = c(0, 1.1)) +
  # 设置y坐标标签
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     labels = c(0, "0.5", "1.0"),
                     expand = c(0, 0)) +
  # 柱状图
  geom_col(aes(x = x1_offset, y = OffsetChunk, fill = "Off. C."), width = 0.35, 
           color = "black", size = 0.5) +
  geom_col(aes(x = x2_offset, y = ModifiedChunk, fill = "Mix. Mod."), width = 0.35, 
           color = "black", size = 0.5) +
  # 手动设置填充颜色
  scale_fill_manual(name = "", values = c("Off. C." = "#AD0626", "Mix. Mod." = "#B79AD1")) +
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
  labs(y = " FRP ", x = "Workloads") +
  # 设置刻度内容及位置
  scale_x_continuous(breaks = x3_offset,
                     labels = Workloads) +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  # 标签
  geom_text(aes(x = x2_offset, y = ModifiedChunk), hjust = 0,
            vjust = 0.5, label=round(ModifiedChunk, 2), angle = 90, size = 13, nudge_y = 0.05) +
  geom_text(aes(x = x1_offset, y = OffsetChunk), hjust = 0,
            vjust = 0.5, label=round(OffsetChunk,2), angle = 90, size = 13, nudge_y = 0.05) +
  # 图例设置
  theme(
    legend.position = c(0.85, 0.85),
    legend.text = element_text(size = 30),
    legend.margin = margin(b = 10)
  )

# 保存文件
ggsave(paste(exportPath, exportName, sep=""), plot = p, width = 12, height = 6)