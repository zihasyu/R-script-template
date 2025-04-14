rm(list = ls())

exp = "OneTerm"
exportPath = "C:/Users/杨劲远/Desktop/"
exportName = "exp5.pdf"

root = "D:/RProject/CODSync"
setwd(sprintf("%s/%s", root, exp))

library(ggplot2)
library(ggpattern)
library(readxl)
library(extrafont)
library(showtext)

font_add('Arial','C:/Windows/Fonts/arial.ttf')
showtext_auto()

# 数据定义
ChunkRedunce <- c(15, 15, 15, 15, 15, 15)
DeltaCompression <- c(15, 15, 15, 15, 15, 15)
Encryption <- c(15, 15, 15, 15, 15, 15)
Deletion <- c(15, 15, 15, 15, 15, 15)
Version <- c(15, 15, 15, 15, 15, 15)
Other <- c(15, 15, 15, 15, 15, 15)

data <- data.frame(
  category = rep(
    c("Md", "Html", "Diffuser", "SYN-16k", "SYN-1m", "SYN-16m"), 
    each = 6
  ),
  subcategory = rep(
    c("ChunkRedunce", "DeltaCompression", "Encryption", "Deletion", "Version", "Other"), 
    times = 6
  ),
  value = c(ChunkRedunce, DeltaCompression, Encryption, Deletion, Version, Other)
)

# 绘图
ggplot() +
  coord_cartesian(xlim = c(0.9, 6), ylim = c(0, 100)) +
  geom_col_pattern(data = data, 
                   aes(x = category, y = value, fill = subcategory, pattern = subcategory), 
                   position = "stack",
                   pattern_density = 0.1,
                   pattern_fill = "black",
                   pattern_angle = 45,
                   width = 0.9, 
                   color = "black", 
                   size = 0.5) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 22, color = "black")) +
  theme(axis.text.y = element_text(size = 27, color = "black")) +
  theme(text = element_text(family = "Arial")) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 32)) +
  theme(axis.title.y = element_text(size = 30)) +
  labs(y = "Time Duration", x = "Dataset") +
  scale_fill_manual(values = c(
    "ChunkRedunce" = "red", 
    "DeltaCompression" = "green", 
    "Encryption" = "blue", 
    "Deletion" = "purple", 
    "Version" = "orange", 
    "Other" = "pink"
  )) +
  scale_pattern_manual(values = c(
    "ChunkRedunce" = "stripe", 
    "DeltaCompression" = "none", 
    "Encryption" = "stripe", 
    "Deletion" = "none", 
    "Version" = "stripe", 
    "Other" = "none"
  ))

# 保存文件
ggsave(paste(exportPath, exportName, sep = ""), plot = last_plot(), width = 10, height = 4)
