# Load required packages
library(tidyverse)      # Data manipulation and visualization
library(ggtext)         # Text formatting in ggplot2
library(tidytext)       # Text mining tools
library(here)           # File path management
library(circlize)       # Circular visualization
library(schrute)        # The Office dataset
library(openxlsx)
# 设置输出路径（当前路径下）
setwd("E:/Projects/离线工作记忆/graphs/chordDiagram")
# 设置输出路径（当前路径下）
base_path <- here("generated_chord_diagram")


# Create a square matrix from mentions data, with character names as row and column names
df <- read.xlsx("./spcfwe_mat.xlsx")
# 设为行名
rownames(df) <- df[[1]]

#删除第一列
df <- df[,-1]
# 转为矩阵 保留下三角即可
mat <- as.matrix(df)

# Define color palette for characters and background
# 定义新的调色板，包含17种颜色
pal_color <- c(
  "#1F77B4", "#6CA4D9", "#B9D1ED",   # CNa, CNb, CNc
  "#A463D7", "#C39AE4", "#E1D0F2",   # DMNa, DMNb, DMNc
  "#FF7F0E", "#FFBD80",               # DANa, DANb
  "#E377C2", "#F2BADF",               # LIMBICa, LIMBICb
  "#2CA02C", "#93D193",               # SNa, SNb
  "#17BECF", "#93E2EA",               # SMNa, SMNb
  "#BCBD22",                          # TempPar
  "#D62728", "#E9989A"                # VisCent, VisPeri
)
bg_color <- "#FFFFFF"

# 3. 生成 link.col，控制每条连接的颜色
# 如果 mat[i, j] < 10，则该连接边设为灰色，否则设为对应区域的颜色
link.col <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))

for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    if (mat[i, j] < 10) {
      link.col[i, j] <- "white"  # 白色边
    } else {
      # 保留原本的分区颜色（使用起点节点的颜色）
      link.col[i, j] <- pal_color[i]
    }
  }
}

# 绘制和保存chord diagram
ragg::agg_png(here(base_path, "17-network.png"),
              res = 500, width = 8, height = 8, units = "in", bg = bg_color)
par(
  family = "Arial", cex = 2, col = "black", # font family, size, color
  bg = bg_color, 
  mai = rep(0.5, 4) # plot margin in inches
) 
chordDiagram(mat, transparency = 0.3, 
             grid.col = pal_color[1:17], # assign colors to each character
             link.border = "white", link.lwd = 0.2, # add thin white borders to connections
             col = link.col, # 使用自定义的边颜色
             annotationTrack = c("name", "grid"), # show only name and grid tracks (excluding axis)
             annotationTrackHeight = mm_h(c(3, 5)),
             link.largest.ontop = TRUE
)
title(
  main = "title",
  sub = "subtitle",
  col.main = "white", cex.main = 1.3)
invisible(dev.off())

# Display the saved plot in R Markdown
knitr::include_graphics("https://github.com/holtzy/R-graph-gallery/blob/master/character-interaction-analysis/17-network-the-office-mentions.png?raw=true")
