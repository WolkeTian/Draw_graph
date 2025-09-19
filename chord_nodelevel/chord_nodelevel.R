# Load required packages
library(tidyverse)      # Data manipulation and visualization
library(ggtext)         # Text formatting in ggplot2
library(tidytext)       # Text mining tools
library(here)           # File path management
library(circlize)       # Circular visualization
library(schrute)        # The Office dataset
library(openxlsx)

# 设置输出路径（当前路径下）
setwd("E:/Projects/离线工作记忆/graphs/chordDiag_nodelevel")


# 读取node和net对应关系的表格
mapping_df <- read.xlsx("node_to_network.xlsx")

# 检查数据
head(mapping_df)

# 只保留nodename和Module
node_to_module <- mapping_df[, c(2, 3)]
colnames(node_to_module) <- c("node", "module")

# 确保node是字符串（方便和矩阵行列名对应）
node_to_module$node <- as.character(node_to_module$node)

# 查看结果
head(node_to_module)

# 定义17个模块
module_names <- c("CNa", "CNb", "CNc", 
                  "DMNa", "DMNb", "DMNc", 
                  "DANa", "DANb", 
                  "LNa", "LNb", 
                  "SNa", "SNb", 
                  "SMNa", "SMNb", 
                  "TempPar", 
                  "VisCent", "VisPeri")

# 颜色定义（每个模块一个颜色）
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
names(pal_color) <- module_names
# mat 是 节点 × 节点 的矩阵，行名和列名必须为节点编号，且和 Excel 中的第一列一致
# 读取节点连接矩阵
df <- read.xlsx("sig_nodes.xlsx")

# 设置第一列为行名
rownames(df) <- df[[1]]
df <- df[,-1]

# 转为矩阵
mat <- as.matrix(df)
# 强制对称
mat <- (mat + t(mat)) / 2

# 确认矩阵节点顺序与node_to_module一致
stopifnot(all(rownames(mat) %in% node_to_module$node))
# 现在将每个节点映射到对应模块，并给节点着色：
# 按矩阵顺序排列
node_to_module <- node_to_module[match(rownames(mat), node_to_module$node), ]

# 每个节点的颜色 = 对应模块的颜色
node_colors <- pal_color[node_to_module$module]
names(node_colors) <- node_to_module$node

# 每个模块的节点数量
module_sizes <- table(mapping_df$network)

# 输出高分辨率图片
ragg::agg_png("network_chord.png", res = 450, width = 12, height = 12, units = "in", bg = "white")

# 设置字体为Arial
par(family = "Arial", cex = 1.5, bg = "white")
# 将小于2的边设为白色
threshold <- 2
link_col <- ifelse(mat < threshold, "white", rgb(0, 0, 0, alpha = 0.3))

# ========== 9. 初始化 Circos 图 ==========
circos.clear()
circos.par(
  start.degree = 90,
  gap.degree = 2,
  track.margin = c(0.01, 0.01),
  canvas.xlim = c(-1.18, 1.18),   # 调大画布边界，比例值
  canvas.ylim = c(-1.18, 1.18)    # 调大画布边界
)

# 每个模块是一个连续的 sector，不再分散
sector_data <- data.frame(
  sector = names(module_sizes),
  x_start = rep(0, length(module_sizes)),
  x_end = as.numeric(module_sizes)
)

# 初始化
circos.initialize(factors = sector_data$sector, xlim = sector_data[, c("x_start", "x_end")])

# ========== 10. 绘制模块外圈（连续色块） ==========
circos.trackPlotRegion(
  ylim = c(0, 1),
  bg.border = NA,
  track.height = 0.16, # 外环的宽度
  panel.fun = function(x, y) {
    module_name <- CELL_META$sector.index
    module_color <- pal_color[module_name]
    
    # 绘制连续色块
    circos.rect(CELL_META$xlim[1], 0, CELL_META$xlim[2], 1, col = module_color, border = NA)
    
    # 模块名称居中显示
    mid <- mean(CELL_META$xlim)
    circos.text(mid, 1.5, module_name,
                facing = "clockwise",
                niceFacing = TRUE,
                cex = 1.4, # 字体大小
                adj = c(0, 0.5),
                col = "black")
  }
)

# ========== 11. 绘制内部连线（节点级别） ==========
# 为每个节点分配在所属模块中的位置
node_positions <- node_to_module %>%
  group_by(module) %>%
  mutate(pos = row_number() - 0.5) %>%
  ungroup()

# 循环绘制连线（两端模块颜色平均值 + 透明度）
library(colorspace)
for (i in 1:(nrow(mat)-1)) {
  for (j in (i+1):ncol(mat)) {
    value <- mat[i, j]
    if (!is.na(value) && value >= threshold) {
      module_i <- node_to_module$module[i]
      module_j <- node_to_module$module[j]
      pos_i <- node_positions$pos[i]
      pos_j <- node_positions$pos[j]
      
      col_i <- pal_color[module_i]
      col_j <- pal_color[module_j]
      
      # 使用颜色渐变（circlize 支持 vector of 2 colors）
      circos.link(
        sector.index1 = module_i, point1 = pos_i,
        sector.index2 = module_j, point2 = pos_j,
        col = c(col_i, col_j),
        lwd = 5, # 宽度
        border = 'white'
      )
    }
  }
}

# 保存图片
dev.off()
