# encoding UTF-8, by Tian Yun
# install.packages("circlize") # 如未安装，首先安装该工具包
library("circlize")

# 读取存放在csv文件中的数据
df0 <- read.csv("network_flow.csv", stringsAsFactors=FALSE)
# chordDiagram(x = df0, annotationTrack = "grid")    # 该函数默认绘图，每次随机分配颜色
# 增加link.visible = df0$flow < 0,为只画负的，相应只画正的
# clolor = 'red'或 '#FF0000'等指定颜色,以及轮行指定颜色：col = df0$rowcol等
# 后可跟透明度，根据高低强调或区分，比如#FF000080 80是透明度

# 以下为自定义颜色绘图
# 读取配色和label
# label和数据里的数量和名称必须匹配，不能多也不能少
# order也要注意匹配
df1 <- read.csv("network_plot.csv", stringsAsFactors=FALSE)
# 绘图
chordDiagram(x = df0, grid.col = df1$col, transparency = 0.25,
             order = df1$region, annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1), 
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)


# 添加labels
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA,  # 外边圆环的轮廓宽度
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim") # 迭代获取每段圆环的起点和终点
    reg = get.cell.meta.data("sector.index") # 迭代获取sector name
    circos.text(x = mean(xlim), y = 2.4, 
                labels = reg, facing = "bending", cex = 1.2)
  }
)

# 储存结果
dev.copy2pdf(file ="networks_test.pdf", height=10, width=10)
file.show("networks_test.pdf") 

