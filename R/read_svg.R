read_svg = function(file){
  # 读取SVG文件
  svg_data <- rsvg::rsvg(file, height = 600)

  # 提取坐标信息
  coords <- svg_data$polyline$points

  # 转换为数据框
  df <- data.frame(x = coords[, 1], y = coords[, 2])

  # 使用ggplot2画出坐标
  ggplot(df, aes(x, y)) +
    geom_path() +
    theme_minimal()

}
