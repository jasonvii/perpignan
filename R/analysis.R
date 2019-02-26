#############
# FUNCTIONS #
#############
rescale  <-  function (x, range) {
    # some useless comments
    p  <-  (x - min(x)) / (max(x) - min(x))
    range[[1]] + p * (range[[2]] - range[[1]])
}

category_color  <-  function (things, table) {
    unname(table[things])
}

trend_line_add  <-  function (x, y, data, ...) {
    lx   <-  log10(data[[x]])
    fit  <-  lm(data[[y]] ~ lx)
    xr   <-  range(lx)
    lines(10^xr, predict(fit, list(lx = xr)), ...)
}

trend_line_continent_add  <-  function (x, y, data, continent, color_table) {
    trend_line_add(x, y, data[data$continent == continent,], col = color_table[continent])
}

f  <-  function (...) {
    trend_line_continent_add(x = 'gdpPercap', y = 'lifeExp', ...)
}
