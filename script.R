source('R/analysis.R')

########
# DATA #
########
data       <-  read.csv('data/gapminder-FiveYearData.csv', stringsAsFactors = FALSE)
data_1982  <-  data[data$year == 1982, ]

############
# ANALYSIS #
############
library(lme4)
model  <-  lme4::lmer(log(lifeExp) ~ log(gdpPercap) + (1 + log(gdpPercap) | year), data = data)
out    <-  coef(model)

########
# PLOT #
########
color_table  <-  c(Asia = 'tomato', Europe = 'chocolate4', Africa = 'dodgerblue2', Americas = 'darkgoldenrod1', Oceania = 'green4')
col          <-  category_color(data_1982$continent, color_table)
cex          <-  rescale(sqrt(data_1982$pop), c(0.2, 10))
continents   <-  unique(data_1982$continent)

pdf('./figure1.pdf', width = 7, height = 7)
plot(lifeExp ~ gdpPercap, data_1982, log = 'x', cex = cex, col = col, pch = 21, las = 1, xlab = 'GDP per capita', ylab = 'Life expectancy (years)', main = 1982)
for (i in seq_along(continents)) {
    f(data = data_1982, continent = continents[i], color_table = color_table)
}
dev.off()
