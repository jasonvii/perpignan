library(lme4)

source('R/analysis.R')
source('R/figures.R')

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

makeFigure1(data_1982, output_filename = 'output/figure1.pdf', main = '1982')
makeFigure1(data, output_filename = 'output/figure2.pdf', main = 'whole time series')
