rm(list=ls())
library(ggplot2)
library(gganimate)
theme_set(theme_bw())
library(gapminder)
head(gapminder,20)
#head(gapminder, 15)
tail(gapminder)
dim(gapminder)
1704/12
table(gapminder$country)
tab
str(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d(0.5) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p


p + transition_time(year) +
  labs(title = "Year: {frame_time}")



p + facet_wrap(~continent) +
  transition_time(year) +
  labs(title = "Year: {frame_time}")
#------------------------my codes for specific continent
str(gapminder)
head(gapminder)
gapminder[1500:1704,]
table(gapminder$country)

table(gapminder$country)
p1<- ggplot(
  gapminder[which(gapminder$continent =="Asia"),], #continent =="Asia" 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = T, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p1

p1 + transition_time(year) +
  labs(title = "Year: {frame_time}")
#another dataset
rm(list=ls())
data("airquality")
head(airquality)
dim(airquality)
q <- ggplot(
  airquality,
  aes(Day, Temp, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
q

q + geom_point() +
  transition_reveal(Day)
#--------------------
q1 <- ggplot(
  airquality,
  aes(Day, Solar.R, group = Month, color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Solar") +
  theme(legend.position = "top")
q1

q1 + geom_point() +
  transition_reveal(Day)
#------------------------transmition=month
q2 <- ggplot(
  airquality,
  aes(Month, Temp,  color = factor(Month))
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Day of Month", y = "Temperature") +
  theme(legend.position = "top")
q2

q2 + geom_point() +
  transition_reveal(Month)


#---------------------------------------




#------------------------my codes for comparing  specific countries
str(gapminder)
head(gapminder)
gapminder[1500:1704,]
head(gapminder)

p1 <- ggplot(
  gapminder[which(gapminder$country==c("Iran", "Turkey")),],
  aes(x = gdpPercap, y=lifeExp, type=country,  colour = country)
) +
  geom_line(show.legend = T, alpha =30.5) +
  scale_color_viridis_d() +
  scale_size(range = c(18, 28)) +
  scale_x_log10(10) +
  labs(x = "GDP per capita", y = "Life expectancy")
p1
p1 + geom_point() +
  transition_reveal(year)




library(tibble)

tibble(
  x = 1:5, 
  y = c(1:2, 10:12), 
  z = x ^ 2 + y,
  w=z+8,
  m=LETTERS[1:5],
  n=c("bs","bs","phd","phd","phd")
)

dd=tibble(
  c = 1:50,
  year=2001:2050,
  gdp = c(1:20,6:35 ),
  life = c ^ 2 + gdp,
  pop=c+gdp^2
) 
dd
dd[,1:3]
#c=as.factor(c,dd)
p <- ggplot(
  dd, 
  aes(x = gdp, y=life, size = pop, colour = as.factor(c))
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(12, 24)) +
  scale_x_log10() +
  labs(x = "GDP ", y = "Life ")
p



p + transition_time(year) +
  labs(title = "Year: {frame_time}")



p1 <- ggplot(
  dd,
  aes(gdp, life)
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "gdp", y = "Life") +
  theme(legend.position = "top")
p1

p1 + geom_point() +
  transition_reveal(year)
#---------------------------


dd=tibble(
  c = 1:50,
  grp=c(rep(1,25), rep(2,25)),
  year=2001:2050,
  gdp = c(1:20,11:40 ),
  life = c ^ 2 + gdp,
  pop=c+gdp^2
  
)  ; tail(dd)

table(dd$grp)

p <- ggplot(
  dd,
  aes(gdp, life, size=pop, colour = as.factor(grp))
) +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "gdp", y = "Life") +
  theme(legend.position = "top")
p


p + geom_point() +
  transition_reveal(year)

p + geom_line() +
  transition_reveal(year)

dd=tibble(
  c = 1:50,
  grp=rep(1:5,10),
  year=2001:2050,
  gdp = c(1:20,11:40 ),
  life = c ^ 2 + gdp+rnorm(1, 100,3),
  pop=c+gdp^2+rnorm(1, 10,2)
  
)  ; dd
p <- ggplot(
  dd,
  aes(gdp, life, size=pop, colour=as.factor(grp))
) +
  geom_point() +
  scale_color_viridis_d() +
  labs(x = "gdp", y = "Life") +
  theme(legend.position = "top")
p


#p + geom_point() +
 # transition_reveal(year)

p + geom_line() +
  transition_reveal(year)

