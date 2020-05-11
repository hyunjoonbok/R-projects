### 1. Correlation ###

options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)
theme_set(theme_bw())  # pre-set the bw theme.
midwest <- read.csv("http://goo.gl/G1K41K")  # bkup data source
data("midwest", package = "ggplot2")

# Basic plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Area Vs Population", 
       y="Population", 
       x="Area", 
       title="Scatterplot", 
       caption = "Source: midwest")

plot(gg)

# Jitter Scatterplot -> shows all the points without hiding
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg, aes(cty, hwy))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")

# Bubble Chart -> good for one categorical and one numerical 

data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

mpg_select <- mpg[mpg$manufacturer %in% c("audi", "ford", "honda", "hyundai"), ]

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(mpg_select, aes(displ, cty)) + 
  labs(subtitle="mpg: Displacement vs City Mileage",
       title="Bubble chart")

g + geom_jitter(aes(col=manufacturer, size=hwy)) + 
  geom_smooth(aes(col=manufacturer), method="lm", se=F)


# Animated Bubble Chart
devtools::install_github("dgrtwo/gganimate")
require(gganimate)
require(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10()  # convert to log scale

gganimate(g, interval=0.2)


## Correlogram
devtools::install_github("kassambara/ggcorrplot")
require(reshape2)
require(ggcorrplot)

# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           method="square", 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)



### 2. Deviation ###
# Data Prep
data("mtcars")  # load data
mtcars$carname <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$carname <- factor(mtcars$carname, levels = mtcars$carname)  # convert to factor to retain sorted order in plot.

## Diverging Lollipop Chart

ggplot(mtcars,aes(x=carname,y=mpg_z, label=mpg_z)) +
  geom_point(stat = "identity", fill="red", size=5) +
  geom_segment(aes(y=0,x = carname, xend=carname, yend=mpg_z), color= 'red') + 
  geom_text(color="white", size =2) +
  ylim(-2.5, 2.5) +
  coord_flip()

# 3. Ranking
## Ordered Bar Chart

# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make","mileage")
cty_mpg <- cty_mpg[order(cty_mpg$mileage),]
cty_mpg <- factor(cty_mpg$make,levels = cty_mpg$make)
# Draw plot
ggplot(cty_mpg, aes(x=make, y=mileage)) +
  geom_bar(stat='identity', fill="green") 

## Lollipop Chart
ggplot(cty_mpg, aes(x=make, y=mileage)) +
  geom_point(size=3) + 
  geom_segment(aes(y=0, xend=make, yend=mileage))

