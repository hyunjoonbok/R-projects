##==== GGplot2 codes ====##

require(ggplot2)
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut))
ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut, shape=color))

ggplot(diamonds) + geom_point(aes(x=carat, y=price, color=cut)) + labs(main="Scatterplot", x="Carat", y="price")

gg <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) + geom_point() + labs(title="Scatterplot", x="Carat", y="Price")

gg + facet_wrap( ~ cut, ncol=3)
gg + facet_grid(color ~ cut)
library(gridExtra)
grid.arrange()




options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
data("midwest", package = "ggplot2")  # load the data
midwest <- read.csv("http://goo.gl/G1K41K") # alt source 
# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal))

ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()

#add a smoothing layer using geom_smooth(method='lm')
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method = "lm")

# Adjusting limits
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point() + geom_smooth(method = "lm")
g + xlim(c(0,0.1)) + ylim(c(0,1000000))

# Change the Title and Axis Labels

g + labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# So the full function call would be
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change the Color and Size of Points
ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(col="steelblue" ,size=4) +  # Set static color and size for points
  geom_smooth(method="lm", col="red") + # change the color of line
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")

# Change the Color To Reflect Categories in Another Column
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state), size=3) +  # Set color to vary based on state categories.
  geom_smooth(method="lm", col="firebrick", size=2) + 
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area Vs Population", subtitle="From midwest dataset", y="Population", x="Area", caption="Midwest Demographics")
gg

gg + scale_colour_brewer(palette = "Set1")  # change color palette


# Write Customized Texts for Axis Labels
gg + scale_x_continuous(breaks=seq(0, 0.1, 0.01), labels = sprintf("%1.2f%%", seq(0, 0.1, 0.01))) + 
  scale_y_continuous(breaks=seq(0, 1000000, 200000), labels = function(x){paste0(x/1000, 'K')})

gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest"); gg
gg + labs(color = "State", size="Density")



gg + scale_color_manual(name="State", 
                        labels = c("Illinois", 
                                   "Indiana", 
                                   "Michigan", 
                                   "Ohio", 
                                   "Wisconsin"), 
                        values = c("IL"="blue", 
                                   "IN"="red", 
                                   "MI"="green", 
                                   "OH"="brown", 
                                   "WI"="orange"))

gg + theme(legend.position="None") + labs(subtitle="No Legend")


# Plot text and label that REPELS eachother (using ggrepel pkg) ------------
midwest_sub <- midwest[midwest$poptotal > 300000, ]
midwest_sub$large_county <- ifelse(midwest_sub$poptotal > 300000, midwest_sub$county, "")

require(ggrepel)
gg + geom_text_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_text_repel") + theme(legend.position = "None")   # text

gg + geom_label_repel(aes(label=large_county), size=2, data=midwest_sub) + labs(subtitle="With ggrepel::geom_label_repel") + theme(legend.position = "None")   # label

# Flip the X and Y axis -------------------------------------------------
gg + coord_flip()


# Draw multiple plots within one figure


data(mpg, package="ggplot2")  # load data
mpg <- read.csv("http://goo.gl/uEeRGu")

g <- ggplot(mpg, aes(x=displ, y=hwy)) + 
  geom_point() + 
  labs(title="hwy vs displ", caption = "Source: mpg") +
  geom_smooth(method="lm", se=FALSE) + 
  theme_bw()  # apply bw theme 
g

# Facet wrap with common scales
g + facet_wrap( ~ class) + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure")  # Shared scales

# Facet wrap with free scales
g + facet_wrap( ~ class, scales = "free") + labs(title="hwy vs displ", caption = "Source: mpg", subtitle="Ggplot2 - Faceting - Multiple plots in one figure with free scales")  # Scales free