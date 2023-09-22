library(readr)
data <- read_csv("multivariate.csv")
View(data)

head(data)
help(lm)

lr <- lm(data$Homeowners~data$Immigrant)
lr
summary(lr)$coef

plot(data$Homeowners~data$Immigrant)
help(abline)
abline(lr)
abline(lr, col=2, lwd=3)

install.packages("magrittr")
library(magrittr)

new <-data.frame(Immigrant = c(0,20))
lr %>% predict(new)

abline(lr)
abline(lr, col=3,lwd=3)

attributes(lr)
lr$coefficients

install.packages("ggplot2")
library(ggplot2)

help(mtcars)

data("mtcars")
head(mtcars)
summary(mtcars)

##Creating Plots
plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt,mtcars$mpg)


# Plot using ggplot2
mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point()

# Plot using base R
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue")

qplot(pressure$temperature, pressure$pressure, geom = "line")
qplot(temperature,pressure,data= pressure, geom = "line")
ggplot(pressure, aes(x=temperature, y= pressure))+ geom_line()+geom_point()
ggplot(pressure,aes(x=temperature, y = pressure))+ geom_line()+ geom_point()

##Bar graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))


##Bar graphs of counts
mtcars %>% 
  qplot(factor(cyl))
  ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()
  
  
## Creating Histograms
  
hist(mtcars$mpg)
hist(mtcars$mpg, breaks =10)
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)
qplot(mpg, data=mtcars, binwidth =4)
ggplot(mtcars, aes(x=mpg))+ geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg))+ geom_histogram(binwidth=5)


###Boxplot

plot(ToothGrowth$supp, ToothGrowth$len)

boxplot(len~supp, data=ToothGrowth)
boxplot(len~supp+dose, data=ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
ggplot(ToothGrowth, aes(x= supp, y=len))+geom_boxplot()
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y = len))+geom_boxplot()



###Excercise 1

data(diamonds)
head(diamonds)          
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()




###EXERCISE 2


install.packages("gcookbook")
library(gcookbook) 
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD
str(BOD)

ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")

ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")

ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")

library(ggplot2)
cabbage_exp

ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")

library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

# Making Bar Graph of Counts
ggplot(diamonds, aes(x=cut)) +geom_bar() # this is equvalent to using geom_bar(stat="bin)


ggplot(diamonds,aes(x=carat)) + geom_bar()

ggplot(diamonds, aes(x=carat)) + geom_histogram()


ups <- subset(uspopchange, rank(Change)>40)
ups

ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")

ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()

ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") +
  scale_fill_manual(values=c("#224455","#DDCC33"))

csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)


ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
# Narrow Bars
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
# Wider bars, maximum width = 1
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
# Different bar widths
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))
# Making a Sketched Bar Graph
library(gcookbook) # for the dataset
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))


ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
library(ggplot2)
# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)


tophit <- tophitters2001[1:25,] # take top 25 top hitters
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))

ggplot(tophit, aes(x=avg, y=reorder(name,avg))) + geom_point(size=2.5, colour="blue") + 
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = twodash))
# Get the names sorted by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn the name into factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey70")+
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey40") +
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space="free_y")

install.packages("gapminder")
library(gapminder)
library(dplyr)

str(gapminder)
China <- gapminder %>% filter(country == "China") %>% head(10)
View(China)
ggplot(data = China, aes(x=year,y=lifeExp)) + geom_point(color = 'red', size = 3) + xlab('Year') +ylab('Life Expectancy')+ggtitle("Life Expectancy in China")+ theme_bw(base_size = 18)

ggplot(data = gapminder, aes(x= year, y=lifeExp, group =country,color =continent)) + geom_line() +xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")+theme_bw()
ggplot(data = gapminder, aes(x= year, y=lifeExp, group=country,color=continent))+geom_line()+theme_bw()+facet_wrap(~continent)+ xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")

ggplot(data = China, aes(x=year,y=gdpPercap))+geom_line()+scale_y_log10(breaks=c(1000,2000,3000,4000,5000),labels=scales::dollar)+xlim(1940,2010)+theme_gray(base_size = 20)

