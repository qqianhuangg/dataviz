#create a proportion graph for media composition: body image and media meta-analysis
install.packages(c("openxlsx","readxl"))
install.packages("treemap")
install.packages("gganimate")
install.packages("babynames")
install.packages("hrbrthemes")
install.packages("gapminder")
install.packages("gifski")
install.packages("dplyr")
install.packages("viridis")
library(viridis)
library(gifski)
library(hrbrthemes)
library(babynames)
library(openxlsx)
library(readxl)
library(treemap)
library(gganimate)
library(gapminder)
library(dplyr)

#GDP per capita 3d animation
myPlot<-ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
 
   # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
animate(myPlot, duration = 5, fps = 20, width = 700, height = 700, renderer = gifski_renderer())
anim_save("output.gif")

#baby names animation
names(babynames)
is.data.frame(babynames)
str(babynames)
babynames$name<-as.factor(babynames$name)
is.factor(babynames$name)
is.factor(babynames$sex)
babynames$sex<-as.factor(babynames$sex)


# Keep only 3 names
don <- babynames %>%  
  filter(sex ==  "F") %>%
  filter(name %in% c("Ashley", "Patricia", "Helen")) 

names(babynames)


# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(year)+
  scale_color_viridis(discrete = TRUE) 

# Save at gif:
anim_save("287-smooth-animation-with-tweenr.gif")

# Load dataset from github

data <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda", "Jessica", "Patricia", "Linda", "Deborah", "Dorothy", "Betty", "Helen")) %>%
  filter(sex=="F")



# line plot = spaghetti chart
data %>%
  ggplot( aes(x= year, y= n, group= name, color= name)) +
  geom_line() +
  ggtitle("Popularity of American names in the previous 30 years")


data %>%
  ggplot( aes(x=year, y=n, group=name, color=name)) +
  geom_line() +
  geom_point() +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  ylab("Number of babies born") +
  transition_reveal(year)+
  scale_color_viridis(discrete = TRUE) 





