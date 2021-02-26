table2014<- read.table("/Users/thalassa/Dropbox/PHD2016to2020/PHD2017SUMMER/R programming/files/names-1/yob2014.txt", header=FALSE,
                       sep = ",",
                       col.names=c("name","sex","number"))


str(table2014)
table2014<-as.data.frame(table2014)
is.data.frame(table2014)
nrow(table2014)


#10

#sub2014.10<-which(substring(levels(table2014$name),first=1,last=1)=="G")
length(which(substring(unique(table2014$name), first = 1, last = 1) == "G"))
#624 unique names with G



#11

sub2014.11<-sum(table2014$number[which(substring(table2014$name,first=1,last=1)=="K")])
sub2014.11

#12
sub2014.f<-subset(x=table2014,
                  subset=(sex == "F"),
                  select=c("name","sex","number"))

sub2014.m<-subset(x=table2014,
                  subset=(sex == "M"),
                  select=c("name","sex","number"))

sub2014.12<-merge(sub2014.f,sub2014.m,by="name",all=TRUE)
sub2014<-subset(x=sub2014.12,
                subset=(sex.x=="F" & sex.y=="M"))
length(sub2014$name)

#13
sub2014.13 <- subset(x=table2014,
                     subset=(5000<=number&number<=6000),
                     select=c("name"))
sub2014.13



#14
table2014$proportion<-table2014$number/sum(table2014$number)

vector<-c()
for(i in 1: length(LETTERS))
{
  vector[i]<-sum(table2014$proportion[which(substring(table2014$name,1,1)==LETTERS[i])])
}
vector

vector<-vector*100
names(vector)<-c(LETTERS[1:26])
vector

###test###
ggplot(mtcars, aes(factor(gear), fill=factor(carb))) +
  geom_bar() +
  scale_fill_got(discrete = TRUE, option = "Tully")

ggplot(mtcars, aes(factor(cyl), fill=factor(vs))) +
  geom_bar() +
  scale_fill_got(discrete = TRUE, option = "Daenerys")

ggplot(mtcars, aes(x = mpg, y = disp, colour = hp)) +
  geom_point(size = 2) +
  scale_colour_got(option = "Lannister")

###test ends###


barplot(vector, col = c("blue", "red", "green","orange"), border="white",
        ylim=c(0,20),
        xlim=c(0,30),
        main="Proportion of babies with a name
        starting A,B,C,..., and Z in 2014",
        xlab="LETTERS",
        ylab="Percentage(%)",
        axisnames=TRUE,
        cex.names = 0.7,
        names.arg=c(LETTERS))


text(0.65,15,labels="14.1%",col=c("black"),cex=0.5)
text(2,5.5,labels="4.6%",col=c("black"),cex=0.5)
text(3.3,7.5,labels="7.1%",col=c("black"),cex=0.5)
text(4.5,4.5,labels="4.1%",col=c("black"),cex=0.5)
text(5.5,7.3,labels="6.8%",col=c("black"),cex=0.5)
text(6.7,1.4,labels="0.9%",col=c("black"),cex=0.5)
text(8,3.5,labels="3%",col=c("black"),cex=0.5)
text(9.2,3.3,labels="2.8%",col=c("black"),cex=0.5)
text(10.3,2.6,labels="2.2%",col=c("black"),cex=0.5)
text(11.5,10.5,labels="10%",col=c("black"),cex=0.5)
text(12.7,6.3,labels="5.8%",col=c("black"),cex=0.5)
text(14,7,labels="6.5%",col=c("black"),cex=0.5)
text(15.2,8.5,labels="8.1%",col=c("black"),cex=0.5)
text(16.4,3.6,labels="3.2%",col=c("black"),cex=0.5)
text(17.55,2,labels="1.5%",col=c("black"),cex=0.5)
text(18.7,2.4,labels="1.9%",col=c("black"),cex=0.5)
text(19.9,0.7,labels="0.2%",col=c("black"),cex=0.5)
text(21.2,4.4,labels="3.9%",col=c("black"),cex=0.5)
text(22.4,6.1,labels="5.6%",col=c("black"),cex=0.5)
text(23.6,3.1,labels="2.8%",col=c("black"),cex=0.5)
text(24.8,0.6,labels="0.1%",col=c("black"),cex=0.5)
text(26,1.7,labels="1.2%",col=c("black"),cex=0.5)
text(27.2,1.8,labels="1.3%",col=c("black"),cex=0.5)
text(28.4,0.8,labels="0.3%",col=c("black"),cex=0.5)
text(29.55,1,labels="0.5%",col=c("black"),cex=0.5)
text(30.7,1.9,labels="1.4%",col=c("black"),cex=0.5)

#15
#NOTE: importing path of the files needed in this section might be different on your computer#

setwd("/Users/thalassa/Dropbox/PHD2016to2020/PHD2017SUMMER/R programming/files/names-1")

temp <- list.files(pattern="*.txt")
data<-list()


for (i in 1:length(temp))
{data[[i]] <-read.table(temp[i],
                        header = FALSE,
                        sep = ",",
                        col.names = c("name","sex","number"))}

data[[1]]

year<-c(1880:2014)
year<-as.data.frame(year)


options(digits=4)

chris<-c()

for(x in 1:length(data)){
  chris[x]<-sum(data[[x]]$number[which(data[[x]]$name=="Chris")])/sum(data[[x]]$number)
}
chris

christian<-c()

for(x in 1:length(data)){
  christian[x]<-sum(data[[x]]$number[which(data[[x]]$name=="Christian")])/sum(data[[x]]$number)
}
christian



christopher<-c()

for(x in 1:length(data)){
  christopher[x]<-sum(data[[x]]$number[which(data[[x]]$name=="Christopher")])/sum(data[[x]]$number)
}

christopher


year$chris<-chris
year$chrisian<-christian
year$christopher<-christopher

summary(year)

plot(chris ~ year, 
     data=year,
     type="l",
     lty=1,
     lwd = 3,#width of line
     xlim=c(1880,2015),
     ylim=c(0,0.02),
     main= "Proportion of babies with the names Chris, 
        Christian, and Christopher from 1880 to 2014",
     ylab= "Proportion",
     xlab= "Year",
     cex=0.5,
     col= "green")

points(christian ~ year,
       data=year,
       type="l",
       lty=2,
       lwd = 3,
       col = "red")

points(christopher ~ year,
       data=year,
       type="l",
       lty=3,
       lwd = 3,
       col = "Blue")

legend(x = "topleft",
       legend = c("Chris","Christian","Christopher"),
       lty = c(1,2,3),
       lwd = 4,
       col=c("green","red","blue"),
       cex = 1)