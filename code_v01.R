#install.packages("openxlsx")
library("openxlsx")
library("ggplot2")
#install.packages("tidyverse")
library("tidyr")
library("dplyr")
#install.packages("reshape")
library("reshape")
#data <- openXL("input.xlsx")
#install.packages("gridExtra")
library(gridExtra)
setwd("C:/Users/Imii/OneDrive - Károli Gáspár Református Egyetem/2. félév/Statisztikai Programozás")
data <- read.xlsx("data3.xlsx", sheet = 1)
data

#vagy
data <- read.xlsx("https://github.com/kovacsvlagy/R/blob/master/data3.xlsx", sheet = 1)
data

for( i in 1:length(data[,1])){
  data[i, 1] <- gsub(";.*", "", data[i, 1])
}

for(i in 3:18){
  data[,i] <- as.numeric(data[,i])
}
data

alltyp$Country
newhead <- enc2native(colnames(alltyp))
newhead
years <- c("2000", "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")

alltyp <- data[data[,2] %in% "All types" == TRUE, ]
alltyp

alltyp[1,3:18]

length(alltyp[,1])

atlag <- NULL
for( i in 1:length(alltyp[,1])){
  atlag <- c(atlag, mean(unlist(alltyp[i,3:18]), na.rm=TRUE))
}


#Átlag italfogyasztás
atlag2 <- NULL
atlag2 <- data.frame(atlag , Country=alltyp[,1], key="atlag")

#növekvő sorrendben
atlag2[order(atlag2$atlag),]

#2000 és 2015 között átlagban legtöbb alkoholt fogyasztó 11 ország
top11 <- NULL
top11<- tail(atlag2[order(atlag2$atlag),], 11)
top11
top11$Country
position <- top11$Country
position

#BarChart
abra1 <- ggplot(data=top11, aes(x=Country, y=atlag, fill=Country)) +
  geom_bar(colour="black", stat="identity") +
   scale_x_discrete(limits = position) +
    geom_text(aes(label = sprintf("%.02f",atlag)),position = position_stack(vjust = 0.5))+
       ggtitle("Top 11 alcohol consuption in average 2000-2015")+
  labs(y="Liters of pure alcohol", x="Country")+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))
abra1

#BarChart by type
data

which(data == "Hungary")
which(data == top11[1,2])

magyar <- data[data$Country == 'Hungary',]
magyar2 <- melt()

mean(magyar2[41,4], magyar2[49,4])

magyar2 <- melt(magyar, id = c("Country", "Type"))
magyar2 <- filter(magyar2, Type!="Other alcoholic beverages")
magyar2 

#Hányzó adatok helyettesítése
magyar2[45,4] <- (magyar2[41,4]+magyar2[49,4])/2
magyar2[46,4] <- (magyar2[42,4]+magyar2[50,4])/2
magyar2[47,4] <- (magyar2[43,4]+magyar2[51,4])/2
magyar2[48,4] <- (magyar2[44,4]+magyar2[52,4])/2

#Magyarországi fogyasztás 2000 és 2015 között
abra2 <- ggplot(data = magyar2 , aes(x=variable, y=value, group=Type)) + geom_line()+
  scale_x_discrete(limits = years)+
  geom_line(aes(col=Type), size = 0.72) + 
  geom_point(aes(col=Type))+
  ggtitle("Alcohol, recorded per capita (15+) consumption in Hungary")+
  labs(y="Liters of pure alcohol", x="Year")+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18))+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))
abra2

position
#Észtországi fogyasztás 2000 és 2015 között
Estonia <- data[data$Country == 'Estonia',]
Estonia

Estonia2 <- melt(Estonia, id = c("Country", "Type"))
abra3 <- ggplot(data = Estonia2 , aes(x=variable, y=value, group=Type)) + geom_line()+
  scale_x_discrete(limits = years)+
  geom_line(aes(col=Type), size = 0.72) + 
  geom_point(aes(col=Type))+
  ggtitle("Alcohol, recorded per capita (15+) consumption in Estonia")+
  labs(y="Liters of pure alcohol", x="Year")+ 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18))+
  scale_color_hue(labels = c("All types", "Beer", "Other", "Spirits", "Wine"))+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))

abra3

#Top11
top11_2 <-data[data$Country %in% position,]
top11_2
top11_3 <- melt(top11_2, id = c("Country", "Type"))
top11_4 <- filter(top11_3, variable=="2015", Type!="All types")
top11_4
top11_2015 <- filter(top11_3, variable=="2015", Type=="All types")
top11_2015


#megoszlás a top10 fogyasztó esetében
abra4 <- ggplot(data = top11_4,aes(x=Country, y=value, fill=Type))+ geom_bar( stat = "identity") +
  geom_bar(colour="black", stat="identity") +
  scale_x_discrete(limits = position) +
  geom_text(aes(label = sprintf("%.02f",value)),position = position_stack(vjust = 0.5))+
  ggtitle("Alcohol consuption in 2015 by type")+
  labs(y="Liters of pure alcohol", x="Country")+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))
abra4

ggplot(data = top11_4,aes(x=Country, y=value, fill=Type))+ geom_bar( stat = "identity") +
  geom_bar(colour="black", stat="identity") +
  scale_x_discrete(limits = position) +
  geom_text(aes(label = sprintf("%.02f",value)),position = position_stack(vjust = 0.5))+
  ggtitle("Alcohol consuption in 2015 by type")+
  labs(y="Liters of pure alcohol", x="Country")


+
  scale_color_hue(labels = c("All types", "Beer", "Other", "Spirits", "Wine"))+
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))

#legnagyobb csökkenés és növekedés
valt <- NULL
valt <- data.frame(valtozas=data[,18]-data[,3] , Country=data[,1], key="valt", Type=data[,2])
valt

valtsor <- valt[order(valt$valtozas),]
valtsorall <- filter(valtsor, Type=="All types")
valtsorall
valtnov5 <- head(valtsorall,5)
valtnov5[,2]

tail(valtsorall,21)
valtcsok5 <- valtsorall[171:175,2]
valtcsok5
head(valt[order(valt$valtozas),],5)

valtsor

atlag2[order(atlag2$atlag),]

valt2 <- sort(valt)
valt2

#top11 átlag
ggplot(data=top11, aes(x=Country, y=atlag, fill=Country)) +
  geom_bar(colour="black", stat="identity") +
  scale_x_discrete(limits = position) +
  geom_text(aes(label = sprintf("%.02f",atlag)),position = position_stack(vjust = 0.5))


data[1:100,1]

atlag[,2] <- data[1:100,1]
atlag2 <- c(atlag, )

meltall <- melt(alltyp, id = c("Country", "Type"))
meltall

ggplot(data = meltall , aes(x=variable, y=value, group=Country)) + geom_line()

#mindenki
ggplot(meltall, aes(x=variable, y=value, group=Country)) +
  geom_line() + 
  geom_point() +
  ggtitle("Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)") +
  labs(y="Liter of pure alcohol", x="Year") +
  theme(legend.position="none") +
  scale_color_gradient(low="lightblue", high="darkblue")+
  scale_x_discrete(limits = years)

#top10 line

meltall2 <- meltall[meltall$Country %in% position,]

abra5 <- ggplot(meltall2, aes(x=variable, y=value, group=Country)) +
  geom_line(aes(col=Country), size = 0.72) + 
  geom_point(aes(col=Country)) +
  ggtitle("Top 11 alcohol consuption in 2000-2015") +
  labs(y="Liters of pure alcohol", x="Year") +
  theme(legend.title = element_blank()) +
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))+
  scale_x_discrete(limits = years)+
  scale_color_manual(values = 
                       c("Angola"="chocolate1",
                         "Austria"="cornflowerblue",
                         "Czechia"="cyan4",
                         "Equatorial Guinea"="darkgoldenrod1",
                         "Estonia"="darkgray",
                         "Georgia"="darkolivegreen",
                         "Estonia"="darkorange",
                         "Ireland"="darkorchid",
                         "Luxembourg"="darkred",
                         "Portugal"="darkseagreen4",
                         "Hungary"="darkslategray3",
                         "Belarus"="chocolate4",
                         "Croatia"="darkviolet",
                         "France"="deeppink3",
                         "Lithuania"="deepskyblue4"))

abra5

#top5 növekedés
meltall3 <- meltall[meltall$Country %in% valtnov5[,2],]
meltall3

abra6 <- ggplot(meltall3, aes(x=variable, y=value, group=Country)) +
  geom_line(aes(col=Country), size = 0.72) + 
  geom_point(aes(col=Country)) +
  ggtitle("Highest increase in alcohol consumption") +
  labs(y="Liters of pure alcohol", x="Year") +
  theme(legend.title = element_blank()) +
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))+
  scale_x_discrete(limits = years)+
  scale_color_manual(values = 
                       c("Angola"="chocolate1",
                         "Austria"="cornflowerblue",
                         "Czechia"="cyan4",
                         "Equatorial Guinea"="darkgoldenrod1",
                         "Estonia"="darkgray",
                         "Georgia"="darkolivegreen",
                         "Estonia"="darkorange",
                         "Ireland"="darkorchid",
                         "Luxembourg"="darkred",
                         "Portugal"="darkseagreen4",
                         "Hungary"="darkslategray3",
                         "Belarus"="chocolate4",
                         "Croatia"="darkviolet",
                         "France"="deeppink3",
                         "Lithuania"="deepskyblue4"))


abra6

#top5 csökkenés
valtcsok5

meltall4 <- meltall[meltall$Country %in% valtcsok5,]
meltall4

abra7 <- ggplot(meltall4, aes(x=variable, y=value, group=Country)) +
  geom_line(aes(col=Country), size = 0.72) + 
  geom_point(aes(col=Country)) +
  ggtitle("Alcohol, recorded per capita (15+) consumption") +
  labs(y="Liters of pure alcohol", x="Year") +
  theme(legend.title = element_blank()) +
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))+
  scale_x_discrete(limits = years)
abra7

#top5 csökkenés és növekedés együtt
valtcsok5
valtnov5

valt5 <- c(valtcsok5, valtnov5[,2])
valt5

meltall5 <- meltall[meltall$Country %in% valt5,]
meltall5

ggplot(meltall5, aes(x=variable, y=value, group=Country)) +
  geom_line(aes(col=Country), size = 0.72) + 
  geom_point(aes(col=Country)) +
  ggtitle("Alcohol, recorded per capita (15+) consumption") +
  labs(y="Liters of pure alcohol", x="Year") +
  theme(legend.title = element_blank()) +
  theme(legend.direction = "horizontal", legend.position = "bottom",
        axis.text.x = element_text(angle = 75, vjust = 0.5, face = "bold", color = "black"),
        axis.text.y = element_text(color = "black", face = "bold"),
        axis.title = element_text(face="bold"))+
  scale_x_discrete(limits = years)

top11_2 <-data[data$Country %in% position,]
top11_2



abra1
abra2
abra3
abra4
abra5
abra6
abra7

plotos<-grid.arrange(abra1,abra5,abra6,abra4,abra3,abra2,ncol=3,nrow=2, top= textGrob("Alcohol Consumption Trends"))
ggsave("plotos.png")

w1 <- grid.arrange(a3, a2, d, f, ncol=2, top= textGrob("Analyze Global Suicide Trends",gp=gpar(fontsize=20,font=3)),
                   bottom="Data Source:\nhttps://www.kaggle.com/russellyates88/suicide-rates-overview-1985-to-2016/ ")
