

# Author : Romil Godha
# Purpose : Final Poster Project


# Libraries
install.packages("treemap")
install.packages("grDevices")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("dplyr")
install.packages("zoo")
install.packages("knitr")
install.packages("ggplot2")
install.packages("viridis")
install.packages("kableExtra")
install.packages("tidyr")
library(tidyr)
library(viridis)
library(grDevices)
library(treemap)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(zoo)
library(knitr)
library(kableExtra)

# Loading the data
file.name <- file.choose()
data <- read.csv(file.name, header=TRUE,stringsAsFactors = FALSE)
View(data)
typeof(data$AmountInUSD)


# Removing Remarks Column
data$Remarks <- NULL
data$AmountInUSD<- as.numeric(data$AmountInUSD)

data
View(data)

# Plot 1 - 2D Plot - Top Startups Investors in India - Tree Map

# Dataframe for tree map
v=aggregate(data$AmountInUSD, by=list(Investors=data$InvestorsName, City=data$CityLocation), FUN=sum)
v=v[rev(order(v$x)),]
v=v[1:10,]
View(v)

# Data frame for HTML Table
table_v = v
table_v$Amount <- paste(format(round(v$x/ 1e6, 1), trim = TRUE), "M")
table_v$Label <- paste(table_v$Investors,table_v$Amount, sep = ", ")
table_v$City_Num <- NULL

# Treemap Code
treemap(table_v, #Your data frame object
        index= c("Label"),  #A list of your categorical variables
        vSize = "x",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Blues",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Top Investors in India", #Customize your title
        fontsize.title = 18) #Chan


# Donught Chart for Cities from where top investors are from
donut=aggregate(table_v$Amount, by=list(City=table_v$City), FUN=length)
View(donut)
donut$ymax= cumsum(donut$x/sum(donut$x))
donut$ymin= c(0, head(donut$ymax, n=-1))
p1 = ggplot(donut, aes(fill=City, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Cities of Top Investors") +
  labs(title="")
p1


# Plot 2 - 2D Plot - Top Startups in India by Investment - Tree map
# Data frame 
q = aggregate(data$AmountInUSD, by=list(Startups=data$StartupName,City=data$CityLocation), FUN=sum)
q=q[rev(order(q$x)),]
q=q[1:10,]
View(q)

# Data frame 
table_q = q
table_q$Amount <- paste(format(round(q$x/ 1e6, 1), trim = TRUE), "M")
table_q$Label <- paste(table_q$Startups,table_q$Amount, sep = ", ")
View(table_q)

# Tree map code 
treemap(table_q, #Your data frame object
        index="Label",  #A list of your categorical variables
        vSize = "x",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Greens",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Top Startups in India", #Customize your title
fontsize.title = 18)#Change the font size of the title



# Creating a donut chart to understand top cities of startups
donut2=aggregate(table_q$Amount, by=list(City=table_q$City), FUN=length)
View(donut2)
donut2$ymax= cumsum(donut2$x/sum(donut2$x))
donut2$ymin= c(0, head(donut2$ymax, n=-1))
p2 = ggplot(donut2, aes(fill=City, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Cities of Top Startups") +
  labs(title="")
p2

# Plot 3 - 2D Plot - Industries with highest startup amounts - Bar Plot

# Data frame for bar plot
data$IndustryVertical=tolower(data$IndustryVertical)
x=aggregate(data$AmountInUSD, by=list(Industry=data$IndustryVertical), FUN=sum)
x= x[rev(order(x$x)),]
x=x[1:10,]
View(x)

# Kable Table code
table_x = x
table_x$Amount <- paste(format(round(x$x/ 1e6, 1), trim = TRUE), "M")
table_x$x <- NULL 
kable(table_x,"html") %>% kable_styling("striped",full_width=T) %>% 
  column_spec(1:2,bold=T,background="white") %>% 
  row_spec(c(1,2,3,4,5,6,7,8,9,10),bold=F,color="white",background="#009ACD ")


# Bar plot code
x=na.omit(x)
View(x)
options(scipen=999)
op <- par(mar = c(12,7,4,2) + 0.1)
barplot(x$x[1:10]/10000000,names.arg = x$Industry[1:10]
        ,main= "Industries with Highest Startup Funding"
        ,ylab="",col = brewer.pal(9, name = "Greens")
        ,ylim=c(0,1000)
        ,cex.names = 1
        ,cex.lab=1.1,axis.lty = 0,lwd=2,las=2)
pts <- pretty(x$x / 10000000)
pts
axis(2, at = pts, labels = paste(pts, "MM", sep = ""), las = 1)
title(ylab = "Total Funding", line = 5,cex.lab=1.1)



# Plot 4- 2D Plot - Trends of startups by quarter
install.packages("plyr")
library(plyr)
data$Date=as.POSIXct(strptime(data$Date,format="%d/%m/%Y")) # converting into date format 
data$Qtr=as.yearqtr(data$Date,format="%y-Q%q")
temp=ddply(data,.(Qtr),summarise,Investment=sum(AmountInUSD,na.rm=TRUE))
View(temp)

# Creating a KABLE TABLE
table_temp = temp
table_temp$Investment = paste(format(round(table_temp$Investment/ 1e6, 1), trim = TRUE), "M")
View(table_temp)

kable(table_temp,"html") %>% kable_styling("striped",full_width=T) %>% 
  column_spec(1:2,bold=T,background="white") %>% 
  row_spec(c(1,2,3,4,5,6,7,8),bold=F,color="white",background="#A2CD5A ")

# Bar plot
op <- par(mar = c(12,7,4,2) + 0.1)
barplot(temp$Investment/1000000000,names.arg = temp$Qtr,
        ylim= c(0,6)
        ,main= "Startup Funding By Quarter"
        ,ylab="",col = brewer.pal(9, name = "PRGn")
        ,cex.names = 1
        ,cex.lab=1.1,axis.lty = 0,lwd=2,las=2)
title(ylab = "Total Funding", line = 3,cex.lab=1.1)
title(xlab="Quarters", line=5, cex.lab=1.1)




# Plot 1-1D- Percentage of Startups by cities
y= aggregate(data$SNo, by=list(Category=data$CityLocation), FUN=length)
y= y[rev(order(y$x)),]
View(y)

options(scipen=999)
op <- par(mar = c(12,7,4,2) + 0.1)
barplot((y$x[1:10]/sum(y$x[1:10])*100),names.arg = y$Category[1:10],axes=FALSE
        ,main= "Cities with the Most Startups"
        ,ylab="",col = "#7FFF00"
        ,cex.names = 1
        ,cex.lab=1.1,axis.lty = 0,lwd=2,las=2)
pts <- pretty(y$x[1:10]/sum(y$x[1:10])*100)
axis(2, at = pts, labels = paste(pts, "%", sep = ""), las = 1)
title(ylab = "Percentage of Startups", line = 4,cex.lab=1.1)
title(xlab= "Indian Cities",line=6,cex.lab=1.1)
text(labels=y$x, xpd=TRUE)


# Plot 2- 1D  - Word Cloud of Top Startups 

r=aggregate(data$SNo, by=list(Category=data$SubVertical), FUN=length)
r= r[rev(order(r$x)),]

makeWordClouds <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "GnBu"),
            random.color=TRUE) 
}  
makeWordClouds(r["Category"])


# Plot 3 - 1D - Quaterly trend:

data$Qtr=as.yearqtr(data$Date,format="%y-Q%q")
temp=data %>% group_by(Qtr) %>% summarise(count(SNo
                                                )) 
quarter=aggregate(data$SNo, by=list(Quarter=data$Qtr), FUN=length)
View(temp)
ggplot(quarter,aes(Quarter,x,group=1))+geom_line(color="blue",size=1)+
  labs(x="Year and Quarter",y="Total Investment Count",
       title="Investment Trends Over the Quarter")+theme(axis.title=element_text()
  ,axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_yearqtr(format="%Y-Q%q")





