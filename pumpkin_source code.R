#Toolbox project
#Maier Wang
#mw3171

# Introduction:

#Since the Halloween is an important festival in U.S., 
#the production and consumption of pumpkins have a rapid increase during Hallowing season. 
#In this paper, I would like to talk about my R project, 
#which analyzes the topic of pumpkin price during the year from September 24 2016 to September 30 2017. 
#Many states of Unites states have pumpkin planting or production.

#In this paper, for better managing the dataset, 
#I will set the data within 12 main U.S. cities of pumpkin production and consumption, and they are:
  #Atlanta, GA, Baltimore, MD, Boston, MA, Chicago, IL, Columbia, SC, Dallas, TX, Detroit, MI, Los Angeles, CA, New York, NY, Philadelphia, PA, San Francisco, CA; Saint Louis, MO (in alphabetic order).
  #The data set for this case study was intended to answer at least the following research questions:
  #1.	Which city sells the largest pumpkins?
  #2.	Where are pumpkin prices highest?
  #3.	How does pumpkin size relate to price?
  #4.	Which pumpkin variety is the most expensive? Least expensive?
  #5. How does pumpkin price relate to date?
  #The analysis of the above problems will be shown both in text and in graphs.


# Part 1: Dataset

# Attaching necessary packages
library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(ggvis)
library(mdsr)

# Set working directory to where the data saved

mypath="C:/Users/Maier.Wang/Desktop/documents/CU/toolbox/pumpkin_dataset" # Use specific directory to replace "my path".
setwd(mypath)

# Save each csv file as a dataframe when reading data for all 12 cities.
pmk1<- read.csv("atlanta_9-24-2016_9-30-2017.csv")
pmk2<- read.csv("baltimore_9-24-2016_9-30-2017.csv")
pmk3<- read.csv("boston_9-24-2016_9-30-2017.csv")
pmk4<- read.csv("chicago_9-24-2016_9-30-2017.csv")
pmk5<- read.csv("columbia_9-24-2016_9-30-2017.csv")
pmk6<- read.csv("dallas_9-24-2016_9-30-2017.csv")
pmk7<- read.csv("los-angeles_9-24-2016_9-30-2017.csv")
pmk8<- read.csv("detroit_9-24-2016_9-30-2017.csv")
pmk9<- read.csv("new-york_9-24-2016_9-30-2017.csv")
pmk10 <- read.csv("philadelphia_9-24-2016_9-30-2017.csv")
pmk11 <- read.csv("san-fransisco_9-24-2016_9-30-2017.csv")
pmk12<- read.csv("st-louis_9-24-2016_9-30-2017.csv")

# Merge 12 datasets and save as dataframe in pmk_all
pmk_all <- rbind(pmk1,pmk2,pmk3,pmk4,pmk5,pmk6,pmk7,pmk8,pmk9,pmk10,pmk11,pmk12)

# Get familiar with the structure of the new dataframe
head(pmk_all)
str(pmk_all)

# Part 2: Clean data:
#1) Select varibles relevant to data analysis
pmk_select <- pmk_all %>%
select(Commodity.Name, City.Name, Type, Package, Variety,	Sub.Variety, Date, Low.Price, High.Price, Mostly.Low, Mostly.High, Origin, Origin.District,	Item.Size, Color)

#2) Organize variable "Package" into similar format and seperate "Package" into number and size
pmk_select$Package[which(pmk_select$Package == "1 1/9 bushel cartons")]="50 lb cartons"
pmk_select$Package[which(pmk_select$Package == "1 1/9 bushel crates")]="50 lb cartons"
pmk_select$Package[which(pmk_select$Package == "bushel cartons")]="40 lb cartons"
pmk_select$Package[which(pmk_select$Package == "1/2 bushel cartons")]="22 lb cartons"
pmk_select$Package[which(pmk_select$Package == "bushel baskets")]="40 lb cartons"
unique(pmk_select$Package)
pmk_select<-pmk_select %>% separate(Package, into = c("package","package_size","package_size2")," ")
pmk_select$package[which(pmk_select$package=="each")]=1
pmk_select$package[which(pmk_select$package=="bins")]=0

pmk_select$package<-as.numeric(pmk_select$package)

str(pmk_select$package)


#3) Transfer "Item.Size" into a numerical variable
pmk_select$Item.Size <- factor(pmk_select$Item.Size, levels = c("sml", "med", "med-lge", "lge", "xlge", "jbo", "exjbo"), labels = c(1:7))
pmk_select$Item.Size<-as.numeric(pmk_select$Item.Size)


#4) Transfer "Date" into a Date variable, use a "%m/%d/%Y" format
pmk_select$Date <- as.Date(pmk_select$Date,"%m/%d/%Y")


#5) Select numerical variables only
pmk_num<- select(pmk_select,package, Date, Low.Price, High.Price, Mostly.Low, Mostly.High, Item.Size)
attach(pmk_num)
pairs(pmk_num) #graph 1.
#From the pairs graphs, we can see that most variables don't have strong correclations. 
#The correclations between 4 prices should be ignored.

#6) check if Missing values exist in variables: 
which(is.na(pmk_select$Commodity.Name))
which(is.na(pmk_select$City.Name))
which(is.na(pmk_select$package))
which(is.na(pmk_select$Date)) 
which(is.na(pmk_select$High.Price))
which(is.na(pmk_select$Low.Price))
which(is.na(pmk_select$Origin))
which(is.na(pmk_select$Item.Size))
# Only variable "Item.Size" has missing values.
# replace misisng value in variable "Item.Size" with mean value
pmk_select$Item.Size[which(is.na(pmk_select$Item.Size))]=mean(pmk_select$Item.Size, na.rm=T)

# Part 3: Data Analysis

#1.	Which city sells the largest pumpkins?

 write.csv(pmk_select, file = "pmk_select.csv")

 # draw the size of pumpkins sold with bar graph and facet based on city
  g2 <- ggplot(pmk_select) +
  geom_bar(aes(x = Item.Size, fill = City.Name), 
           position = "dodge", stat = "count") + 
  facet_wrap(~City.Name) +
  ggtitle("Pumpkin size in 12 cities") 
  g2 #graph 2.

mean(pmk_select$Item.Size, na.rm=T)
 # draw the size of pumpkins sold with bar graph and facet based on size.
  g3 <- ggplot(pmk_select) +
  geom_bar(aes(x = City.Name, fill = City.Name), 
           position = "dodge", stat = "count") + 
  facet_wrap(~Item.Size) +
  ggtitle("Pumpkin size in 12 cities ~facet size") 
  g3 #graph 3.
 
  # From the 2 graphs above, Boston and Columbia Has the highest counts in item size 7- Extra jumbo, 
  # and Boston has the highest counts in item size from 5 to 7.
  # then compare the mean item size of cities Boston and Columbia.
  t1 <- mean(pmk_select$Item.Size[pmk_select$City.Name=="BOSTON"])
  t2 <- mean(pmk_select$Item.Size[pmk_select$City.Name=="COLUMBIA"])
  t1>t2

  # Since t1>t2 is true, Boston sells the largest item size in pumpkin.

#2.	Where are pumpkin prices highest?
  
  #Add a new variable "Mean_Price" that equals the average of High.Price and Low.Price.
  pmk_select <- pmk_select %>%
  mutate(Mean_Price=(High.Price+ Low.Price)/2)

 #then draw the graph with date on x and mean price on y 
  g4 <- ggplot(pmk_select) +
    geom_point(aes(x = Date, y=Mean_Price,col=Item.Size)) + 
    facet_wrap(~City.Name) +
    scale_color_gradient(low="yellow", high="red")+
    ggtitle("Pumpkin prices in different time of a year ~facet cities") 
  g4 #graph 4.
  
  #The graph above shows that pumpkin price is rarely higher than 400.
  #Use which function to find the cities where pumpkin price were higher than 400.
  pmk_select$City.Name[which(pmk_select$Mean_Price>400)]
  #the result of the above code indicates that all 4 pumpkin prices higher than 400 were sold in SAN FRANCISCO
  #Therefore, SAN FRANCISCO was the city with highest pumpkin price.

#3.	How does pumpkin size relate to price?
  
  # plot pumpkin size on x and price on y, with simple linear regression model
  g5 <-  ggplot(pmk_select, aes(x =Item.Size , y = Mean_Price)) + 
  geom_point(alpha = 0.6, size = 2, position = 'jitter',aes(color=Variety)) +
    stat_smooth(method=lm,se=F)+
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Variety', reverse = T,
                                          override.aes = list(alpha = 1, size = 2)))+
  ggtitle('Pumpkin size VS. Mean_price') 
  g5 #graph 5.
  
  # Measuring the Strength of the Fit
  mod_size<-lm(Mean_Price~Item.Size,data=pmk_select)
  coef(mod_size)
  rsquared(mod_size) #calculate r^2
  
  #Since r^2 equals 0.11, the fit of this model is not very strong.
  #The reason could be that the pumpkin price was also impacted by other varibales, such as variaty and date.
  #Add one categorical/binary explanatory variable "Variety"
  mod_size2 <- lm(Mean_Price~Item.Size+Variety,data=pmk_select) 
  coef(mod_size2)
  rsquared(mod_size2) #calculate r^2
  g6 <- plotModel(mod_size2, system = "ggplot2") 
  g6 #graph 6.
  #The new r^2 is 0.366, closer to 1 than the previous r^2.
  # This means that the new model is more fit than the previous model.
  
#4.	Which pumpkin variety is the most expensive? Least expensive?
  #plot pumpkin variety on x and price on y. Use the code bellow:
  g7 <- ggplot(pmk_select, aes(x =Variety, y = Mean_Price)) + 
  geom_point(alpha = 0.6, size = 2,aes(col=Variety)) +
  scale_color_brewer(type = 'div',palette='RdYlGn',
  guide = guide_legend(title = 'Variety', reverse = T,
  override.aes = list(alpha = 1, size = 2))) +
  ggtitle('Pumpkin variety VS. mean_price') 
  g7 #graph 7.

  # The graph above shows that the higest price were sold in type of "Pie type."
  # Since several types had price around 0. Find the minimum priced pumpkin and search for its type with which function.
  min(pmk_select$Mean_Price)
  pmk_select$Variety[which(pmk_select$Mean_Price==0.24)]
  # The least price was in type of "FAIRYTALE".
  
#5. How does pumpkin price relate to date?
  attach(pmk_select)
  plot(Date,Mean_Price) #graph 8.
  # From the graph, we can see that most sales were happened aound October and November. 
  # The sales were very rare from January to August and the price was stable during that period. 
  # When it comes to September, the sales started to increase and the price became more various.
  
  