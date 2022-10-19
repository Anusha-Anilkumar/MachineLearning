#Loading packages
library(recipes)
library(caret)
library(tidyverse)
library(ggplot2)
library(graphics)
library(magrittr)
library(mlbench)
library(scatterplot3d)
library(Hmisc)
library(gtsummary)
library(ggcorrplot)
library(dplyr)
library(factoextra)
library(corrplot)
library(PerformanceAnalytics)
library(lubridate)
library(readr)
library(stringr)
library(plyr)
library(flexclust)
library(NbClust)
library(mlbench)
library(caret)
library(factoextra)
library(janitor)
library(clusterSim)
library(mets)
library(cluster)
library(amap)

# *******************************************
# Step 1 – Data collection
# *******************************************
#Read the data
Df <- read.csv("marketing_campaign.csv")
View(Df)

#To know the no of observations
Df %>%
  dplyr::summarise(observations = n())

#To examine the dataframe

dim(Df) #dimension of the df
str(Df) #datatype of the df
summary(Df) 
colnames(Df)#columns of the df
sapply(Df, class)
# The head() and tail() we can adjust the number of rows to 10 using the "n = " argument
head(Df, n = 10)
tail(Df, n = 10)

# *******************************************
# Step 2 – Data preparation
# *******************************************
#Checking for missing values
sum(is.na(Df))

#Checking for missing values by variable
sapply(Df, function(x) sum(is.na(x)))

#option 1 Delete the rows with missing values(found only in the income column)
#Df <- na.omit(Df) 

#option 2 replace the missing values with zero.
#Df$Income[is.na(Df$Income)] <- 0
#dim(Df)

#option 3 replace the missing values with mean of income.
mean_income <- mean(Df$Income, na.rm = TRUE)
Df$Income[is.na(Df$Income)] <- mean_income
dim(Df)

#Create a new dataframe to prevent modifying the original dataframe
newdata <- Df

#Replacing the strings of education to a proper classification name
newdata$New_Education = str_replace_all(newdata$Education,
                                        c("2n Cycle" = "Undergraduate", 
                                          "Basic" = "Undergraduate", 
                                          "Graduation" = "Graduate", 
                                          "Master" = "Postgraduate",  
                                          "PhD" = "Postgraduate"))

newdata$Living_with <- str_replace_all(newdata$Marital_Status,
                                       c("Absurd" = "Single",
                                         "Alone" = "Single",
                                         "Divorced" = "Single",
                                         "Married" = "Partner",
                                         "Single" = "Single",
                                         "Together" = "Partner",
                                         "Widow" = "Single",
                                         "YOLO" = "Single"))

data(newdata)
str(newdata)

#Convert categorical variables into factors
newdata <-newdata %>% mutate(New_Education = factor(New_Education),
                             Living_with = factor(Living_with),
                             Dt_Customer = factor(Dt_Customer))


#confirm if factors
is.factor(newdata$New_Education)
is.factor(newdata$Living_with)
is.factor(newdata$Dt_Customer)

# ********************************
# Feature construction
# ********************************
#To know which customers have been registered the longest in the company to know whether the reason they bought the most products. by benchmarking the all customers with the maximum date in the list 
#to get Newest and oldest dates
newdata$Dt_Customer_new <- strptime(as.character(newdata$Dt_Customer), "%d-%m-%Y")

newdata$Dt_Customer_new <-as.Date(newdata$Dt_Customer_new)
min(newdata$Dt_Customer_new, na.rm = TRUE)
max(newdata$Dt_Customer_new, na.rm = TRUE)

Max_dates <- max(newdata$Dt_Customer_new, na.rm = TRUE)

newdata$shopping_length <- Max_dates - newdata$Dt_Customer_new

#Create a column called Age 
newdata$Age <- 2021 -newdata$Year_Birth
newdata$Age

#Create a new column to show amount spent by each customer across various items
newdata$amountspent <- rowSums(newdata[,c("MntWines","MntFruits", "MntMeatProducts", "MntFishProducts", "MntSweetProducts", "MntGoldProds")])

summary(newdata$amountspent)
count(newdata$amountspent)

#Create a new column to show total number of children in a household
newdata$Children <- rowSums(newdata[,c("Kidhome","Teenhome")])
#levels of the dataset
levels(newdata$New_Education)
levels(newdata$Marital_Status)

#encode the column to be able to add it to teens and kids then get family size
newdata$EncodeMarital_Status <- factor(newdata$Marital_Status, level = c("Absurd","Alone","Divorced","Married","Single","Together","Widow","YOLO"),labels = c(1,1,1,2,1,2,1,1))

sapply(newdata$Teenhome, class)
sapply(newdata$EncodeMarital_Status, class)
sapply(newdata$Kidhome, class)


#convert to integer to be be able to add columns and get size.
newdata$EncodeMarital_Status <-as.integer(newdata$EncodeMarital_Status)

#Create new column to show the family size
newdata$familysize <- rowSums(newdata[,c("EncodeMarital_Status", "Kidhome","Teenhome")])

newdata$Is_parent <- newdata$Children

for(i in 1:length(newdata$Children)){
  if(newdata$Children[i] == 0) 
  {
    newdata$Is_parent[i] = 0
  }
  else
  {
    newdata$Is_parent[i] = 1
  }
}


glimpse(newdata)

# *******************************************
# Step 3 – Data explorartion
# *******************************************
#Data visualisation

#FEW GENERAL STATISTICS & EDA

#Age in ascending order, checking for outliers
max(newdata$Age)
min(newdata$Age)
count(newdata$Age) 

#boxplot to check for outliers in age
newdata %>% ggplot(aes(x =Age, y = ..count.., )) + geom_histogram(color = "black", fill = "blue") +geom_density(alpha =.2, fill = "#FF6666") 
bins = 30
labs(title = "Age boxplot")

#removing the age outliers because it is not practicable for people aged above 100 to visit the supermarket
newdata <- newdata[newdata$Age <=100, ]
max(newdata$Age, na.rm = TRUE)

#Average age of customers is 52 years
mean(newdata$Age)

#how much customers in general spend on average 
mean(newdata$amountspent)
#____________________________________________________________________
#breaking amount spent down into bins to know the range and percentage of customer spending
max(newdata$amountspent)
min(newdata$amountspent)
#bins of 500
#set up cut-off values 
breaks <- c(0,500,1000,1500,2000,2500, 3000)
#specify interval/bin labels
tags <- c("[0-500)",  "[500-1000)","[1000-1500)","[1500-2000)","[2000-2500)","[2500-3000)")
#bucketing values into bins
group_tags <- cut(newdata$amountspent,
                  breaks = breaks,
                  include.lowest =TRUE,
                  right = FALSE,
                  labels = tags)
#inspect bins and get the range, adjustable
summary(group_tags)
#plot bins
ggplot(data= as_tibble(group_tags), mapping = aes(x = value))+ 
  geom_bar(fill ="blue", colour = "white", alpha = 0.7) + 
  stat_count(geom ="text", aes(label = sprintf(" %.4f",..count../length(group_tags))), vjust =0.5) +
  labs(x ='amount spent in percentage bins')+
  theme_minimal()


#-------------------------------------------------------------------------------------------------
#breaking income down into bins to know the range and percentage of customer income(if we were to classify the customers into low, average and high income earners)
max(newdata$Income)
min(newdata$Income)
#bins 
#set up cut-off values 
breaks <- c(0,20000,80000,7000000)
#specify interval/bin labels
tags <- c("[0-20000)",  "[20001-80000)","[80001-700000)")
#bucketing values into bins
group_tags <- cut(newdata$Income,
                  breaks = breaks,
                  include.lowest =TRUE,
                  right = FALSE,
                  labels = tags)
#inspect bins and get the range, adjustable
summary(group_tags)
#plot bins
ggplot(data= as_tibble(group_tags), mapping = aes(x = value))+ 
  geom_bar(fill ="#774D8E", colour = "white", alpha = 0.7) + 
  stat_count(geom ="text", aes(label = sprintf(" %.4f",..count../length(group_tags))), vjust =0.5) +
  labs(x ='income in percentage bins')+
  theme_minimal()

#____________________________________________________________________
#boxplot of all the products(colums 10:15)
par(mfrow=c(1,6))
par(bg ='white')
boxplot(newdata[ ,10], main = names(newdata)[10])
boxplot(newdata[ ,11], main = names(newdata)[11])
boxplot(newdata[ ,12], main = names(newdata)[12])
boxplot(newdata[ ,13], main = names(newdata)[13])
boxplot(newdata[ ,14], main = names(newdata)[14])
boxplot(newdata[ ,15], main = names(newdata)[15])


#amount spent by shopping length, the amount spent is not larger for older customers
#count the numbers of persons in category 
amount.shop <- newdata %>% 
  tbl_df %>%
  dplyr::select(shopping_length, amountspent)%>% arrange(amountspent)

ggplot(data = amount.shop, 
       mapping = aes(x = shopping_length , y = amountspent)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = F)+
  labs(title = "Amount spent by shopping length")

#____________________________________________________________________
#there is a positive relationship between age and amount spent mostly for graduate and undergraduate
newdata %>%
  ggplot(aes(Age, amountspent
  )) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(method = lm, se = F) + 
  facet_wrap(~New_Education)+
  labs(title = "Amount spent by people")+
  theme_bw()
#____________________________________________________________________
#no of observation of each group 
newdata %>%
  group_by(Is_parent)%>%
  dplyr::summarize(number_rows =n())
#we have more people who are not parents spending more

newdata %>%
  ggplot(aes(Is_parent, amountspent,
             colour = Is_parent)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth() + 
  labs(title = "Amount spent by parents")+ labs(x = "parent datapoints", y = "amountspent")
theme_bw()

#____________________________________________________________________
#preffered mode of purchase 
modeofpurchasedf<-newdata[c(16:19)]
view(modeofpurchasedf)

colMeans(modeofpurchasedf)
#distribution of the number of Web and Store purchases.
Web<- density(modeofpurchasedf$NumWebPurchases)
Store<- density(modeofpurchasedf$NumStorePurchases) 
Deals <-density(modeofpurchasedf$NumDealsPurchases) 
Catalog <-density(modeofpurchasedf$NumCatalogPurchases) 


par(mfrow=c(1,4))
par(bg ='white')
plot(Web, frame = F, col = 'steelblue',
     main ="Density plot of WebPurchases")
polygon(Web, col = "steelblue")

plot(Store, frame = F, col = 'brown1',
     main ="Density plot of Store Purchases")
polygon(Store, col = "steelblue")


plot(Deals, frame = F, col = 'brown1',
     main ="Density plot of Store Purchases")
polygon(Deals, col = "steelblue")


plot(Catalog, frame = F, col = 'brown1',
     main ="Density plot of Store Purchases")
polygon(Catalog, col = "steelblue")

amount_by_modeofpurchase <- as.table(colSums(modeofpurchasedf))

amount_by_modeofpurchase


# #____________________________________________________________________
#Education visualisation
#____________________________________________________________________
#count the numbers of persons in New_Education categories  
newdata %>%
  group_by(New_Education)%>%
  dplyr::summarize(number_rows =n())

# the amount of persons in New_Education categories  
newdata %>%
  dplyr::select(New_Education, amountspent)%>%
  group_by(New_Education)%>%
  dplyr::summarise(sum(amountspent, na.rm =TRUE))

#average amount spent in each category
AverageEduspend <- aggregate(amountspent ~New_Education, newdata, mean)
#Barchart of average amount spent by education level
ggplot(AverageEduspend, aes(x = New_Education, y = amountspent))+
  geom_bar(
    aes(color = amountspent, fill =amountspent),
    stat = "identity", position = position_stack()
  )


Average_Income <- aggregate(Income ~Marital_Status, newdata, mean)
#Barchart of average Income by marital status 
ggplot(Average_Income , aes(x = Marital_Status, y = Income))+
  geom_bar(
    aes(fill = Marital_Status),
    stat = "identity", position = position_stack()
  )


Average_Maritalstatus <- aggregate(Income ~EncodeMarital_Status, newdata, mean)




Education_Spending <- aggregate(amountspent ~New_Education, newdata, mean)
Education_Spending

aggregate(newdata$amountspent, list(newdata$New_Education), FUN = sum)
#piechart
Piechart1 <- Education_Spending%>%
  mutate(per = amountspent/sum(amountspent))%>%
  arrange(desc(New_Education))
Piechart1$label <- scales::percent(Piechart1$per)
#Plot
ggplot(data = Piechart1)+
  geom_bar(aes(x="", y = per, fill= New_Education), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x =1, y = cumsum(per)- per/2, label = label)) +
  labs(title = "Average amount spent v/s Education level")+
  theme(plot.title = element_text(hjust =0.5)) 


#products data
products <- dplyr::select(newdata,
                          c(10:15))%>%
  group_by(newdata$New_Education)
view(products)

#amount spent on products across all Education levels
Education_and_products<- aggregate(.~products$`newdata$New_Education`,data =products, FUN =mean)
#transpose the matrix to plot graph
Education_and_products<- t(as.matrix(Education_and_products[c(2:7)]))
#add column names back
colnames(Education_and_products) <- c('Graduate', 'Postgraduate', 'Undergraduate')
print(Education_and_products)
colours = c("#DCD0FF","#B57EDC","#774D8E","#B53389","#8E3179","#8D029B")
# the use of ylim to give 30% space for legend
barplot(Education_and_products, main ='Education level and amount spent across products', ylab='products', beside = TRUE, col = colours, ylim= c(0, max(Education_and_products)*1.3))
options(scipen=999)
#to add a box around the plot
box()
# add a legend
legend('topright', fill =colours, legend = c('Wines', 'Fruits', 'Meat','Fish', 'Sweet', 'Gold'))


#-------------
#mode purchased grouped by education level 
mode_purchased <- dplyr::select(newdata,
                                c(16:19))%>%
  group_by(newdata$New_Education)
view(mode_purchased)


#number of items purchased on means of purchase across all Education levels
Education_and_means_of_purchase <- aggregate(.~mode_purchased$`newdata$New_Education`,data =mode_purchased, FUN =mean)
#transpose the matrix to plot graph
Education_and_means_of_purchase<- t(as.matrix(Education_and_means_of_purchase[c(2:5)]))
#add column names back
colnames(Education_and_means_of_purchase) <- c('Graduate', 'Postgraduate', 'Undergraduate')
print(Education_and_means_of_purchase)
colours = c("#DCD0FF","#B57EDC","#774D8E","#B53389")
# the use of ylim to give 30% space for legend
barplot(Education_and_means_of_purchase, main ='Number of items purchased and means of purchase across all education levels', ylab='mode of purchase', beside = TRUE, col = colours, ylim= c(0, max(Education_and_means_of_purchase)*1.5))
options(scipen=999)
#to add a box around the plot
box()
# add a legend
legend('topright', fill =colours, legend = c('Deals', 'Web', 'Catalog','Store'),cex = 0.8)

#____________________________________________________________________
#living with visualisation
#____________________________________________________________________
#no of people in each living with category
newdata %>%
  group_by(Living_with)%>%
  dplyr::summarize(number_rows =n())

#sum of amount spent in each category   
newdata %>%
  dplyr::select(Living_with, amountspent)%>%
  group_by(Living_with)%>%
  dplyr::summarise(sum(amountspent, na.rm =TRUE))

#average amount spent in each category
AverageLivingspend <- aggregate(amountspent ~Living_with, newdata, mean)

#Barchart of average amount spent by education level
ggplot(AverageLivingspend, aes(x = Living_with, y = amountspent))+
  geom_bar(
    aes(color = amountspent, fill =amountspent),
    stat = "identity", position = position_stack()
  )


#piechart
#create a new variable
Livingwith_Spending <- aggregate(amountspent ~Living_with, newdata, sum)
Livingwith_Spending


Piechart2 <- Livingwith_Spending%>%
  mutate(per = amountspent/sum(amountspent))%>%
  arrange(desc(Living_with))
Piechart2$label <- scales::percent(Piechart2$per)
#Plot
ggplot(data = Piechart2)+
  geom_bar(aes(x="", y = per, fill= Living_with), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x =1, y = cumsum(per)- per/2, label = label)) +
  ggtitle("pie chart sum Living _with amount spent")


#products data
products2 <- dplyr::select(newdata,
                           c(10:15))%>%
  group_by(newdata$Living_with)
view(products2)

#amount spent on products across Partner and single
Livingwith_and_products<- aggregate(.~products2$`newdata$Living_with`,data =products2, FUN =sum)
#transpose the matrix to plot graph
Livingwith_and_products<- t(as.matrix(Livingwith_and_products[c(2:7)]))
#add column names back
colnames(Livingwith_and_products) <- c('Partner', 'Single')
print(Livingwith_and_products)
colours = c("red","blue","yellow","green","purple","pink")
# the use of ylim to give 30% space for legend
barplot(Livingwith_and_products, main ='Living_with and amount spent across products', ylab='products', beside = TRUE, col = colours, ylim= c(0, max(Livingwith_and_products)*1.3))
options(scipen=999)
#to add a box around the plot
box()
# add a legend
legend('topright', fill =colours, legend = c('Wines', 'Fruits', 'Meat','Fish', 'Sweet', 'Gold'))

#____________________________________________________________________
#Marital status Exploration
#____________________________________________________________________
#no of people in each Marital status category
newdata %>%
  group_by(Marital_Status)%>%
  dplyr::summarize(number_rows =n())

#sum of amount spent in each category   
newdata %>%
  dplyr::select(Marital_Status, amountspent)%>%
  group_by(Marital_Status)%>%
  dplyr::summarise(sum(amountspent, na.rm =TRUE))

#average amount spent in each category
AverageMaritalstatus <- aggregate(amountspent ~Marital_Status, newdata, mean)

#Barchart of average amount spent by Marital status category
ggplot(AverageMaritalstatus, aes(x = Marital_Status, y = amountspent))+
  geom_bar(
    aes(color = amountspent, fill =amountspent),
    stat = "identity", position = position_stack()
  )

newdata <-newdata %>% mutate(Marital_Status = factor(Marital_Status))

#piechart
#create a new variable, average amount spent in marital status category
Maritalstatus_Spending <- aggregate(amountspent ~Marital_Status, newdata, mean)
Maritalstatus_Spending


Piechart3 <- Maritalstatus_Spending%>%
  mutate(per = amountspent/sum(amountspent))%>%
  arrange(desc(Marital_Status))
Piechart3$label <- scales::percent(Piechart3$per)
#Plot
ggplot(data = Piechart3)+
  geom_bar(aes(x="", y = per, fill= Marital_Status), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x =1, y = cumsum(per)- per/2, label = label)) +
  labs(title = "Average amount spent v/s Marital Status")+
  theme(plot.title = element_text(hjust =0.5)) 


#ggtitle("Average amount spent v/s Marital Status",(hjust = 1))

#products data
products3 <-dplyr::select(newdata,
                          c(10:15))%>%
  group_by(newdata$Marital_Status)
view(products3)

#amount spent on products across Marital status categories
Maritalstatus_and_products <- aggregate(.~products3$`newdata$Marital_Status`,data =products3, FUN =sum)
#transpose the matrix to plot graph
Maritalstatus_and_products<- t(as.matrix(Maritalstatus_and_products[c(2:7)]))
#add column names back
colnames(Maritalstatus_and_products) <- c('Absurd', 'Alone','Divorced','Married','Single','Together','Widow', 'YOLO')
print(Maritalstatus_and_products)
colours = c("red","blue","yellow","green","purple","pink")
# the use of ylim to give 30% space for legend
barplot(Maritalstatus_and_products, main ='Marital status and amount spent across products', ylab='products', beside = TRUE, col = colours, ylim= c(0, max(Maritalstatus_and_products)*1.3))
options(scipen=999)
#to add a box around the plot
box()
# add a legend
legend('topright', fill =colours, legend = c('Wines', 'Fruits', 'Meat','Fish', 'Sweet', 'Gold'))


#____________________________________________________________________
#Is_parent visualisation
#____________________________________________________________________

Isparent_income <- aggregate(Income ~Is_parent, newdata, mean)
Isparent_income

Piechart4 <- Isparent_income%>%
  mutate(per = Income/sum(Income))%>%
  arrange(desc(Is_parent))
Piechart4$label <- scales::percent(Piechart4$per)
#Plot
ggplot(data = Piechart4)+
  geom_bar(aes(x="", y = per, fill= Is_parent), stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(x =1, y = cumsum(per)- per/2, label = label)) +
  labs(title = "Income v/s Is_Parent")+
  theme(plot.title = element_text(hjust =0.5)) 

#____________________________________________________________________
#now lets see for mode of purchase

#WEB 
newdata%>%
  ggplot(aes(New_Education, NumWebPurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and web purchases for is parent")+
  facet_wrap(~Is_parent)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")


newdata%>%
  ggplot(aes(New_Education, NumWebPurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and web purchases for living with")+
  facet_wrap(~Living_with)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")


newdata%>%
  ggplot(aes(New_Education, NumWebPurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and web purchases for family size")+
  facet_wrap(~familysize)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")

#Store
newdata%>%
  ggplot(aes(New_Education, NumStorePurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and Catalog for is parent")+
  facet_wrap(~Is_parent)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")

newdata%>%
  ggplot(aes(New_Education, NumStorePurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and Catalog for living with")+
  facet_wrap(~Living_with)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")


newdata%>%
  ggplot(aes(New_Education, NumStorePurchases,fill =New_Education))+
  geom_bar(position = "dodge",
           stat = "identity")+
  ggtitle("Education and Catalog for family size")+
  facet_wrap(~familysize)+
  theme_classic()+
  theme(legend.position ="none")+
  xlab("")


#Extra exploration
#MntWines 

plot(newdata$MntWines, pch=19, ylab = "Amount on Wines", col = "#055888", main="Scatterplot-Amount on Wines") 

boxplot(newdata$MntWines, main="Boxplot-Amount on Wines", 
        
        xlab="Amount spent on Wines", ylab="Amount", col="orange", horizontal = TRUE) 


#MntFruits 

plot(newdata$MntFruits, pch=19, ylab = "Amount on Fruits", col = "#055888", main="Scatterplot-Amount on Fruits") 

boxplot(newdata$MntFruits, main="Boxplot-Amount on Fruits", 
        
        xlab="Amount spent on Fruits", ylab="Amount", col="orange", horizontal = TRUE) 


#MntMeatProducts 

plot(newdata$MntMeatProducts, pch=19, ylab = "Amount - Meat Products", col = "#055888", main="Scatterplot-Amount on Meat Products") 

boxplot(newdata$MntMeatProducts, main="Boxplot-Amount on Meat Products", 
        
        xlab="Amount spent on Meat Products", ylab="Amount", col="orange", horizontal = TRUE) 


#MntFishProducts 

plot(newdata$MntFishProducts, pch=19, ylab = "Amount on Fish Products", col = "#055888", main="Scatterplot-Amount on Fish Products") 

boxplot(newdata$MntFishProducts, main="Boxplot-Amount on Fish Products", 
        
        xlab="Amount spent on Fish Products", ylab="Amount", col="orange", horizontal = TRUE) 


#MntSweetProducts 

plot(newdata$MntSweetProducts, pch=19, ylab = "Amount on Sweet Products", col = "#055888", main="Scatterplot-Amount on Sweet Products") 

boxplot(newdata$MntSweetProducts, main="Boxplot-Amount on Sweet Products", 
        
        xlab="Amount spent on Sweet Products", ylab="Amount", col="orange", horizontal = TRUE) 



##Histograms for "Amount Spent wrt to New Education and Marital_Status" 

#-------------------------------------------------------------------------------------------------- 


ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=amountspent))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntWines))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntFruits))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntMeatProducts))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntFishProducts))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntSweetProducts))+geom_bar(position='dodge', stat='identity') 

ggplot(newdata, aes(x=New_Education, y=Marital_Status, fill=MntGoldProds))+geom_bar(position='dodge', stat='identity') 



#Income vs NumWebPurchases, NumStorePurchases, NumCatalogPurchases, NumWebVisitsMonth

plot(newdata$Income, newdata$NumWebPurchases, xlab="Income", ylab="NumWebPurchases", main="Income vs NumWebPurchases", pch=19, col="#3173B2")
plot(newdata$Income, newdata$NumStorePurchases, xlab="Income", ylab="NumStorePurchases", main="Income vs NumStorePurchases", pch=19, col="#3173B2")
plot(newdata$Income, newdata$NumCatalogPurchases, xlab="Income", ylab="NumCatalogPurchases", main="Income vs NumCatalogPurchases", pch=19, col="#3173B2")
plot(newdata$Income, newdata$NumWebVisitsMonth, xlab="Income", ylab="NumWebVisitsMonth", main="Income vs NumWebVisitsMonth", pch=19, col="#3173B2")


#Familysize vs NumWebPurchases, NumStorePurchases, NumCatalogPurchases, NumWebVisitsMonth

plot(newdata$familysize, newdata$NumWebPurchases, xlab="Family Size", ylab="NumWebPurchases", main="Family Size vs NumWebPurchases", pch=19, col="#3173B2")
plot(newdata$familysize, newdata$NumStorePurchases, xlab="Family Size", ylab="NumStorePurchases", main="Family Size vs NumStorePurchases", pch=19, col="#3173B2")
plot(newdata$familysize, newdata$NumCatalogPurchases, xlab="Family Size", ylab="NumCatalogPurchases", main="Family Size vs NumCatalogPurchases", pch=19, col="#3173B2")
plot(newdata$familysize, newdata$NumWebVisitsMonth, xlab="Family Size", ylab="NumWebVisitsMonth", main="Family Size vs NumWebVisitsMonth", pch=19, col="#3173B2")
ggplot(newdata, aes(fill=amountspent, y=New_Education, x=Living_with)) +
  geom_bar(position='dodge', stat='identity')


#correlation 
#Scaling the data in order to perform correlation
newdata_norm <- scale(newdata[,c("Age","Income","Children","Recency","MntWines","MntFruits","MntMeatProducts", "MntFishProducts","MntSweetProducts","MntGoldProds","NumDealsPurchases","NumWebPurchases","NumCatalogPurchases","NumStorePurchases","NumWebVisitsMonth","AcceptedCmp3","AcceptedCmp4","AcceptedCmp5","AcceptedCmp1","AcceptedCmp2","Complain","Response","amountspent","familysize","Is_parent")]) %>% as_tibble()

#Correlation matrix
correlationMatrix <- cor(newdata_norm[,1:25])
print(correlationMatrix)

#correlation plot
res <- cor(newdata_norm)
corrplot(res, type = "upper",
         tl.col = "black", tl.srt = 45)

#Correlation between all variables
chart.Correlation(newdata_norm, histogram=TRUE, pch=25)

#Correlation plot using heatmap
res2 <- rcorr(as.matrix(newdata_norm))
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

#Checking for multicollinearity
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
print(highlyCorrelated)

#Since column 3(Children) and 23 (amountspent) are highly correlated, we need to remove one them to make it more efficient

#Feature selection
#dropping columns 
data1= dplyr::select(newdata_norm, -Complain,  -AcceptedCmp1, -AcceptedCmp2, -AcceptedCmp3, -AcceptedCmp4, -AcceptedCmp5, -Response, -Children, -Recency)

#Principal component analysis
# pc <- prcomp(data1,
#              center = TRUE,
#             scale. = TRUE)
# summary(pc)
# 
# fviz_pca_ind(pc,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
#              )
# 
# screeplot(pc, type = "l", npcs = 10, main = "Screeplot of the first 10 PCs")
# abline(h = 1, col="red", lty=5)
# legend("topright", legend=c("Eigenvalue = 1"),
#        col=c("red"), lty=5, cex=0.6)
# 
# cumpro <- cumsum(pc$sdev^2 / sum(pc$sdev^2))
# plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
# abline(v = 10, col="blue", lty=5)
# abline(h = 0.88802, col="blue", lty=5)
# legend("topleft", legend=c("Cut-off @ PC10"),
#        col=c("blue"), lty=5, cex=0.6)
# 
# pca_data <- as.data.frame(-pc$x[,1:3])

# *******************************************
# Step 4 – Cluster modelling
# *******************************************
#Hierarchical clustering

#To find optimal value of K for Hierarchical clustering(Elbow method)
fviz_nbclust(data1, hcut, method = "wss") +
  labs(subtitle = "Elbow method")

#To find optimal value of K for Hierarchical clustering(Silhouette method)
fviz_nbclust(data1, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#To find distance using euclidean method
dist <- dist(data1, 
             method = "euclidean")

#Run the algorithm and store the result to an object
hc <- hclust(dist, method = "ward.D2")
hc

#4 clusters
plot(hc) #plot the dendogram
rect.hclust(hc, k = 4, border = "red") #draw rectangles highlighting the clusters
hc4 <- cutree(hc, k = 4) # create a four-cluster solution
table(hc4)
data1 %>% # take the std data
  mutate(hc4 = factor(hc4)) %>% # add the cluster assignment
  group_by(hc4) %>% # group by cluster
  dplyr::mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the prop per group 
  print(width = Inf)
hc4_flex <- as.kcca(hc, data1, k = 4)
par(mfrow=c(1,1))
barchart(hc4_flex)
table(hc4, clusters(hc4_flex))

#3 clusters
plot(hc)
rect.hclust(hc, k = 3, border = "red")
hc3 <- cutree(hc, k = 3)
data1 %>% # take the std data
  mutate(hc3 = factor(hc3)) %>% # add the cluster assignment
  group_by(hc3) %>% # group by cluster
  dplyr::mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the prop per group 
  print(width = Inf) # print all columns
hc3_flex <- as.kcca(hc, data1, k = 3)
par(mfrow=c(1,1))
barchart(hc3_flex)
table(hc3, clusters(hc3_flex))

#2 clusters
plot(hc)
rect.hclust(hc, k = 2, border = "red")
hc2 <- cutree(hc, k = 2)
data1 %>% # take the std data
  mutate(hc2 = factor(hc2)) %>% # add the cluster assignment
  group_by(hc2) %>% # group by cluster
  dplyr::mutate(n = n()) %>% # calculate the n per group 
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  mutate(prop = n/sum(n)) %>% # calculate the prop per group 
  print(width = Inf) # print all columns
hc2_flex <- as.kcca(hc, data1, k = 2)
barchart(hc2_flex)
table(hc2, clusters(hc2_flex))

#K-means clustering
#To find optimal value of K for k-means(Elbow method)
fviz_nbclust(data1, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

#To find optimal value of K for k-means(Silhouette method)
fviz_nbclust(data1, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#To find optimal value of K for k-means(Gap static method)
#fviz_nbclust(data1, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
#labs(subtitle = "Gap statistic method")


#To find optimal value of k for K-means(Hubert and D index method)
#NbClust(data = data1[,1:5],
#min.nc = 2, max.nc = 15, index = "all", method = "kmeans")$Best.ncBest

#K-means algorithm(K=4)
set.seed(123)
km4 <- kmeans(data1, 4, nstart = 25)
km4

#Cluster plot(K=3)
fviz_cluster(km4, data = data1,
             palette = c("#2E9FDF", "#00AFBB","#E7B800","#d02edf"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#K-means algorithm(K=3)
set.seed(123)
km3 <- kmeans(data1, 3, nstart = 25)
km3

#Cluster plot(K=3)
fviz_cluster(km3, data = data1,
             palette = c("#2E9FDF", "#00AFBB","#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

#K-means algorithm(K=2)
set.seed(123)
km2 <- kmeans(data1, 2, nstart = 25)
km2

#Custer plot(K=2)
fviz_cluster(km2, data = data1,
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
# *******************************************  
# Step 5 – Evaluating model performance
# *******************************************

#To determine the hierarchical clustering performance using Silhouette method
Silhouette_score_hc4 <- silhouette(hc4, dist(data1))
mean(Silhouette_score_hc4[, 3])

Silhouette_score_hc3 <- silhouette(hc3, dist(data1))
mean(Silhouette_score_hc3[, 3])

Silhouette_score_hc2 <- silhouette(hc2, dist(data1))
mean(Silhouette_score_hc2[, 3])

#To determine the kmeans performance using Silhouette method
Silhouette_score_km4 <- silhouette(km4$cluster, dist(data1))
mean(Silhouette_score_km4[, 3])

Silhouette_score_km3 <- silhouette(km3$cluster, dist(data1))
mean(Silhouette_score_km3[, 3])

Silhouette_score_km2 <- silhouette(km2$cluster, dist(data1))
mean(Silhouette_score_km2[, 3])

# *******************************************
# Step 6 – Performance improvement
# *******************************************
# Optimal cluster number
# ____________________________________________________________________
# Evaluating clustering results for multiple centres - Function approach
# ____________________________________________________________________
Silhouette_Score <- function(k){
  Clustered_Data_wd_multiple_cen <- kmeans(data1, centers = k,
                                           nstart=25)
  sils <- silhouette(Clustered_Data_wd_multiple_cen$cluster,
                     dist(data1))
  mean(sils[, 3])
}

k <- 2:10
avg_sil <- sapply(k, Silhouette_Score)
par(mfrow=c(1,1))
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=TRUE)

#To find silhouette score for different values of k
#sil_score <- sil.score(data1, nb.clus = c(2:10), nb.run = 100, iter.max = 1000,
#method = "manhattan")

#Add hc2 and hc3 clusters value to the dataframe
hc3 <- factor(hc3, 
              levels = c(1, 2, 3),
              labels = c("High income shoppers HC", "Low income non-shoppers HC", "Average income deals shoppers HC"))

hc2 <- factor(hc2, 
              levels = c(1, 2),
              labels = c("High income shoppers HC", "Low income non-shoppers HC"))

newdata <- newdata %>% mutate(hc2 = hc2)
newdata <- newdata %>% mutate(hc3 = hc3)

#Targeting the segments (Hierarchical clustering)
#Education
newdata %>%
  tabyl(hc3, New_Education) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

#Marital status
newdata %>%
  tabyl(hc3, Living_with) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

#Add km2 and km3 cluster values to the dataframe
km2 <- factor(
  km2$cluster,
  levels = c(1, 2),
  labels = c("Low income non-shoppers KM", "High income shoppers KM"))

newdata <- newdata %>% mutate(km2 = km2)

km3 <- factor(
  km3$cluster,
  levels = c(1, 2, 3),
  labels = c("Low income non-shoppers KM", "High income shoppers KM", "Average income deals shoppers KM"))

newdata <- newdata %>% mutate(km3 = km3)

#Targeting the segments (K-means clustering)
#Education
newdata %>%
  tabyl(km3, New_Education) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

#Marital status
newdata %>%
  tabyl(km3, Living_with) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

#Comparing HC with K-means
newdata %>%
  tabyl(km2, hc2) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

newdata %>%
  tabyl(km3, hc3) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()