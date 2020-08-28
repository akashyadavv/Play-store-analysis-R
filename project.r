library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


dataset <- read.csv("googleplaystore.csv")
str(dataset)
dataset$Category <- tolower(dataset$Category)

sum(is.na(dataset$Rating))
sum(is.na(dataset$Installs))

dataset <- dataset %>% select(Rating) %>% filter(!is.na(dataset$Rating))
dataset <- dataset %>% select(Reviews) %>% filter(!is.na(dataset$Reviews))

head(dataset['Rating'],25)
# Firstly, we visualize the most installed (one billion) app's categories.


library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
a <- dataset %>% select(Category, Installs) %>% filter(Installs == "1,000,000,000+") %>% 
group_by(Category) %>% arrange(Category)

ggplot(a, aes(x= Installs, fill = Category)) + geom_bar(position = "dodge") + coord_flip()

# Respectively, communication, social and game apps are the most installed by over one billion people.



# How many apps included in each category ?
c <- dataset %>% group_by(Category) %>%
  summarize(Count = n()) %>% arrange(desc(Count))
  
c <- head(c, 10)

ggplot(c, aes(x = Category, y = Count)) +
  geom_bar(stat="identity", width=.5,  fill="firebrick4") +
  labs(title = "Top10 Categories") +
   # subtitle = "How many apps included in each category ?") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

dataset <- dataset %>% filter(Installs != "0")
dataset$Installs <- as.factor(dataset$Installs)
options(scipen = 999)

#1
dataset$Installs <- gsub(",", "", dataset$Installs)

#2
dataset$Installs <- as.character(dataset$Installs)
dataset$Installs = substr(dataset$Installs,1,nchar(dataset$Installs)-1)

summary(dataset$Installs)
dataset$Installs <- dataset %>% filter(dataset$Installs != "NA")
x<-filter(is.na(dataset$Installs))
x
#3
dataset$Installs <- as.numeric(dataset$Installs)


dataset %>% filter(Type == "Paid") %>% group_by(Category) %>% summarize(totalInstalls = sum(Installs)) %>%
  arrange(desc(totalInstalls)) %>% head(10) %>% ggplot(aes(x = Category, y = totalInstalls)) +
  geom_bar(stat="identity", width=.5,  fill="forestgreen") + labs(title= "Top10 Paid Categories" ) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


income<- sum(as.numeric(filter(dataset,dataset$Price!="0")$Price)*as.numeric(substr(filter(dataset,dataset$Price!=0)$Installs,1,nchar(filter(dataset,dataset$Price!=0)$Installs)-1)))
income



dataset %>% filter(!is.na(Rating), !is.na(Installs)) %>% ggplot(mapping = aes(x = log(Installs), y = Rating))
 + geom_smooth(method = "lm") + xlab("Logarithm of the installations") + 
 ggtitle("Relationship between the logarithm of the number of installations and the rating received")

It turns out, more installations do help in higher ratings,
although the confidence intervals significantly broaden as an app is installed more.
I had to take the logarithm to slightly help in normalizing the data, 
as some apps had a significantly higher amount of installations.

a<-"abcdefgh"
  x<- substr(a,2,nchar(a))
  nchar(x)
  x

class(a)
y<-"1234"
class(y)
z<-as.numeric(y)
class(z)


### Exploring the ratings given to the apps, categorically
dataset %>%  group_by(Category) %>%  filter(!is.na(Rating), Category!='1.9') %>% 
summarise(meanRating = mean(Rating)) %>% ggplot(mapping = aes(x = Category, y = meanRating)) + 
geom_col(aes(fill = Category)) + geom_line(group = 1) +  coord_flip() + 
ggtitle("Average rating across categories") + ylab("Average rating") + guides(fill=FALSE)

dataset$Category