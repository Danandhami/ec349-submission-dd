#rm(list=ls())

###loading in the datas
##firstly the RDA files
#this file was too large to load in on my computer, so took the smaller Rda
setwd("C:/Users/u2103494/OneDrive - University of Warwick/ec349")
load("yelp_review_small (1).Rda")

#View(review_data_small)
#View(user_data_small)
##now the JSON files
require(jsonlite)
businessdata <- stream_in(file("yelp_academic_dataset_business.json"))
#View(businessdata)
#summary(businessdata)
na_count <- colSums(is.na(businessdata))
rowno <- nrow(businessdata)
na_prop <- na_count / rowno
print(na_prop)
rm(na_count, na_prop, rowno)

checkindata <- stream_in(file("yelp_academic_dataset_checkin.json"))
#View(checkindata)
tipdata <- stream_in(file("yelp_academic_dataset_tip.json"))
#View(tipdata)
userdata <- stream_in(file("yelp_academic_dataset_user.json"))
#View(userdata)
require(dplyr)
require(stringr)
###start of data cleaning
##taking just the variables which intuitively may be useful in user data
minuserdata <- select(userdata,user_id,review_count,yelping_since,useful,funny,cool,fans,average_stars,elite,friends)
minuserdata <- minuserdata %>%
  #changing the yelping_since to a numerical variable representing year
  mutate(startyear=substr(yelping_since,1,4))%>%
  mutate(startyear=as.numeric(startyear))%>%
  #removing the comma in "20,20" in elite, to allow count of commas (for years elite)
  #firstly removing the instances with 2021 after
  #generating a binary variable that takes value 1 if elite is not na
  mutate(elite=ifelse(elite=="",NA,elite))%>%
  mutate(elitedummy=ifelse(!is.na(elite),1,0))%>%
  mutate(elite=str_replace(elite,"20,20,2021","2020,2021"))%>%
  mutate(elite=str_replace(elite,"20,20","2020"))%>%
  mutate(elite=str_replace(elite,"20202021","2020,2021"))%>%
  mutate(yearselite=ifelse(elitedummy ==1, str_count(elite,",")+1,0))%>%
  #count number of commas in elite to give years a person is elite
  #counting number of friends
  mutate(friendsno=str_count(friends,","))%>%
  mutate(userfunny=funny)%>%
  mutate(usercool=cool)%>%
  mutate(useruseful=useful)

minuserdata <- subset(minuserdata, select = -c(yelping_since,elite,friends,useful,funny,cool))
rm(userdata)

checkin1 <- checkindata%>%
  #counting the number of checkins - may be relevant to customer opinion
  mutate(checkincount=str_count(date,",")+1)
checkin1 <- subset(checkin1, select = c(business_id,checkincount))
rm(checkindata)

#matching the number of checkings to the business. Unmatched businesses receive n/a 
minbusinessdata <- left_join(businessdata,checkin1,by = "business_id")
rm(checkin1)
rm(businessdata)

##cleaning the merged business data
#checking the different strings in accepting credit cards, to see if 
# returns "True" "False" and "None"
uniquestrings <- unique(minbusinessdata$attributes$BusinessAcceptsCreditCards)
print(uniquestrings)
rm(uniquestrings)
#now we want to clean the other variables which could be useful, before merging with our user data
#most of the attributes have too high propotions of n/a entries
#accepting credit cards only has 20% n/a so can replace n/a with the modal value (will introduce some bias)

minbusinessdata <- minbusinessdata%>%
  #replacing n/a in check in count with 0 - as no match implies no check in data for that business
  mutate(checkincount=ifelse(is.na(checkincount),0,checkincount))%>%
  mutate(creditcards=ifelse(attributes$BusinessAcceptsCreditCards=="None","False",attributes$BusinessAcceptsCreditCards))%>%
  mutate(creditcards=ifelse(creditcards=="True",1,ifelse(creditcards=="False",0,creditcards)))%>%
  mutate(creditcards=as.numeric(creditcards))%>%
  #after summarising the column, the mean value is 0.949 - so mode of 1 imposed on n/as
  mutate(creditcards=ifelse(is.na(creditcards),1,creditcards))%>%
  #making state categorical
  mutate(state=factor(state))%>%
  mutate(busiavgstar=stars)%>%
  mutate(busireview=review_count)
minbusinessdata1 <- subset(minbusinessdata, select = c(business_id,state,busiavgstar,busireview,checkincount,creditcards))
rm(minbusinessdata)  

#now i have prepared my variables from other datasets, I will focus on the review sata set - assigning a
#polarity score to the text and removing unnecessary variables, before matching my user and business data onto it

#on a separate script I am running a sentiment analysis. It is taking extremely long, so I am anticipating I will need to cut
#down my number of review observations.

rm(tipdata)

#reviewdatawork

revdata <- review_data_small
revdata <-revdata%>%
  #taking the year of review and making numeric, to be compared with the year user started on yelp
  mutate(reviewyear=substr(date,1,4))%>%
  mutate(reviewyear=as.numeric(reviewyear))

rm(review_data_small)

####TEXT ANALYSIS CODE THAT DOESN'T CURRENTLY WORK
#detecting language to remove non-english observations
#library(cld3)
#detect_language<-function(text){
#lang <- cld3::detect_language(text)
#return(lang)
#}
#revdata$language <- sapply(revdata$text,detect_language)
### Filtering out the non english rows
#englishrevdata <-revdata[revdata$language=="en",]
#rm(revdata)
#### REST OF THIS SENTIMENT CODE IS IN ANOTHER SCRIPT


###MAKING REVIEW DATA SMALLER
set.seed(1)
smallerrevdata <-  revdata%>%sample_n(size=350000,replace=FALSE)
smallerrevdata <- select(smallerrevdata,-c(date))

merged1 <- left_join(smallerrevdata,minuserdata,by="user_id")
merged2 <- left_join(merged1,minbusinessdata1,by="business_id")
rm(merged1)
rm(minbusinessdata1)
rm(revdata)
rm(smallerrevdata)
rm(minuserdata)

##making stars categorical, removing elitedummy, getting yelpyears, 
merged2<-merged2%>%
  mutate(stars=as.factor(stars))%>%
  mutate(yelpyears=reviewyear-startyear)

#drop id data and drop startyear, although multicolinearity shouldn't be an issue for random forest
merged2<-select(merged2,-c(review_id,user_id,business_id,elitedummy,startyear))


####FOR NOW - EXCLUDING TEXT - No sentiment analysis intitally.
merged3<-select(merged2,-c(text))
merged4<-merged3

####plots to see variable relationships
require(ggplot2)
require(GGally)

merged4<-na.omit(merged4)


### useful, funny, cool plots for users and reviews
##useful
boxplot_useful_review <- ggplot(merged4, aes(x = factor(stars), y = useful)) +
  geom_boxplot() +
  labs(x = "Stars", y = "Useful Review") +
  ggtitle("Boxplot of Useful Review votes by Star Rating")+
  scale_y_continuous(limits = c(0, 15))

boxplot_useful_user <- ggplot(merged4, aes(x = factor(stars), y = useruseful)) +
  geom_boxplot() +
  labs(x = "Stars", y = "Useful User") +
  ggtitle("Boxplot of Useful User votes by Star Rating")+
  scale_y_continuous(limits = c(0, 60))
require(gridExtra)
grid.arrange(boxplot_useful_review, boxplot_useful_user, ncol = 2)

##funny

boxplot_funny_review <- ggplot(merged4, aes(x = factor(stars), y = funny)) +
  geom_boxplot() +
  labs(x = "Stars", y = "funny Review") +
  ggtitle("Boxplot of funny Review votes by Star Rating")+
  scale_y_continuous(limits = c(0, 5))

boxplot_funny_user <- ggplot(merged4, aes(x = factor(stars), y = userfunny)) +
  geom_boxplot() +
  labs(x = "Stars", y = "funny User") +
  ggtitle("Boxplot of Useful User votes by Star Rating")+
  scale_y_continuous(limits = c(0, 20))
grid.arrange(boxplot_funny_review, boxplot_funny_user, ncol = 2)

##cool 
boxplot_cool_review <- ggplot(merged4, aes(x = factor(stars), y = cool)) +
  geom_boxplot() +
  labs(x = "Stars", y = "cool Review") +
  ggtitle("Boxplot of funny Review votes by Star Rating")+
  scale_y_continuous(limits = c(0, 3))

boxplot_cool_user <- ggplot(merged4, aes(x = factor(stars), y = usercool)) +
  geom_boxplot() +
  labs(x = "Stars", y = "cool User") +
  ggtitle("Boxplot of Useful User votes by Star Rating")+
  scale_y_continuous(limits = c(0, 10))
grid.arrange(boxplot_cool_review, boxplot_cool_user, ncol = 2)

##boxplots for average business stars and average user stars
violin_business <- ggplot(merged4, aes(x = factor(stars), y = busiavgstar)) +
  geom_boxplot() +
  labs(x = "Stars", y = "business averages") +
  ggtitle("Boxplot of business avg by Star Rating")+
  scale_y_continuous(limits = c(0, 5))

violin_user <- ggplot(merged4, aes(x = factor(stars), y = average_stars)) +
  geom_boxplot() +
  labs(x = "Stars", y = "user averages") +
  ggtitle("Boxplot of user avg by Star Rating")+
  scale_y_continuous(limits = c(0, 5))
grid.arrange(violin_business, violin_user, ncol = 2)
#plots for check ins
violin_checkin<-ggplot(merged4, aes(x = factor(stars), y = checkincount)) +
  geom_violin() +
  labs(x = "Stars", y = "check ins") +
  ggtitle("Violin plot checkins by Star Rating")+
  scale_y_continuous(limits = c(0, 500))
boxplot_checkin<-ggplot(merged4, aes(x = factor(stars), y = checkincount)) +
  geom_boxplot() +
  labs(x = "Stars", y = "check ins") +
  ggtitle("Violin plot checkins by Star Rating")+
  scale_y_continuous(limits = c(0, 500))
grid.arrange(violin_checkin,boxplot_checkin, ncol = 2)
###seeing the proportion of review observations in each star category
star_counts <- table(merged4$stars)
percentage <- prop.table(star_counts) * 100  

percentage_df <- data.frame(Stars = names(percentage), Percentage = as.numeric(percentage))


ggplot(percentage_df, aes(x = Stars, y = Percentage)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(x = "Stars", y = "Percentage of Observations") +
  ggtitle("Percentage of Observations in Each Star Category")

#looking at newly constructed variable years elite and years on site
eliteplot <- ggplot(merged4, aes(x = factor(stars), y = yearselite)) +
  geom_boxplot() +
  labs(x = "Stars", y = "years elite") +
  ggtitle("Boxplot of years elite by Star Rating")+
  scale_y_continuous(limits = c(0, 10))
yearsonyelp<-ggplot(merged4, aes(x = factor(stars), y = yelpyears)) +
  geom_boxplot() +
  labs(x = "Stars", y = "years on site") +
  ggtitle("Boxplot of years on site by Star Rating")+
  scale_y_continuous(limits = c(0, 10))
grid.arrange(eliteplot,yearsonyelp, ncol = 2)

#number of reviews
#plot for reviewcount
violin_count<-ggplot(merged3, aes(x = factor(stars), y = review_count)) +
  geom_violin() +
  labs(x = "Stars", y = "count") +
  ggtitle("Violin plot review no. by Star Rating")+
  scale_y_continuous(limits = c(0, 500))
boxplot_count<-ggplot(merged3, aes(x = factor(stars), y = review_count)) +
  geom_boxplot() +
  labs(x = "Stars", y = "count") +
  ggtitle("box plot review no by Star Rating")+
  scale_y_continuous(limits = c(0, 500))
grid.arrange(violin_count,boxplot_count, ncol = 2)



####CLASSIFICATION
#splitting data into training and test
#90% training data, 10% test data
set.seed(230)
index<-sample(1:nrow(merged3),size=0.9*nrow(merged3),replace=FALSE)
trainingdata<-merged3[index,]
testdata<-merged3[-index,]

#now testing different classification methods



#Decision tree, with pruning:
#making a tree on training
require(tree)
set.seed(230)
decisiontree <- tree(stars ~ .,trainingdata)
plot(decisiontree)
text(decisiontree)
#only resulted in two splits, based on user average stars and business
#predicting on test data
decisiontree1prediction <- predict(decisiontree,testdata,type="class")
table(testdata$stars,decisiontree1prediction)
##the poor peformance of a single decision tree suggests few variables were adding
##new information?


na_counttraining<-colSums(is.na(trainingdata))
print(na_counttraining)
na_counttest<-colSums(is.na(testdata))
print(na_counttest)
trainingdata<-na.omit(trainingdata)


#random forest

rm(index)
rm(decisiontree1prediction)
set.seed(230)
require(randomForest)
##with initially 50 trees (to check functions)
#rf1<-randomForest(stars ~ ., data=trainingdata, ntree=50, mtry=5,do.trace=TRUE)
##        OOB estimate of  error rate: 42.63%
#Confusion matrix:
#  1    2    3     4      5 class.error
#1 33013 2207 1401  2885   8727   0.3155516
#2  6982 3209 2100  4619   7626   0.8692126
#3  3358 1537 4294 10237  11641   0.8617826
#4  3135 1661 4692 23740  32636   0.6395603
#5  5954 1687 2893 18305 116460   0.1984804


#with 500 trees
rf1<-randomForest(stars ~ ., data=trainingdata, ntree=500, mtry=5,do.trace=TRUE)
## output from the training
# ntree      OOB      1      2      3      4      5
#    500:  40.64% 28.54% 89.51% 88.72% 63.64% 15.70%
# so clearly, my variables do better in explaining extreme reviews
# compared to more nuanced reviews (2,3,4 stars)
# on which it does poorly
# we can actually observe the forest getting worse at predicting
# the 2 and 3 stars as the number of trees increases
# the OOb error still decreases - can see the third line of the plot
plot(rf1, main="Error Rate vs. Number of Trees, mtry=5", type="l", col="blue")
#we can see that with mtry=5, our OOB error roughly stabilises
#at 300 trees
rf1prediction <- predict(rf1,testdata,type="class")
table(testdata$stars,rf1prediction)


##choosing the mtry - number of paramters allowed in each tree node split
set.seed((230))
oob.err<-rep(0,8)
#test.err-double(8)
for(j in 1:8){
  fit<-randomForest(stars~.,data=trainingdata, ntree=50, mtry=j,do.trace=TRUE) *
    oob.err[j]<-fit$err.rate[50,1]
    #pred-predict(fit,testdata)
    #test.rr[mtry]-with(testdata,mean((medv-pred)^2))
    cat(j," ")
}

plot(1:8, oob.err, type = "b", xlab = "mtry", ylab = "Out-of-bag Error",
     main = "Out-of-Bag Error vs. mtry Values")

#optimal random forest seems to be mtry=3 to 4 and ntree=350 so now running the optimal model
rf2<-randomForest(stars ~ ., data=trainingdata, ntree=350, mtry=4,do.trace=TRUE)
rf2prediction <- predict(rf2,testdata,type="class")
#evaluating prediction
confusionmatrix<-table(actual=testdata$stars,predicted=rf2prediction)
accuracy <- (sum(diag(confusionmatrix)))/(sum(confusionmatrix))
print(accuracy)
#[1] 0.5952571
varImpPlot(rf2)

