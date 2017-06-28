tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
##Always add this on the text analysis problem
str(tweets)
tweets$Negative = as.factor(tweets$Avg<=-1)

table(tweets$Negative)

install.packages("tm")
library(tm)

install.packages("SnowballC")
library(SnowballC)

corpus = Corpus(VectorSource(tweets$Tweet))

corpus[[1]]$content## they changed corpus to a dataset

corpus = tm_map(corpus, tolower)##lower every wards. 

corpus = tm_map(corpus, removePunctuation) ##remove the punctuation

stopwords("english")[1:10]

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))

corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)

inspect(frequencies [1000:1005,505:515])
findFreqTerms(frequencies, lowfreq = 20)

sparse = removeSparseTerms(frequencies, 0.995)##remove the low frequency word.
##important
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))##make sure all the columns is a name despite the name might have number
##important

tweetsSparse$Negative = tweets$Negative
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = subset(tweetsSparse, split ==TRUE)
testSparse = subset(tweetsSparse, split ==FALSE)
library(rpart.plot)
tweetCART = rpart(Negative~. , data = trainSparse, method = "class")##method = class because it is a classfication problem
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type = "class")

table(testSparse$Negative, predictCART)
## accuracy is (294+18)/(294+18+6+37)
table(testSparse$Negative)
##baseline model is (300)/(300+55)
library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative~., data = trainSparse)
predictRF = predict(tweetRF, newdata = testSparse)
table(testSparse$Negative, predictRF)
