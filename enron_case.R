emails = read.csv("energy_bids.csv",stringsAsFactors = FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
##responsive is indicate whether the email is related with our cases. 
library(tm)

corpus = Corpus(VectorSource(emails$email))
corpus[[1]]$content
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords,stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content

dtm = DocumentTermMatrix(corpus)
dtm

dtm = removeSparseTerms(dtm, 0.97)

labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, SplitRatio = 0.7)
train = subset(labeledTerms, spl==TRUE)
test = subset(labeledTerms, spl==FALSE)

library(rpart)
library(rpart.plot)

emailCART= rpart(responsive~., data = train, method = "class")
prp(emailCART)
pred = predict(emailCART, newdata = test)
pred[1:10,]
pred.prob = pred[,2]
table(test$responsive, pred.prob>=0.5)
table(test$responsive)

##since the false negative will undermine the result. We use ROCR to identify the cutoff


library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", "fpr")##true positive rate and false positive rate
plot(perfROCR, colorize=TRUE)

performance(predROCR,"auc")@y.values
## this mean the model can identify the result correctly 81% of the time. 


