# to read: source("train.R")

library(rpart)
library(rpart.plot)
library(caTools)
library(tm)

setwd("Z:/The Analytics Edge/Competition")
ebay = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
ebay$sold = as.factor(ebay$sold)
ebay$biddable = as.factor(ebay$biddable)
ebay$condition = as.factor(ebay$condition)
ebay$cellular = as.factor(ebay$cellular)
ebay$carrier = as.factor(ebay$carrier)
ebay$color = as.factor(ebay$color)
ebay$storage = as.factor(ebay$storage)
ebay$productline = as.factor(ebay$productline)
#ebay$notWorking = ebay$condition == "For parts or not working"
#ebay$unknown = ebay$productline == "Unknown"


# split the data
biddable = subset(ebay, biddable == 1)
biddable$biddable = NULL
biddable$UniqueID = NULL
notBiddable = subset(ebay, biddable == 0)
notBiddable$biddable = NULL
notBiddable$UniqueID = NULL


# Create corpus
corpus = Corpus(VectorSource(biddable$description))
corpus[[1]]
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]

dtm = DocumentTermMatrix(corpus)
dtm

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97)
dtm

# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))
colnames(labeledTerms) = make.names(colnames(labeledTerms))
# Add in the outcome variable
labeledTerms$sold = biddable$sold

# take the important words and put into biddable data
biddable$box=labeledTerms$box
biddable$case=labeledTerms$case
biddable$condit=labeledTerms$condit
biddable$crack=labeledTerms$crack
biddable$function.=labeledTerms$function.
biddable$good=labeledTerms$good
biddable$great=labeledTerms$great
biddable$ipad=labeledTerms$ipad
biddable$item=labeledTerms$item
biddable$minor=labeledTerms$minor
biddable$new=labeledTerms$new
biddable$pleas=labeledTerms$pleas
biddable$scratch=labeledTerms$scratch
biddable$screen=labeledTerms$screen
biddable$sign=labeledTerms$sign
biddable$use=labeledTerms$use
biddable$work=labeledTerms$work
biddable$description = NULL

biddable$productlineiPad1 = biddable$productline == "iPad 1"
biddable$productlineiPad2 = biddable$productline == "iPad 2"
biddable$productlineiPad3 = biddable$productline == "iPad 3"
biddable$productlineiPad4 = biddable$productline == "iPad 4"
biddable$productlineiPad5 = biddable$productline == "iPad 5"
biddable$productlineiPadAir = biddable$productline == "iPad Air"
biddable$productlineiPadAir2 = biddable$productline == "iPad Air 2"
biddable$productlineiPadmini = biddable$productline == "iPad mini"
biddable$productlineiPadmini2 = biddable$productline == "iPad mini 2"
biddable$productlineiPadmini3 = biddable$productline == "iPad mini 3"
biddable$productlineiPadminiRetina = biddable$productline == "iPad mini Retina"
biddable$productlineUnknown = biddable$productline == "Unknown"
biddable$productline = NULL

set.seed(47308)
spl = sample.split(biddable$sold, 0.7)
biddableTrain = subset(biddable, spl == TRUE)
biddableTest = subset(biddable, spl == FALSE)
biddableTrain$productline = NULL
biddableTest$productline = NULL

LogModel = glm(sold ~ ., data=biddable, family="binomial")
summary(LogModel)
predictLog = predict(LogModel, type="response")
table(biddable$sold, predictLog > 0.5)
(116+611)/(116+611+81+29)
# 0.8685783


LogModel2 = glm(sold ~ startprice + condition + storage + crack + great + scratch + use + productlineiPad1 + productlineiPadAir + productlineiPadAir2 + productlineiPadmini2 + productlineiPadmini3, data=biddable, family="binomial")
summary(LogModel2)
predictLog2 = predict(LogModel2, type="response")
table(biddable$sold, predictLog2 > 0.5)
(104+609)/(104+609+93+31)
# 0.8518519


# ------ MODEL 1 BEGIN ------
LogModelTrain = glm(sold ~ ., data=biddableTrain, family="binomial")
#summary(LogModelTrain)
predictLogTrain = predict(LogModelTrain, type="response")
table(biddableTrain$sold, predictLogTrain > 0.5)
(77+427)/(77+427+61+21)
# 0.8685783

# Predictions on the test set
predictTest = predict(LogModelTrain, type="response", newdata=biddableTest)
table(biddableTest$sold, predictTest > 0.5)
(28+179)/(28+179+31+13)
# [1] 0.8247012
# ------ MODEL 1 END ------


# ------ MODEL 2 BEGIN ------
LogModel2Train = glm(sold ~ startprice + condition + storage + crack + great + scratch + use + productlineiPad1 + productlineiPadAir + productlineiPadAir2 + productlineiPadmini2 + productlineiPadmini3, data=biddableTrain, family="binomial")
#summary(LogModel2Train)
predictLog2Train = predict(LogModel2Train, type="response")
table(biddableTrain$sold, predictLog2Train > 0.5)
(63+429)/(63+429+75+19)
# 0.8395904

# Predictions on the test set
predictTest2 = predict(LogModel2Train, type="response", newdata=biddableTest)
table(biddableTest$sold, predictTest2 > 0.5)
(33+183)/(33+183+26+9)
# [1] 0.8605578
# ------ MODEL 2 END ------




# cart
biddableCART = rpart(sold~., data=biddableTrain, method="class", cp=0.0)
prp(biddableCART, tweak=1.5, gap=0, space=0)

# Make predictions on the test set
pred = predict(biddableCART, newdata=biddableTest)
pred[1:10,]
pred.prob = pred[,2]

# Compute accuracy

table(biddableTest$sold, pred.prob >= 0.5)
(22+180)/(22+180+12+37)
# [1] 0.8047809


# -----------------------------------------------------------------------------------


# create training / testing sets
#set.seed(47308)
#spl = sample.split(biddable$sold, SplitRatio = 0.7)
#biddableTrain = subset(biddable, spl==TRUE)
#biddableTest = subset(biddable, spl==FALSE)

set.seed(47308)
spl = sample.split(notBiddable$sold, SplitRatio = 0.7)
notBiddableTrain = subset(notBiddable, spl==TRUE)
notBiddableTest = subset(notBiddable, spl==FALSE)
rm(spl)


# baseline
table(ebay$sold)
baseline=1001/(1001+860)
# 0.5378829

ebayNoSold = ebay
ebayNoSold$sold = NULL

LogModel = glm(sold ~ biddable + condition + cellular + carrier + color + storage + productline, data=ebay, family="binomial")
LogModel2 = glm(sold ~ biddable + condition + color + storage , data=ebay, family="binomial")
LogModel3 = glm(sold ~ biddable + condition + storage , data=ebay, family="binomial")
LogModel4 = glm(sold ~ biddable + condition + storage  + productline, data=ebay, family="binomial") # <- for some reason I think this is the best, same as LogModel6
LogModel5 = glm(sold ~ biddable + condition + color + storage  + productline, data=ebay, family="binomial")
LogModel6 = glm(sold ~ biddable + condition + carrier + color + storage  + productline, data=ebay, family="binomial")
LogModel7 = glm(sold ~ biddable + startprice + condition + storage  + productline, data=ebay, family="binomial")

predictLog = predict(LogModel, type="response")
table(ebay$sold, predictLog > 0.5)
(810+639)/(810+639+221+191)
# 0.7786136

predictLog2 = predict(LogModel2, type="response")
table(ebay$sold, predictLog2 > 0.5)
(805+639)/(805+639+196+221)
# 0.7759269

predictLog3 = predict(LogModel3, type="response")
table(ebay$sold, predictLog3 > 0.5)
(809+636)/(809+636+192+224)
# 0.7764643

predictLog4 = predict(LogModel4, type="response")
table(ebay$sold, predictLog4 > 0.5)
(806+641)/(806+641+195+219)
# 0.777539

predictLog5 = predict(LogModel5, type="response")
table(ebay$sold, predictLog5 > 0.5)
(807+639)/(807+639+221+194)
# 0.7770016

predictLog6 = predict(LogModel6, type="response")
table(ebay$sold, predictLog6 > 0.5)
(808+639)/( 808+639+221+193)
# 0.777539

predictLog7 = predict(LogModel7, type="response")
table(ebay$sold, predictLog7 > 0.5)
(847+643)/(847+643+154+217)
# 0.8006448


CARTmodel = rpart(sold ~ biddable + condition + storage  + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ biddable + condition + cellular + carrier + color + storage + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ condition + cellular + carrier + color + storage + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ condition + cellular + color + storage + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ biddable + startprice + condition + cellular + color + storage + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ biddable + notWorking + unknown + startprice + condition + cellular + color + storage + productline, data=ebay)
prp(CARTmodel)

CARTmodel = rpart(sold ~ biddable + startprice + condition + storage, data=ebay)
prp(CARTmodel)

notBiddable = subset(ebay, biddable==0)
biddable = subset(ebay, biddable==1)

CARTmodel = rpart(sold ~ notWorking + unknown + startprice + condition + cellular + color + storage + productline, data=biddable)
prp(CARTmodel)

CARTmodel = rpart(sold ~ startprice + condition + cellular + color + storage + productline, data=notBiddable)
prp(CARTmodel)

CARTmodel = rpart(sold ~ notWorking + unknown + startprice + condition + cellular + storage + productline, data=notBiddable, cp=0.0)
prp(CARTmodel)

CARTmodel = rpart(sold ~ notWorking + unknown + startprice + condition + productline, data=notBiddable)
prp(CARTmodel)

CARTmodel = rpart(sold ~ notWorking + unknown + startprice + condition + productline, data=biddable)
prp(CARTmodel)

y = subset(notBiddable, productline != "Unknown")
CARTmodel = rpart(sold ~ startprice + condition + cellular + storage + productline, data=y)
prp(CARTmodel)

z = subset(notBiddable, productline == "Unknown")
CARTmodel = rpart(sold ~ startprice + condition + cellular + storage + productline, data=z, cp=0.0)
prp(CARTmodel, tweak=1.5, gap=0, space=0)
prp(CARTmodel, tweak=1.5, gap=0, space=0, type=2, extra=100)



# thoughs
## biddable vs not biddable (biddable)
## "For parts or not working" vs all others (condition)
## "Unknown" vs all others (productline)
## parse description

# I'm thinking xxx models

## notBiddable (4 models)
### "For parts or not working" (condition)
#### "Unknown" (productline) [model = y]
#### !"Unknown" (productline) [model = y]
### !"For parts or not working"
#### "Unknown" (productline) [model = y]
#### !"Unknown" (productline) [model = y]

## biddable
### "For parts or not working" (condition)
#### price xxxxxx
#### !price xxxxxx
### !"For parts or not working"
#### price xxxxxx
#### !price xxxxxx

