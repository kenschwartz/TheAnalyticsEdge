# setwd("Z:/The Analytics Edge/Competition")
# to read: source("train.R")

library(rpart)
library(rpart.plot)
library(caTools)
library(tm)

ebay = read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
ebay$sold = as.factor(ebay$sold)
ebay$biddable = as.factor(ebay$biddable)
ebay$condition = as.factor(ebay$condition)
ebay$cellular = as.factor(ebay$cellular)
ebay$carrier = as.factor(ebay$carrier)
ebay$color = as.factor(ebay$color)
ebay$storage = as.factor(ebay$storage)
ebay$productline = as.factor(ebay$productline)
# table(ebay$sold,log(ebay$startprice) > 6.515)
ebay$tooExpensive = log(ebay$startprice) > 6.515
table(ebay$sold, ebay$tooExpensive)

#ebay$notWorking = ebay$condition == "For parts or not working"
#ebay$unknown = ebay$productline == "Unknown"


# split the data
#biddable = subset(ebay, biddable == 1 & tooExpensive == FALSE)
biddable = subset(ebay, biddable == 1)
biddable$biddable = NULL
biddable$UniqueID = NULL
#notBiddable = subset(ebay, biddable == 0 & tooExpensive == FALSE)
notBiddable = subset(ebay, biddable == 0)
notBiddable$biddable = NULL
notBiddable$UniqueID = NULL

#notBiddableExpensive = subset(ebay, biddable == 0 & log(startprice) > 5.9)
#notBiddableExpensive$biddable = NULL
#notBiddableExpensive$UniqueID = NULL
#notBiddableExpensive$startprice = log(notBiddableExpensive$startprice)

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

corpus2 = Corpus(VectorSource(notBiddable$description))
corpus2[[1]]
corpus2 = tm_map(corpus2, tolower)
corpus2 = tm_map(corpus2, PlainTextDocument)
corpus2 = tm_map(corpus2, removePunctuation)
corpus2 = tm_map(corpus2, removeWords, stopwords("english"))
corpus2 = tm_map(corpus2, stemDocument)
corpus2[[1]]

dtm2 = DocumentTermMatrix(corpus2)
dtm2

# Remove sparse terms
dtm2x = removeSparseTerms(dtm2, 0.92)
dtm2x


labeledTerms2 = as.data.frame(as.matrix(dtm2x))
colnames(labeledTerms2) = make.names(colnames(labeledTerms2))
# Add in the outcome variable
labeledTerms2$sold = notBiddable$sold

notBiddable$condit=labeledTerms2$condit
notBiddable$good=labeledTerms2$good
notBiddable$ipad=labeledTerms2$ipad
notBiddable$scratch=labeledTerms2$scratch
notBiddable$use=labeledTerms2$use
notBiddable$work=labeledTerms2$work
notBiddable$description = NULL

notBiddable$productlineiPad1 = notBiddable$productline == "iPad 1"
notBiddable$productlineiPad2 = notBiddable$productline == "iPad 2"
notBiddable$productlineiPad3 = notBiddable$productline == "iPad 3"
notBiddable$productlineiPad4 = notBiddable$productline == "iPad 4"
notBiddable$productlineiPad5 = notBiddable$productline == "iPad 5"
notBiddable$productlineiPadAir = notBiddable$productline == "iPad Air"
notBiddable$productlineiPadAir2 = notBiddable$productline == "iPad Air 2"
notBiddable$productlineiPadmini = notBiddable$productline == "iPad mini"
notBiddable$productlineiPadmini2 = notBiddable$productline == "iPad mini 2"
notBiddable$productlineiPadmini3 = notBiddable$productline == "iPad mini 3"
notBiddable$productlineiPadminiRetina = notBiddable$productline == "iPad mini Retina"
notBiddable$productlineUnknown = notBiddable$productline == "Unknown"
notBiddable$productline = NULL

notBiddable$startprice = log(notBiddable$startprice)

set.seed(47308)
spl = sample.split(biddable$sold, 0.7)
biddableTrain = subset(biddable, spl == TRUE)
biddableTest = subset(biddable, spl == FALSE)
biddableTrain$productline = NULL
biddableTest$productline = NULL

set.seed(47308)
spl = sample.split(notBiddable$sold, 0.7)
notBiddableTrain = subset(notBiddable, spl == TRUE)
notBiddableTest = subset(notBiddable, spl == FALSE)
notBiddableTrain$productline = NULL
notBiddableTest$productline = NULL



LogModel = glm(sold ~ ., data=biddable, family="binomial")
summary(LogModel)
predictLog = predict(LogModel, type="response")
table(biddable$sold, predictLog > 0.5)
(116+611)/(116+611+81+29)
# 0.8685783


LogModel2 = glm(sold ~ startprice + tooExpensive + condition + storage + crack + great + scratch + use + productlineiPad1 + productlineiPadAir + productlineiPadAir2 + productlineiPadmini2 + productlineiPadmini3, data=biddable, family="binomial")
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
#biddableTrain$startprice = log(biddableTrain$startprice)
#biddableTest$startprice = log(biddableTest$startprice)
LogModel2Train = glm(sold ~ startprice + condition + storage + crack + great + scratch + use + productlineiPad1 + productlineiPadAir + productlineiPadAir2 + productlineiPadmini2 + productlineiPadmini3, data=biddableTrain, family="binomial")
#summary(LogModel2Train)
predictLog2Train = predict(LogModel2Train, type="response")
table(biddableTrain$sold, predictLog2Train > 0.5)
(66+430)/(66+430+68+18)
# 0.8522337

# Predictions on the test set
predictTest2 = predict(LogModel2Train, type="response", newdata=biddableTest)
table(biddableTest$sold, predictTest2 > 0.5)
(31+181)/(31+181+27+11)
# [1] 0.8605578
# ------ MODEL 2 END ------


# --------------------- NOT BIDDABLE BEGIN

# ------ MODEL 1 BEGIN ------
#notBiddableTrain$startprice = log(notBiddableTrain$startprice)
#notBiddableTest$startprice = log(notBiddableTest$startprice)
LogModelTrain2 = glm(sold ~ ., data=notBiddableTrain, family="binomial")
summary(LogModelTrain2)
predictLogTrain2 = predict(LogModelTrain2, type="response")
table(notBiddableTrain$sold, predictLogTrain2 > 0.5)
(559+6)/(559+6+4+148)
# 0.7880056

# at least this one uses price as the biggie
#LogModelTrain2 = glm(sold ~ startprice + storage + hous + item + sign
#+ productlineiPadmini2,  data=notBiddableTrain, family="binomial")
#summary(LogModelTrain2)

LogModelTrain2 = glm(sold ~ startprice + scratch + work,  data=notBiddableTrain, family="binomial")
summary(LogModelTrain2)
predictLogTrain2 = predict(LogModelTrain2, type="response")
table(notBiddableTrain$sold, predictLogTrain2 > 0.5)
(559+2)/(554+2+4+152)
# 0.7879213

LogModelTrain2 = glm(sold ~ startprice + condition + scratch + work,  data=notBiddableTrain, family="binomial")
summary(LogModelTrain2)
predictLogTrain2 = predict(LogModelTrain2, type="response")
table(notBiddableTrain$sold, predictLogTrain2 > 0.5)
(561+2)/(561+2+2+152)
# 0.7852162

# Predictions on the test set
predictTest2 = predict(LogModelTrain2, type="response", newdata=notBiddableTest)
table(notBiddableTest$sold, predictTest2 > 0.5)
(28+179)/(28+179+31+13)
# [1] 0.8247012

notBiddableCART = rpart(sold ~ startprice + condition, data=notBiddableTrain, method="class", cp=0.0)
prp(notBiddableCART, tweak=1.5, gap=0, space=0)
pred = predict(notBiddableCART, newdata=notBiddableTest)
pred[1:10,]
pred.prob = pred[,2]
table(notBiddableTest$sold, pred.prob >= 0.5)
(226+9)/(226+9+15+57)
# [1] 0.7654723

# ------ MODEL 1 END ------

# --------------------- NOT BIDDABLE END


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

