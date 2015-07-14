# to read: source("train.R")

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
