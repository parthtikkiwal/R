# R
# The final project
# objective: design a targeted customer retaining strategy

rm(list = ls())
setwd("H:/Data Driven Marketing/Final Project")
install.packages("tree")
install.packages("ranger")
require(gamlr)
require(tree)
require(ranger)

attr = read.csv("abb_attrition.csv")
attr$attrition = factor(attr$attrition)

# training and test sample
n = dim(attr)[1]
set.seed(1)
tsid = sample.int(n,floor(n/5))
attr0 = attr[-tsid,] # training sample
attr1 = attr[tsid,] # testing sample

y0 = as.numeric(as.character(attr0$attrition))
y1 = as.numeric(as.character(attr1$attrition))

# define functions of error rate and deviance
er = function(y,yhat){
  y = as.numeric(y);yhat = as.numeric(yhat>0.5)
  z = sum(abs(y-yhat))/length(y)
  return(z)
}

devf = function(y,p){
  lh = y*p + (1-y)*(1-p)
  dev = -2*sum(log(pmin(pmax(lh,1e-10),1-1e-10)))
  return(dev)
}

# 0. lasso with interaction effects
x0 = model.matrix(attrition ~ .*latitude*longitude,data = attr0)
x1 = model.matrix(attrition ~ .*latitude*longitude,data = attr1)
lr = cv.gamlr(x0,y0,lmr=1e-4,family = "binomial")
yhatlr0 = predict(lr,x0,type = "response",select = "min")
yhatlr1 = predict(lr,x1,type = "response",select = "min")

# report the prediction error rate and deviance for the testing sample
er1 = er(y1,yhatlr1)
dev1 = devf(y1,yhatlr1)
print('OOS error rates and deviance')
c(er1,dev1)

# 1. classification tree (please replace this section with your best model for predicting attrition)
attree = tree(attrition ~ ., data = attr0,mindev = 0.005)
plot(attree,col=8,lwd = 2,cex = 0.5);text(attree,cex=0.68)
cvat = cv.tree(attree, ,prune.tree)

yhattree0 = predict(attree,attr0,type = "vector",eps = 1e-10)[,2]
yhattree1 = predict(attree,attr1,type = "vector",eps = 1e-10)[,2]

# report the prediction error rate and deviance
print('classification tree')
er1 = er(y1,yhattree1)
dev1 = devf(y1,yhattree1)
print('OOS error rates and deviance')
c(er1,dev1)

## New RF Model
rf <- ranger(attrition ~ ., data=attr0, mtry = 4,
             num.tree=200, classification = TRUE, probability = TRUE, max.depth = 5)

yhatrf1 = predict(rf,attr1)$predictions[,2]

# report the prediction error rate and deviance
print('Random Forest Model')
er1 = er(y1,yhatrf1)
dev1 = devf(y1,yhatrf1)
print('OOS error rates and deviance')
c(er1,dev1)

# 2. calculation of total profits

# Total Profit in 2015
total.reservation = rowSums(attr1[,15:26])
revenue = attr1$averagedailyrateusd * total.reservation
profit.amount = revenue*0.15
sum(profit.amount)

# Total Profit in 2016
# Selecting properties retained
prop.retained = attr1[which(attr1$attrition == 0),]

total.reservation = rowSums(prop.retained[,15:26])
revenue = prop.retained$averagedailyrateusd * total.reservation
profit.amount = revenue*0.15
sum(profit.amount)

# 3. what is the cutoff value for the attrition probability if a property's revenue is $25000?
# Since the cost is 1000 and expected revenue is 25000
sample.profit = 25000*0.15
sample.pstar = 1000/sample.profit

print(paste("The cutoff probability is", sample.pstar))

# 4. report the total net profit for properties in the testing sample under your retaining strategy

# Part 1
# Using the RF model we have the predicted attrition probabilities for Test Sample
attr1$prob = predict(rf,attr1)$predictions[,2]

# Part 2
# Using the revenue function to obtain the revenue information for each property in Test Sample
attr1$revenue = attr1$averagedailyrateusd*rowSums(attr1[,15:26])

# Defining p.star for each property based on revenue
attr1$pstar = 1000/(attr1$revenue * 0.15)   # i.e. Cost/Profit

attr1$targeted = as.numeric(attr1$prob > attr1$pstar)
attr1$retained = as.numeric(attr1$attrition == 0 | attr1$targeted == 1)

# Total Profit Calculation
prof.retained = sum((attr1$revenue * 0.15)[attr1$retained == 1])
tot.cost = sum(1000 * attr1$targeted)
net.profit = prof.retained - tot.cost

# Net Profit after targeted campaign
print(net.profit)
