##################################
#         LAB 2 Assignment 3     #
##################################

#########
#   1   #
#########
library(dplyr)
library(tidyr)
library(ggplot2)
library(ISLR)
communities_unscaled = read.csv("communities.csv")

#scale data: 
communities = scale(communities_unscaled[1:100])
communities = as.data.frame(communities)
communities = communities %>%
  mutate(ViolentCrimesPerPop = communities_unscaled$ViolentCrimesPerPop) #adding last column unscaled 

S = cov(communities)
var(communities)
eigen_communities = eigen(S)

variance = eigen_communities$values / sum(eigen_communities$values)*100
sprintf("%2.3f", variance)

for (i in 1:length(variance)){
  if (sum(variance[1:i]) > 95)
    print(i)
}

print(cumsum(variance) >= 95)

#########
#   2   #
#########

res<-princomp(communities)
plot(res)
z=res$loadings
pc1=z[,1]
plot(pc1) 
which.max(pc1)

abs.pc1=abs(pc1)
sorted.pc1 = sort(abs.pc1, decreasing = TRUE) 
five_contribute_mostly = sorted.pc1[1:5]
five_contribute_mostly

pc1_scores = res$scores[,1]
pc2_scores = res$scores[,2]

ggplot(data=communities, aes(x=pc1_scores, y=pc2_scores)) + geom_point(aes(color = communities$ViolentCrimesPerPop))

#########
#   3   #
#########

n = dim(communities)[1]
set.seed(12345)
#split data
id = sample(1:n, floor(n*0.5)) #id = 50% of communities
train = communities[id,] #train 50% of communities
test = communities[-id,] #test 50% of communities
train_df = as.data.frame(train)
test_df = as.data.frame(test)

communities_lm = lm(ViolentCrimesPerPop~.,-1, data=train_df) # -1 due to no intercept
pred_train=predict(communities_lm, train_df)
confusionmatrix_train=table(train_df$ViolentCrimesPerPop, pred_train)
missclassification_error_train=(1-sum(diag(confusionmatrix_train))/nrow(train_df))
MSE_train = sum((train_df$ViolentCrimesPerPop-pred_train)^2) / length(pred_train)

pred_test=predict(communities_lm, test_df)
confusionmatrix_test=table(test_df$ViolentCrimesPerPop, pred_test)
missclassification_error_test=(1-sum(diag(confusionmatrix_test))/nrow(test_df))
MSE_test = sum((test_df$ViolentCrimesPerPop-pred_test)^2) / length(pred_test)

R2 = sum((MSE_test-mean(test_df$ViolentCrimesPerPop))^2/ sum(test_df$ViolentCrimesPerPop-mean(test_df$ViolentCrimesPerPop))^2)

#########
#   4   #
#########

testE = list() 
trainE = list()
k=0

MSE_train_function = function(theta){
  MSE_train_temp = mean((train_df$ViolentCrimesPerPop-(as.matrix(train_df[,1:100]))%*%theta)^2)
  print(MSE_train_temp)
  MSE_test_temp = mean((test_df$ViolentCrimesPerPop-(as.matrix(test_df[,1:100]))%*%theta)^2)
  .GlobalEnv$k = .GlobalEnv$k+1
  .GlobalEnv$trainE[[k]]=MSE_train_temp
  .GlobalEnv$testE[[k]]=MSE_test_temp
  return(MSE_train_temp)
  }

res = optim(rep(0,100), fn=MSE_train_function, method="BFGS")
trainE
testE

which.min(trainE) 
which.min(testE) 


plot((19910:19920), trainE[19910:19920], col="blue", ylim = c(0.06686,0.0669))
lines((1000:20000), testE[1000:20000], col="pink")

trainE[2166] 
testE[2166] 

print(trainE[19914])
print(testE[2166])



