##################################
#         LAB 1 Assignment 3     #
##################################

#########
#   1   #
#########
diabetes = read.csv("pima-indians-diabetes.csv", header = FALSE)

plot(diabetes[,2], 
     diabetes[,8],xlab = "plasma-glucose-concentration", 
     ylab = "age",
     col = ifelse(diabetes[,9]==1, "pink", "blue"))

title(main = "Scatterplot - Plasma and Age")

#########
#   2   #
#########
diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial")
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.5, 1, 0) 
table(diabetes[,9])
coef(diabetes_glm) 
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

plot(diabetes[,2], 
     diabetes[,8], 
     xlab = "plasma-glucose-concentration",
     ylab = "age", 
     col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatterplot")
summary(diabetes_glm)

#########
#   3   #
#########
k_diabetes = -coef(diabetes_glm)[2]/coef(diabetes_glm)[3] #slope
m_diabetes = -coef(diabetes_glm)[1]/coef(diabetes_glm)[3] #intercept 
abline(m_diabetes,k_diabetes)

#########
#   4   #
#########
diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial", data = diabetes[,c(2,8,9)])
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.2, 1, 0)
table(diabetes[,9])
coef(diabetes_glm)
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

plot(diabetes[,2], diabetes[,8],xlab = "plasma-glucose-concentration",ylab = "age",  col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatter Plot r = 0.2")

#r = 0.8
diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial", data = diabetes[,c(2,8,9)])
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.8, 1, 0) 
table(diabetes[,9])
coef(diabetes_glm)
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

plot(diabetes[,2], diabetes[,8],xlab = "plasma-glucose-concentration",ylab = "age", col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatter Plot r = 0.8")

#########
#   5   #
#########
diabetes = diabetes %>%
  mutate(z1 = diabetes$V2^4, z2 = diabetes$V2^3*diabetes$V8, z3 = diabetes$V2^2*diabetes$V8^2, z4 = diabetes$V2*diabetes$V8^3, z5 = diabetes$V8^4) #adding columns to dataframe
diabetes_glm_expansion = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8] + diabetes$z1 + diabetes$z2 + diabetes$z3 + diabetes$z4 + diabetes$z5, family = "binomial")
prob_expansion = predict(diabetes_glm_expansion, type = "response")
pred_expansion = ifelse(prob_expansion>0.5, 1, 0) 
table(diabetes[,9])

confusionMatrix_diabetes_expansion = table(diabetes[,9],pred_expansion)
missclass_error_diabetes_expansion = (1 - sum(diag(confusionMatrix_diabetes_expansion))/nrow(diabetes))

plot(diabetes[,2], diabetes[,8], xlab = "plasma-glucose-concentration", ylab = "age", col = ifelse(pred_expansion==1, "orange", "blue"))
title(main = "Predicted Expansion Scatter Plot")

summary(diabetes_glm_expansion)




