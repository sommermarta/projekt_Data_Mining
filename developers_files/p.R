library("foreign")
library("MASS")
library("rpart")
library("klaR")
library("glmnet")
library("e1071")

set.seed(476)
se <- read.arff("http://archive.ics.uci.edu/ml/machine-learning-databases/00266/seismic-bumps.arff")
se <- se[,-c(14:16,18,9)]
head(se,2)
dim(se)

# specjalnie dla qda, dane przerabiam z faktorowych na ilosciowe:

se_r <- se
levels(se_r$seismic) <- c(0,1,2,3)
levels(se_r$seismoacoustic) <- c(0,1,2,3)
levels(se_r$shift) <- c(0,1)
levels(se_r$ghazard) <- c(0,1,2,3)
se_r[,c(1,2,3,8)] <- apply(se_r[,c(1,2,3,8)],2,as.numeric)

head(se_r,2)

# podzial na probe testowa i treningowa:

dim(se)
ile <- floor((2/3)*dim(se)[1])
s <- sample(1:dim(se)[1],ile)
tren <- se[s,]
test <- se[-s,]

tren_r <- se_r[s,]
test_r <- se_r[-s,]

# bedziemy budowac model na treningowej, a testowac na testowej :D

# lda

mod_lda <- lda(class~., data=tren)
pred <- predict(mod_lda, newdata=test)$class
pred
t <- table(pred,test$class)
100*sum(diag(t))/sum(t)
t
table(test$class)

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

# czyli raczej slabo :(

# qda

mod_qda <- qda(class~., data=tren_r)

pred <- predict(mod_qda, newdata=test_r)$class
t <- table(pred,test_r$class)
100*sum(diag(t))/sum(t)
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

# logistyczna

mod_log <- glm(class~.,data=tren,family="binomial")

pred <- ifelse(predict(mod_log,newdata=test,type="response")>0.5,1,0) 
t <- table(test$class,pred)
sum(diag(t))/nrow(test)*100 
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

aic <- step(mod_log,direction="backward",k=2)
# class ~ seismic + gpuls + nbumps2 + nbumps3
anova(aic,mod_log,test="Chisq")

mod_log_aic <- glm(aic$formula,data=tren,family="binomial")

pred <- ifelse(predict(mod_log_aic,newdata=test,type="response")>0.5,1,0) 
t <- table(test$class,pred)
sum(diag(t))/nrow(test)*100 
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

# ???????????????????????????????????

cv.glmnet(as.matrix(tren_r[,1:13]),ifelse(tren_r$class=="1",1,0))
mod_log_reg <- glmnet(as.matrix(tren_r[,1:13]),tren_r[,14],family="binomial",
                      alpha=0,lambda.min=0.007049594)
plot(mod_log_reg)
coef(mod_log_reg)

pred <- ifelse(predict(mod_log_reg,newx=as.matrix(test_r[,1:13])
                       ,type="response")>0.5,1,0) 

# ????????????????????????????????????????????????????????

# drzewo

mod_tree <- rpart(class~.,data=tren)
plot(mod_tree)
text(mod_tree)

pred <- predict(mod_tree,newdata=test,type="class")
t <- table(test$class,pred)
sum(diag(t))/nrow(test)*100 
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja

# svm

mod <- svm(class~.,data=tren,type="C",kernel="sigmoid")
pred <- predict(mod,test[,-14])

t <- table(test$class,pred)
sum(diag(t))/nrow(test)*100 
t

czulosc <- t[2,2]/(sum(t[2,]))
czulosc
precyzja <- t[2,2]/sum(t[,2])
precyzja



