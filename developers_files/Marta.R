library("foreign")
library("MASS")
library("ROCR")
library("e1071")
library("adabag")
library("randomForest")

# wczytuje dane:

se <- read.arff("http://archive.ics.uci.edu/ml/
                machine-learning-databases/00266/seismic-bumps.arff")
head(se,3)

# wstepna selekcja danych

se_wyb <- se[,-c(14:16,18,9)]
head(se_wyb,3)

# zamiana zmiennych nominalnych na liczbowe:

se_r <- se_wyb
levels(se_r$seismic) <- c(0,1,2,3)
levels(se_r$seismoacoustic) <- c(0,1,2,3)
levels(se_r$shift) <- c(0,1)
levels(se_r$ghazard) <- c(0,1,2,3)
se_r[,c(1,2,3,8)] <- apply(se_r[,c(1,2,3,8)],2,as.numeric)
names(se_r)[c(1,2,3,8)] <- c("seismic2","seismoacoustic2","shift2","ghazard2")
head(se_r,3)

# ostateczne zbiory danych:

head(se_wyb,2)
head(se_r,2)

# podzial na zbior uczacy i testowy:

set.seed(456)
ind <- sample(1:nrow(se_wyb), nrow(se_wyb)*2/3)

Train  <-  se_wyb[ind,]
Test  <-  se_wyb[-ind,]    # dla zmiennych liczbowych

Train2 <- se_r[ind,]
Test2 <- se_r[-ind,]    # dla zmiennych nominalnych

# przydatne funkcje:

tabela <- function(predykcja_klasy, klasy_prawdziwe){
   table(predykcja_klasy, klasy_prawdziwe)
}

procent <- function(t){
   100*sum(diag(t))/sum(t)
}

czulosc <- function(t){
   if(sum(t[2,])==0) return(0) else t[2,2]/(sum(t[2,]))
}

precyzja <- function(t){
   if(sum(t[,2])==0) return(0) else t[2,2]/sum(t[,2])
}

roc <- function(pred_prawdopod, prawdziwe_klasy){
   pred <- prediction(pred_prawdopod, prawdziwe_klasy)
   perf <- performance(pred, measure="tpr",x.measure="fpr")
   plot(perf,col="red")
   abline(0,1)
   lines(c(0.5,0.5),c(-0.1,1.1),lty=2,col="green")
}

auc <- function(pred_prawdopod, prawdziwe_klasy){
   pred <- prediction(pred_prawdopod, prawdziwe_klasy)
   performance(pred, "auc")@y.values[[1]] 
}

################################################################
########################### LDA ################################
################################################################

# dopasowanie modelu:

mod_lda <- lda(class~., data=Train)
pred_klas <- predict(mod_lda, newdata=Test)$class
pred_praw <- predict(mod_lda, newdata=Test)$posterior[,2]

# diagnostyka:

tab <- tabela(pred_klas, Test$class)
tab
procent(tab)
czulosc(tab)
precyzja(tab)

# kroswalidacja dziesieciokrotna:

n <- nrow(se_wyb)
s <- sample(1:n,n)
dane <- se_wyb[s,]
ile <- floor(n/10)
ile_ost <- ile+ (n-10*ile)

pro <- numeric(10)
czu <- numeric(10)
pre <- numeric(10)

for(i in 1:10){
   
   if(i != 10){
      co <- ((i-1)*ile+1):(i*ile)
   } else{
      co <- (n-ile_ost+1):n
   }
   
   mod <- lda(class~., data=dane[-co,])
   pr <- predict(mod, newdata=dane[co,])$class
      
   t <- tabela(pr,dane[co,]$class)
   pro[i] <- procent(t)
   czu[i] <- czulosc(t)
   pre[i] <- precyzja(t)
   
}

mean(pro)
mean(czu)
mean(pre)

# krzywa ROC:

roc(pred_praw, Test$class)

# wspolczynnik AUC:

auc(pred_praw, Test$class)

# wszysko w jednym 

lda_ost <- vector("list",6)
names(lda_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
lda_ost$tabela <- tab
lda_ost$procent <- procent(tab)
lda_ost$czulosc <- czulosc(tab)
lda_ost$precyzja <- precyzja(tab)
lda_ost$roc <- list(pred_praw, Test$class)  # potem wystarczy roc(lda$roc[[1]],lda$roc[[2]])
lda_ost$auc <- auc(pred_praw, Test$class)

lda_ost

####################################################################
############################### QDA ################################
####################################################################

# dopasowanie modelu:

mod_qda <- qda(class~., data=Train2)
pred_klas <- predict(mod_qda, newdata=Test2)$class
pred_praw <- predict(mod_qda, newdata=Test2)$posterior[,2]

# diagnostyka modelu:

tab <- tabela(pred_klas, Test2$class)
tab 

procent(tab)
czulosc(tab)
precyzja(tab)

# krzywa ROC i wspolczynnik AUC:

roc(pred_praw, Test2$class)
auc(pred_praw, Test2$class)

# wszysko w jednym 

qda_ost <- vector("list",6)
names(qda_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
qda_ost$tabela <- tab
qda_ost$procent <- procent(tab)
qda_ost$czulosc <- czulosc(tab)
qda_ost$precyzja <- precyzja(tab)
qda_ost$roc <- list(pred_praw, Test2$class) 
qda_ost$auc <- auc(pred_praw, Test2$class)

qda_ost

##################################################################
############################# SVM ################################
##################################################################

mod_svm1 <- svm(class~., data=Train, type="C", kernel="radial",
               probability=TRUE)
pred1 <- predict(mod_svm1, Test, probability=TRUE)
pred_praw1 <- attr(pred1,"probabilities")[,2]
pred_klas1 <- predict(mod_svm1, Test)

tab1 <- tabela(pred_klas1,Test$class)
tab1
procent(tab1)
czulosc(tab1)
precyzja(tab1)
roc(pred_praw1,Test$class)
auc(pred_praw1,Test$class)

# sprobujmy automatycznie dobrac parametry:

obj <- tune(svm, class~., data = Train, 
            ranges = list(gamma = 2^(-1:1), cost = 2^(1:4)),
            tunecontrol = tune.control(sampling = "cross")
)
summary(obj)
obj$best.parameters

# dopasujmy model z tymi parametrami:
     
mod_svm2 <- svm(class~., data=Train, type="C", kernel="radial", 
                gamma=obj$best.parameters[1], cost=obj$best.parameters[2], 
                probability=TRUE)
pred2 <- predict(mod_svm2, Test, probability=TRUE)
pred_praw2 <- attr(pred2,"probabilities")[,2]
pred_klas2 <- predict(mod_svm2, Test)

tab2 <- tabela(pred_klas2,Test$class)
tab2
procent(tab2)
czulosc(tab2)
precyzja(tab2)
roc(pred_praw2,Test$class)
auc(pred_praw2,Test$class)

# sprobujmy moze inne jadro:

mod_svm3 <- svm(class~., data=Train, type="C", kernel="linear", 
                gamma=obj$best.parameters[1], cost=obj$best.parameters[2], 
                probability=TRUE)
pred3 <- predict(mod_svm3, Test, probability=TRUE)
pred_praw3 <- attr(pred3,"probabilities")[,2]
pred_klas3 <- predict(mod_svm3, Test)

tab3 <- tabela(pred_klas3,Test$class)
tab3                # wszystko klasyfikuje jako 0, tragedia :(
procent(tab3)
czulosc(tab3)
precyzja(tab3)
roc(pred_praw3,Test$class)
auc(pred_praw3,Test$class)

# sprobujmy jeszcze inne jadro:

mod_svm4 <- svm(class~., data=Train, type="C", kernel="polynomial", 
                gamma=obj$best.parameters[1], cost=obj$best.parameters[2], 
                probability=TRUE)
pred4 <- predict(mod_svm4, Test, probability=TRUE)
pred_praw4 <- attr(pred4,"probabilities")[,2]
pred_klas4 <- predict(mod_svm4, Test)

tab4 <- tabela(pred_klas4,Test$class)
tab4                
procent(tab4)
czulosc(tab4)
precyzja(tab4)
roc(pred_praw4,Test$class)
auc(pred_praw4,Test$class)

# i jeszcze inne:

# sprobujmy moze inne jadro:

mod_svm5 <- svm(class~., data=Train, type="C", kernel="sigmoid", 
                gamma=obj$best.parameters[1], cost=obj$best.parameters[2], 
                probability=TRUE)
pred5 <- predict(mod_svm5, Test, probability=TRUE)
pred_praw5 <- attr(pred5,"probabilities")[,2]
pred_klas5 <- predict(mod_svm5, Test)

tab5 <- tabela(pred_klas5,Test$class)
tab5                
procent(tab5)
czulosc(tab5)
precyzja(tab5)
roc(pred_praw5,Test$class)
auc(pred_praw5,Test$class)

# chyba ostatnie daje najlepsze rezultaty, wiec je zapamietamy:

svm_ost <- vector("list",6)
names(svm_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
svm_ost$tabela <- tab5
svm_ost$procent <- procent(tab5)
svm_ost$czulosc <- czulosc(tab5)
svm_ost$precyzja <- precyzja(tab5)
svm_ost$roc <- list(pred_praw5, Test$class) 
svm_ost$auc <- auc(pred_praw5,Test$class)

svm_ost

########################################################################
###################### METODY LACZENIA DRZEW ###########################
########################################################################

# bagging:

mod_bag <- bagging(class~.,data=Train)
predykcja <- predict.bagging(mod_bag,newdata=Test)
pred_klas <- predykcja$class
pred_praw <- predykcja$prob[,2]

tab <- tabela(pred_klas,Test$class)
tab
procent(tab)
czulosc(tab)
precyzja(tab)
roc(pred_praw,Test$class)
auc(pred_praw,Test$class)

# zapisze potrzebne dane:

bagging_ost <- vector("list",6)
names(bagging_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
bagging_ost$tabela <- tab
bagging_ost$procent <- procent(tab)
bagging_ost$czulosc <- czulosc(tab)
bagging_ost$precyzja <- precyzja(tab)
bagging_ost$roc <- list(pred_praw, Test$class) 
bagging_ost$auc <- auc(pred_praw,Test$class)

bagging_ost

# boosting:

mod_boo <- boosting(class~., data=Train, control = (minsplit = 2))
predykcja <- predict.boosting(mod_boo,newdata=Test)
pred_klas <- predykcja$class
pred_praw <- predykcja$prob[,2]

tab <- tabela(pred_klas,Test$class)
tab
procent(tab)
czulosc(tab)
precyzja(tab)
roc(pred_praw,Test$class)
auc(pred_praw,Test$class)

# zapisze potrzebne dane:

boosting_ost <- vector("list",6)
names(boosting_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
boosting_ost$tabela <- tab
boosting_ost$procent <- procent(tab)
boosting_ost$czulosc <- czulosc(tab)
boosting_ost$precyzja <- precyzja(tab)
boosting_ost$roc <- list(pred_praw, Test$class) 
boosting_ost$auc <- auc(pred_praw,Test$class)

boosting_ost

# las losowy:

mod_las <- randomForest(class~., data=Train)
pred_praw <- predict(mod_las,newdata=Test, type="prob")[,2]
pred_klas <- predict(mod_las,newdata=Test, type="response")

tab <- tabela(pred_klas, Test$class)
tab 

procent(tab)
czulosc(tab)
precyzja(tab)

# krzywa ROC i wspolczynnik AUC:

roc(pred_praw, Test$class)
auc(pred_praw, Test$class)

# wszysko w jednym 

las_ost <- vector("list",6)
names(las_ost) <- c("tabela", "procent", "czulosc", "precyzja", "roc", "auc")
las_ost$tabela <- tab
las_ost$procent <- procent(tab)
las_ost$czulosc <- czulosc(tab)
las_ost$precyzja <- precyzja(tab)
las_ost$roc <- list(pred_praw, Test2$class) 
las_ost$auc <- auc(pred_praw, Test2$class)

las_ost

#########################################################################
###################### TABELA Z WNIOSKAMI ###############################
#########################################################################
















