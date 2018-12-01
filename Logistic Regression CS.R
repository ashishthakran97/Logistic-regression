#Importing the file
file <- read.csv('file:///E:/ANALYTIXLABS/DATA SCIENCE USING R/BA CLASSES/CLASS 14/Proactive Attrition Management-Logistic Regression Case Study/Proactive Attrition Management-Logistic Regression Case Study.csv')
str(file)
  
#how do i replace a word with another at a time so that all the words get replaced with the same.?
#UDF
mystat <- function(x){
  
    a <- x[!is.na(x)]
    n <- length(a)
    nmiss <- sum(is.na(x))
    mean <- mean(a)
    std <- sd(a)
    var <- var(a)
    min <- min(a)
    p1 <- quantile(a,.01)
    p5 <- quantile(a,.05)
    p10 <- quantile(a,.10)
    p25<- quantile(a,.25)
    p50<- quantile(a,.50)
    p75<- quantile(a,.75)
    p90<- quantile(a,.90)
    p95<- quantile(a,.95)
    p99<- quantile(a,.99)
    max <- max(a)
    outlier <- max>p95|min<p5
    return(c(length=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,p25=p25,p50=p50,
             p75=p75,p90=p90,p95=p95,p99=p99,max=max,outlier=outlier))
     }
  
file1<- file[,c(1:25,27:29,31:77)]

#understanding the data
daig_stats <- t(data.frame(apply(file1[,],2,mystat)))
write.csv(daig_stats,'daig_stats.csv')

#outlier treatment
M1_fun <- function(x){
  quantiles <- quantile( x, c(.05, .95 ),na.rm=TRUE )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

file1 <- data.frame(apply(file1[,],2,FUN=M1_fun))
file1$EQPDAYS[file1$EQPDAYS==865.75] <- 866


#missing values
apply(is.na(file1[,]),2,sum)
file2 <- apply(data.frame(file1[,]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})





apply(is.na(file2[,]),2,sum)
file3 <- cbind(data.frame(file2),churndep=file$CHURNDEP)


#rm(file3)
#colnames(file2)[colnames(file2)=='file$CHURNDEP'] <- 'CHURNDEP'  #changing column name






#####selecting variables using CHI-SQUARE #########



#chisq.test(xtabs(CHURNDEP~CHURN,data = file3))

c3 <- xtabs(CHURN~UNIQSUBS,data = file3)
c4 <- xtabs(CHURN~ACTVSUBS,data = file3)
c5 <- xtabs(CHURN~PHONES,data = file3)
c6 <- xtabs(CHURN~MODELS,data = file3)
c7 <- xtabs(CHURN~CHILDREN,data = file3)
c8 <- xtabs(CHURN~CREDITA,data = file3)
c9 <- xtabs(CHURN~CREDITAA,data = file3)
c10 <- xtabs(CHURN~CREDITB,data = file3)
c11<- xtabs(CHURN~CREDITC,data = file3)
c12<- xtabs(CHURN~CREDITDE,data = file3)

c16<- xtabs(CHURN~PRIZMUB,data = file3)
c17<- xtabs(CHURN~PRIZMTWN,data = file3)
c18<- xtabs(CHURN~REFURB,data = file3)
c19<- xtabs(CHURN~WEBCAP,data = file3)
c20<- xtabs(CHURN~TRUCK,data = file3)
c21<- xtabs(CHURN~RV,data = file3)
c22<- xtabs(CHURN~OCCPROF,data = file3)

c30<- xtabs(CHURN~MARRYUN,data = file3)
c31<- xtabs(CHURN~MARRYYES,data = file3)
c32<- xtabs(CHURN~MARRYNO,data = file3)
c33<- xtabs(CHURN~MAILORD,data = file3)
c34<- xtabs(CHURN~MAILRES,data = file3)

c36<- xtabs(CHURN~TRAVEL,data = file3)
c37<- xtabs(CHURN~PCOWN,data = file3)
c38<- xtabs(CHURN~CREDITCD,data = file3)

c41<- xtabs(CHURN~NEWCELLY,data = file3)
c42<- xtabs(CHURN~NEWCELLN,data = file3)

c44<- xtabs(CHURN~INCMISS,data = file3)
c45<- xtabs(CHURN~INCOME,data = file3)

c48<- xtabs(CHURN~SETPRCM,data = file3)
c49<- xtabs(CHURN~SETPRC,data = file3)


chisq.test(c3)
chisq.test(c4)
chisq.test(c5)
chisq.test(c6)
chisq.test(c7)
chisq.test(c8)
chisq.test(c9)
chisq.test(c10)
chisq.test(c11)
chisq.test(c12)

chisq.test(c16)
chisq.test(c17)
chisq.test(c18)
chisq.test(c19)
chisq.test(c20)
chisq.test(c21)
chisq.test(c22)

chisq.test(c30)
chisq.test(c31)
chisq.test(c32)
chisq.test(c33)
chisq.test(c34)

chisq.test(c36)
chisq.test(c37)
chisq.test(c38)

chisq.test(c41)
chisq.test(c42)

chisq.test(c44)
chisq.test(c45)

chisq.test(c48)
chisq.test(c49)

require(car)
require(MASS)

#MODEL BUILDING USING GLM()

fit <- glm(CHURN~REVENUE+MOU+    
            
           RECCHRGE+
           DIRECTAS+
           OVERAGE+
           ROAM+
           CHANGEM+
           CHANGER+
           DROPVCE+
           BLCKVCE+
           UNANSVCE+
           CUSTCARE+
           THREEWAY+
           MOUREC+
           OUTCALLS+
           INCALLS+
           PEAKVCE+
           OPEAKVCE+
           DROPBLK+
           
           CALLWAIT+
          
           MONTHS+
           UNIQSUBS+
           ACTVSUBS+
           PHONES+
           MODELS+
           EQPDAYS+
           AGE1+
           AGE2+
           CHILDREN+
           CREDITA+
           CREDITAA+
           CREDITB+
           CREDITC+
           CREDITDE+
          
           PRIZMUB+
           PRIZMTWN+
           REFURB+
           WEBCAP+
           TRUCK+
           RV+
           OCCPROF+
          
           OWNRENT+
           MARRYUN+
           MARRYYES+
          
           MAILORD+
           MAILRES+
         
           TRAVEL+
           PCOWN+
           CREDITCD+
           
          
           NEWCELLY+
           NEWCELLN+
          
           INCMISS+                   
           INCOME+
          
           SETPRCM+
           SETPRC
          ,data=file3,family = binomial(logit))
summary(fit)



fit1 <- glm(CHURN~REVENUE+MOU+RECCHRGE+OVERAGE+ROAM+CHANGEM+
              CHANGER+DROPVCE+CUSTCARE+THREEWAY+INCALLS+PEAKVCE+CALLWAIT+
              MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+CHILDREN+CREDITAA+CREDITB+CREDITDE+PRIZMUB+
              REFURB+WEBCAP+file3$MARRYUN+
              SETPRC,data=file3,family = binomial(logit))
summary(fit1)

# standardising the data
file3$MOU <- sqrt(file3$MOU)
file3$OVERAGE <- sqrt(file3$OVERAGE)
file3$PEAKVCE <- sqrt(file3$PEAKVCE)
file3$EQPDAYS <- sqrt(file3$EQPDAYS)
file3$DROPVCE <- sqrt(file3$DROPVCE)
file3$INCALLS <- sqrt(file3$INCALLS)
file3$MONTHS <- sqrt(file3$MONTHS)
file3$SETPRC <- sqrt(file3$SETPRC)

#splitting the data
train <- file3[!(is.na(file3$churndep)),]
test <- file3[is.na(file3$churndep),]

sum(is.na(test))

fit2 <- glm(CHURN~MOU+OVERAGE+ROAM+CHANGEM+
              CHANGER+DROPVCE+INCALLS+PEAKVCE+
              MONTHS+UNIQSUBS+ACTVSUBS+PHONES+EQPDAYS+AGE1+CHILDREN+CREDITAA+CREDITB+CREDITDE+PRIZMUB+
              REFURB+WEBCAP+
              SETPRC,data=train,family = binomial(logit))

summary(fit2)
source("file:///E:/ANALYTIXLABS/DATA SCIENCE USING R/BA CLASSES/CLASS 14/Logistic/Concordance.R")
Concordance(fit2) #62.175% 
vif(fit2) 

#decile analysis
train1 <- cbind(train,prob=predict(fit2,type = 'response'))
deloc <- quantile(train1$prob,probs=seq(.1,.9,by=.1))
train1$decile <- findInterval(train1$prob,c(-Inf,deloc,Inf))
train1$decile <- factor(train1$decile)

require(dplyr)
decile_grp <- group_by(train1,decile)%>%summarize( total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(CHURN), 
                                                  non_default_cnt=total_cnt -default_cnt )
decile_grp <- arrange(decile_grp,desc(decile_grp$decile))
write.csv(decile_grp,'fit_train_DA.csv')


test1 <- cbind(test,prob=predict(fit2,test,type = 'response'))
# test1$prob
deloc <- quantile(test1$prob,probs=seq(.1,.9,by=.1))


test1$decile <- findInterval(test1$prob,c(-Inf,deloc,Inf))

test1$decile <- factor(test1$decile)
summary(test1$decile)
#require(sqldf)
#R1<- sqldf("select decile, count(prob) as total, min(prob) as mini, max(prob) as maxx, sum(churn) as churner from test1 group by decile order by decile")


decile_grp1 <- group_by(test1,decile)%>%summarize( total_cnt=n(), min_prob=min(p=prob), max_prob=max(prob), default_cnt=sum(CHURN), 
                                                   non_default_cnt=total_cnt -default_cnt )
decile_grp1 <- arrange(decile_grp1,desc(decile_grp1$decile))

write.csv(decile_grp1,'fit_test_DA.csv')

#predicting for new customers
#test$model_churn <- cbind(test,prob=predict(fit2,test,type = 'response')) not needed

 


#confusion matrix
train1$model_churn <- ifelse(train1$prob>0.530691864,1,0)
test1$model_churn <- ifelse(test1$prob>0.536119884,1,0)
#table(train1$prob>0.530691864,train1$model_churn)
#table(test1$prob>0.536119884,test1$model_churn)

table(train1$CHURN,train1$model_churn) #for sensitivity and specificity.
table(test1$CHURN,test1$model_churn) #for sensitivity and specificity.

#AUC
require(ROCR)
pred_train_fit2 <- prediction(train1$prob, train1$CHURN)  
perf_fit2 <- performance(pred_train_fit2, "tpr", "fpr")
plot(perf_fit2)
abline(0, 1)
performance(pred_train_fit2, "auc")@y.values     
