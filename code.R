################################################
#instructions:
#set working directory on line 32
#From line 10 to 115 is data pre-processing and amputation. 
#Run the code from 10-118 to load data from excel files

#119-179 is random forest model which needs not to be run for training
#trained model objects are provided.

#Run the model from 180 till the end to obtain the result for
#sensitivity, error rate and confusion matrix.

################################################
set.seed(150)
#Setup
#-------------
library(tree)
library(ROSE)
library(randomForest)

#------------------------
cfm2err<-function(cnfMat){
  ErrorRate = (cnfMat[1,2]+cnfMat[2,1])/sum(cnfMat)
  return(ErrorRate)
}
cfm2sensitivity<-function(cnfMat){
  Sensitivity = (cnfMat[2,2])/(cnfMat[2,2]+cnfMat[1,2])
  return(Sensitivity)
}

#setwd("//blender/homes/w/a/wajahata99/nt/wajahat/613/Project")
setwd("C:/A/MEEN-ISEN-613/Project")
#getwd()

#------------------------------
dat_name="train_set.csv"

dat0<-read.csv(dat_name)
dat0<-data.frame(dat0)

C_Claim=dat0$Claim_Amount>0
dat0$C_Claim<-as.factor(C_Claim)
dim(dat0)
dat0<-dat0[,-c(1,2,6,7,35)]

#split submodel col and keeping only the last one
Blind_Submodel_split<-strsplit(as.character(dat0$Blind_Submodel),split="\\.")
dat0$Blind_Submodel=lapply(Blind_Submodel_split, function(l) l[[3]])
dat0 <- transform(dat0,Blind_Submodel=as.factor(unlist(dat0$Blind_Submodel)))

dat.train<-dat0

# PCA for dim reduction
col<-c(18:25)#qantitative var
dat.pca = prcomp(dat.train[,col], scale. = TRUE)

#variance explained by PCs
plot(cumsum(dat.pca$sdev^2/sum(dat.pca$sdev^2)))
#keeping only 20 variables which are explaining about 90 percent of variance
dat.pc.trunc<-dat.pca$x[,1:4]
dat.pc<-dat0
dat.pc[,col]<-NULL
dat.pc.train<-cbind(dat.pc,dat.pc.trunc)

#---------------
#Test data importing and processing
dat_name="test_set07.csv"

dat0.test<-read.csv(dat_name)
dat0.test<-data.frame(dat0.test)

C_Claim=dat0.test$Claim_Amount>0
dat0.test$C_Claim<-as.factor(C_Claim)
dat0.test<-dat0.test[,-c(1,2,6,7,35)]

#split submodel col and keeping only the important info
Blind_Submodel_split<-strsplit(as.character(dat0.test$Blind_Submodel),split="\\.")
dat0.test$Blind_Submodel=lapply(Blind_Submodel_split, function(l) l[[3]])
dat0.test <- transform(dat0.test,Blind_Submodel=as.factor(unlist(dat0.test$Blind_Submodel)))

dat.test<-dat0.test

# PCA for dim reduction
col<-c(18:25)#qantitative var

dat.pca.test = prcomp(dat.test[,col], scale. = TRUE)

#variance explained by PCs
plot(cumsum(dat.pca.test$sdev^2/sum(dat.pca.test$sdev^2)))

dat.pc.test<-dat.test
dat.pc.test[,col]<-NULL

#keeping only variables which are explaining about 90 percent of variance
dat.pc.test<-cbind(dat.pc.test,dat.pca.test$x[,1:4])

#-----------------------------
#balancing

prob.minority.pc=0.1
prob.minority=.1

#ROSE on before PCA
dat.train.rs=ROSE(C_Claim~.,data=dat.train,seed=10,p=prob.minority)$data
table(dat.train.rs$C_Claim)
#ROSE after PCA
dat.pc.train.rs=ROSE(C_Claim~.,data=dat.pc.train,seed=10,p=prob.minority.pc)$data
table(dat.pc.train.rs$C_Claim)

#------------------------------------------
## Random forest library has some problem of giving
#false error of class missmatch. FOund the following 
#way around on internet to address the bug.
dat.pc.test <- rbind(dat.pc.train.rs[1, ] , dat.pc.test)
dat.pc.test <- dat.pc.test[-1,]

dat.test <- rbind(dat.train.rs[1, ] , dat.test)
dat.test <- dat.test[-1,]

#Random Forest
#--------
#applying random forest on simplified blind 
#submodel and excluding blind make and submodel
#as random forest can't hanndle more than 53 levels
#in categorical variables, keeping only 4 continuous
#variables after PCA.

#Trained models are provided because training is computationaly
#expensive. However, if training is needed
#uncomment the following

#rf.pc=randomForest(C_Claim~.,data=dat.pc.train.rs,mtry=round(ncol(dat.pc.train.rs)-1,0),ntree=200)
#save(rf.pc, file = "rf_pc4_rs10_2_LI300.rda")
#load("rf_pc4_rs0101.rda")# comment it if model is being trained
#rf.pc.predict=predict(rf.pc,newdata=dat.pc.test[,-col.low_imp])

cmat=table(rf.pc.predict ,dat.pc.test$C_Claim)
cmat
#save(cmat, file = "cmat_pc4_rs20.rda")

cfm2err(cmat)
cfm2sensitivity(cmat)

#varification
rf.pc.err=1-(sum(rf.pc.predict==dat.pc.test$C_Claim)/nrow(dat.pc.test))
rf.pc.sen=sum(rf.pc.predict==T & dat.pc.test$C_Claim==T)/(sum(dat.pc.test$C_Claim==T))

#------------------------------------------------------
#Random Forest, no PCA

rf=randomForest(C_Claim~.,data=dat.train.rs,mtry=round(ncol(dat.train.rs)-1,0),ntree=200)
save(rf, file = "rf_rs5.rda")
#load("rf.rda")

rf.predict=predict(rf,newdata = dat.test)
cmat=table(rf.predict ,dat.test$C_Claim)

cfm2err(cmat)
save(cmat, file = "cmat_rs_p10.rda")
cfm2sensitivity(cmat)
#------------------------------------------------------

#finding optimal number of "m" with PCA
#############
#running the model is expensive. training model saved and provided
#can be loaded inf required
#############
for(i in 1:ncol(dat.pc.train.rs)){
  rf.pc =randomForest(C_Claim~.,data=dat.pc.train.rs ,
                      mtry=i, ntree=200,importance =TRUE)
  
  rf.pc.pred = predict (rf.pc ,newdata =dat.pc.test)
  filename=paste("rf.pc.mod",i)
  save(rf.pc, file=filename)
}

##########################################################
#Ploting error rate and sensitivity from the saved models
rf.pc.err=rep(0,ncol(dat.train.rs))
rf.pc.sen=rep(0,ncol(dat.train.rs))

for(i in 1:ncol(dat.pc.train.rs)){
  
  filename=paste("rf.pc.mod",i)
  load(filename)
  
  rf.pc.pred = predict (rf.pc ,newdata =dat.pc.test)
  cmat=table(rf.pc.pred, dat.pc.test$C_Claim)
  rf.pc.err[i]=cfm2err(cmat)
  rf.pc.sen[i]=cfm2sensitivity(cmat)
}

plot(rf.pc.err,xlab = "m",ylab="Error Rate")
plot(rf.pc.sen,xlab = "m",ylab="Sensitivity")

#------------------------------------------------------
# best m is found to be 9 and 10
# results for m = 10
load("rf.pc.mod 10")
rf.pc.pred = predict (rf.pc ,newdata =dat.pc.test)
importance(rf.pc)
cmat=table(rf.pc.pred, dat.pc.test$C_Claim)
cmat
cfm2err(cmat)
cfm2sensitivity(cmat)

# results for m=9
load("rf.pc.mod 9")
rf.pc.pred = predict (rf.pc ,newdata =dat.pc.test)
cmat=table(rf.pc.pred, dat.pc.test$C_Claim)
cmat
cfm2err(cmat)
cfm2sensitivity(cmat)


#------------------------------------------------------
#finding the optimal number of tree: ntree

ntree=seq(from=10, to=300, by=10)

rf.pc.err.t=rep(0,ncol(dat.train.rs))
rf.pc.sen.t=rep(0,ncol(dat.train.rs))

for(i in 1:length(ntree)){
  rf.pc =randomForest(C_Claim~.,data=dat.pc.train.rs ,
                      mtry=9, ntree=i,importance =TRUE)
  
  rf.pc.pred = predict (rf.pc ,newdata =dat.pc.test)
  
  cmat=table(rf.pc.pred, dat.pc.test$C_Claim)
  rf.pc.err.t[i]=cfm2err(cmat)
  rf.pc.sen.t[i]=cfm2sensitivity(cmat)
  filename=paste("rf.pc.mod.t",i)
  save(rf.pc, file=filename)
}

plot(rf.pc.err.t,xlab = "ntree x 10",ylab="Error Rate")
plot(rf.pc.sen.t,xlab = "ntree x 10",ylab="Sensitivity")

