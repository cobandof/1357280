###
###SECTION 1 PACKAGES
###

#Set working directory for the project
workspace <- "C:/Users/Catalina/SkyDrive/Documents/Data mining/Week 10"
setwd(workspace)

#Get prebuilt corpus package
#install.packages("~/R/win-library/3.0/tm.corpus.Reuters21578_0.0-6.tar.gz", repos = NULL, type = "source")
require(tm)
require(tm.corpus.Reuters21578) # This two packages (<- and above) are used to import the data
require(SnowballC) # Used to do stemming
require(rJava) # Used by other pachages such as FSelector
require(FSelector) # Used to apply different feature selction techniques
library(e1071) # Used for the SVM and NB
library(randomForest) #Used for the RF
library(MASS)
library(SDMTools)
library(caret)
library("slam")
library("topicmodels")# This two packages (<- and above) are used for topic models
library(proxy) # Used for clustering
library(mclust) #Used for clustering
require("clusteval") #Measure of cluster cuality

data(Reuters21578)
rt <- Reuters21578

###---------------------------------------------------------------------------
###SECTION 2 PRE-PROCESSING
###
## Convert to Lower Case
rt <- tm_map(rt, tolower)

## Remove Stopwords Punto 6. de pre-processing
rt <- tm_map(rt, removeWords, stopwords("english"))

## Remove Punctuations Punto 3. the pre-processing
rt <- tm_map(rt, removePunctuation)

## Stemming Usa el paquete SnowballC Punto 5, de pre-processing
rt <- tm_map(rt, stemDocument)

## Remove Numbers
rt <- tm_map(rt, removeNumbers)

## Eliminating Extra White Spaces
rt <- tm_map(rt, stripWhitespace)

#Tokenize
rt<-MC_tokenizer(rt)
#####################################################------------------------------------------------------

##------------------------------------------------------------------------------------------------------
##Split corpus into a training set and a testing set, and generates de classes using the
##ten most popular classes
nn<-length(rt)
##tm::meta(rt[[2]]) Con esto se ven los tags de el documento 2
C<-matrix(0,1,12) # Matrix with tranning documents and classes for each of the topic classes 
Cs<-matrix(0,1,12) # Matrix with testing documents and classes for each of the topic classes
tpcs<-c("earn", "acq", "money-fx", "grain","crude","trade","interest","ship","wheat","corn")
k<-1
j<-1
for (i in 1:nn){
  if(tm::meta(rt[[i]],tag="TOPICS")=="YES"){
    if(length(tm::meta(rt[[i]],tag="Topics"))>0){
      if(length(strsplit(rt[[i]][1],' ')[[1]])>0){
        if(tm::meta(rt[[i]],tag="LEWISSPLIT")=="TRAIN"){
          C[k,1]<-i
          C[k,2]<-1
          
          for(t in 1:length(tpcs)){
            if(tpcs[t] %in%  tm::meta(rt[[i]],tag="Topics"))
            {
              C[k,t+2]<-1
            }
          }
          C<-rbind(C, c(0,0,0,0,0,0,0,0,0,0,0,0))
          k<-k+1
        
        }
        else if(tm::meta(rt[[i]],tag="LEWISSPLIT")=="TEST"){
          Cs[j,1]<-i
          Cs[j,2]<-0
          for(t in 1:length(tpcs)){
            if(tpcs[t] %in%  tm::meta(rt[[i]],tag="Topics"))
            {
              Cs[j,t+2]<-1
            }
          }
          Cs<-rbind(Cs, c(0,0,0,0,0,0,0,0,0,0,0,0))
          j<-j+1
        }
      } 
    } 
  } 
}

View(C)
C<-C[-dim(C)[1],] #Matrix with the binary classes for the tranning set
Cs<-Cs[-dim(Cs)[1],] #Matrix with the binary classes for the test set
BiClass<-rbind(C,Cs) # Matrix with binary classes for test and tranning set
reordered <- sample(nrow(BiClass))
BiClass = BiClass[reordered,]

#----------------------------------------------------------------------------------------------

## create a term document matrix
dtm <- DocumentTermMatrix(rt)
inspect(dtm[,1:20])
DTM<-dtm[BiClass[,1],,]

dtmTr<-dtm[C[,1],,] # Tranning Document term matrix
dtmTs<-dtm[Cs[,1],,]# Test Document term matrix

DTM<-DTM[,findFreqTerms(dtm, 1000),]
dtmTr=dtmTr[,findFreqTerms(dtm, 1000),] 
dtmTs=dtmTs[,findFreqTerms(dtm, 1000),] 
dDoc<-as.data.frame(as.matrix(dtmTr))
dDocTs<-as.data.frame(as.matrix(dtmTs))
d<-dDoc
dTst<-dDocTs
d$Class1<-C[,3]
dTst$Class1<-Cs[,3]


###---------------------------------------------------------------------------
###SECTION 3 FEATURE ENGINEERING
###

#########---------------
#3.1 Feature Selection
featureSele<-function(FSfunction){
  #calculate weights for each atribute using some function
  weights <- FSfunction(Class1~., d)
  print(weights)
  
  #select a subset of 5 features with the lowest weight
  subset <- cutoff.k(weights, 30)
  
  #print the results
  #f <- as.simple.formula(subset, "Class")
  #print(f)
  return(subset)
  
}
Resfs<-featureSele(chi.squared)
Resfs<-featureSele(linear.correlation) # Winner!
Resfs<-featureSele(information.gain)

d<-d[,Resfs]
d$Class1<-C[,3]



#########----------------------
#3.2 Topic Models

#k <- length(tpcs)
k<-30
SEED <- 2014
#Fit topic models
rts_TM <- list(VEM = LDA(dtmTr, k = k, control = list(seed = SEED)),
               VEM_fixed = LDA(dtmTr, k = k,
                               control = list(estimate.alpha = FALSE, seed = SEED)
               ),
               Gibbs = LDA(dtmTr, k = k, method = "Gibbs",
                           control = list(seed = SEED, burnin = 1000,
                                          thin = 100, iter = 1000)
               )
)
#Posterior matrix
ResTm<-posterior(rts_TM$VEM)$topics
ResTm<-posterior(rts_TM$VEM_fixed)$topics
ResTm<-posterior(rts_TM$Gibbs)$topics

TMdataframe<-as.data.frame(ResTm)
TMdataframe$Class1<-C[,3]
TMdataframe$fold = cut(1:nrow(d), breaks=10, labels=F)
d<-TMdataframe
d<-as.matrix(d)

###---------------------------------------------------------------------------
###SECTION 4 CLASSIFICATION PROCESS
###
d$fold = cut(1:nrow(d), breaks=10, labels=F)

resultsClssifiers<-function(dtmTr,C){#d es document term matrix as data frame; C es la matriz pon las variables dummies de las clases
  
  #dDoc<-as.data.frame(as.matrix(dtmTr))
  #d<-dDoc
  #d$Class1<-C[,1]
  #d$fold = cut(1:nrow(d), breaks=10, labels=F)
  d<-dtmTr
  Ma<-vector("list",10)
  Mp<-vector("list",10)
  Mr<-vector("list",10)
  
  M<-vector("list",3)
  
  for(m in 1:10){
    #d<-dDoc
    d$Class1<-C[,m]
    #d$fold = cut(1:nrow(d), breaks=10, labels=F)
    accuracy<-matrix(0,12,3)
    precision<-matrix(0,12,3)
    recall<-matrix(0,12,3)
    
    for(i in 1:10){#Fold
      
      trainset=d[d$fold!=i,]
      testset=d[d$fold==i,]
      svm.model <- svm(Class1 ~ ., data = trainset[,1:31],type="C-classification") #, cost = 100, gamma = 0.01)#,cross=10)
      svm.pred <- predict(svm.model, testset[,1:30])
      
      classifier<-naiveBayes(trainset[,1:30], as.factor(trainset[,31]))
      classifier.pred<-predict(classifier, testset[,1:30])
      
      model.rf <- randomForest(trainset[,1:30], as.factor(trainset[,31]), keep.forest=TRUE)
      rf.pred<-predict(model.rf,testset[,1:31], keep.forest=TRUE)
      
      ressvm=table(true = testset[,31],pred = svm.pred)
      resnb=table(predict(classifier, testset[,1:30]), testset[,31])##, dnn=list('predicted','actual'))
      resrf<-table(true = testset[,31],pred = rf.pred) 
      
      CM<-vector("list",3)
      CM[[1]] <- confusionMatrix(ressvm,positive="1")
      CM[[2]] <- confusionMatrix(resnb, positive="1")
      CM[[3]] <- confusionMatrix(resrf,positive="1")
      
      for(j in 1:3)
      {
        accuracy[i,j] <- as.numeric(CM[[j]]$overall["Accuracy"])
        precision[i,j] <- as.numeric(CM[[j]]$byClass["Pos Pred Value"])
        recall[i,j] <- as.numeric(CM[[j]]$byClass["Sensitivity"])
        #f1[i,j] <- as.numeric((2*recall[i,j]*precision[i,j])/(recall[i,j]+precision[i,j]))
      }
      
    }
    for(j in 1:3)
    {
      accuracy[11,j] <- mean(accuracy[1:10,j])
      accuracy[12,j] <- sd(accuracy[1:10,j])
      
      precision[11,j] <- mean(precision[1:10,j])
      precision[12,j] <- sd(precision[1:10,j])
      
      recall[11,j] <- mean(recall[1:10,j])
      recall[12,j] <- sd(recall[1:10,j])
    }
    
    Ma[[m]]<-accuracy
    Mp[[m]]<-precision
    Mr[[m]]<-recall
    
    
  }
  M[[1]]<-Ma
  M[[2]]<-Mp
  M[[3]]<-Mr
  
  return(M)
 
  
}

classFS<-resultsClssifiers(d,C[,3:12]) # Classification: Data set with feature selection done by linear correlation



######################------------------------------------------------------------------------------------
#Winner model: SVM trainned over topic models (CHi-SQUARE)


###---------------------------------------------------------------------------
###SECTION 5 EVALUATION OF CLASIFICATION
###

d# data frame with tranning documents
dTst# data frame with trst documents 
d<-d[,Resfs] # Selects the features given by chi-squear
dTst<-dTst[,Resfs] # Same

GR<-matrix(0,10,5)
for(m in 1:10){
  
  #d<-dDoc
  d$Class1<-C[,m+2]
  dTst$Class1<-Cs[,m+2]
  svm.model <- svm(Class1 ~ ., data = d,type="C-classification")
  svm.pred <- predict(svm.model, dTst[,1:30])
  
  ressvm=table(true = dTst[,31],pred = svm.pred)
  
  CM<- confusionMatrix(ressvm,positive="1")
  GR[m,1]<-as.numeric(CM$overall["Accuracy"])
  GR[m,2]<-as.numeric(CM$overall["Pos Pred Value"])
  GR[m,3]<-as.numeric(CM$overall["Sensitivity"])
  GR[m,4]<-ressvm[1,1]
  GR[m,5]<-ressvm[2,1]+ressvm[1,2]
  
}

###---------------------------------------------------------------------------
###SECTION 6 CLUSTERING PROCESS
###

## create a term document matrix
dtm <- DocumentTermMatrix(reuters)
# DTM is the document term matrix with documents tag either as TEST or TRAIN
dtmC<- as.matrix(DTM)
dtmC<-dtmC[,Resfs]

## do document clustering

### k-means (this uses euclidean distance)-------------
mC<- as.matrix(dtmC)
rownames(mC) <- 1:nrow(mC)
### Cluster into 10 clusters
cl <- kmeans(mC, 10)
clm<-as.matrix(cl$cluster)# CLUSTER 1
table(cl$cluster)
### show clusters using the first 2 principal components
plot(prcomp(mC)$x, col=cl$cl)

findFreqTerms(DTM[cl$cluster==1,], 50)
inspect(rt[which(cl$cluster==1)])



## hierarchical clustering--------------
### this is going to take 4-ever (O(n^2))
cld <- dist(mC, method="euclidean") # More methods in summary(pr_DB)
hc <- hclust(cld, method="ward")
plot(hc)

plot(hc, labels = NULL, hang = 0.1,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height") # display dendogram
rect.hclust(hc, k=10, border="red")

clH <- cutree(hc, 10)
plot(clH)
clhM<-as.matrix(clH) #CLUSTER 2
table(cl)
findFreqTerms(dtm[cl==1], 50)
plot(prcomp(mC)$x, col=clH)


# EM clustering----------------------------
clEm <- Mclust(mC,G=10)
plot(prcomp(mC)$x, col=clEm$cl)

###---------------------------------------------------------------------------
###SECTION 7 EVALUATION OF CLUSTERING
###

clusterCorr<-function(classes,clusters){
  clusterCorr<-matrix(0,10,11)
  colnames(clusterCorr)=c("earn","acq","money-fx","grain","crude","trade","interest","ship","wheat","corn","others")
    
  for(i in 1:dim(classes)[1])
  {
    if (sum(classes[i,])==0){clusterCorr[clusters[i],11]=clusterCorr[clusters[i],11]+1; } 
    else
    {clusterCorr[clusters[i],1:10]= clusterCorr[clusters[i],1:10]+classes[i,1:10]}
    
  }
  for(i in 1:10)
  {
    clusterCorr[i,]=clusterCorr[i,]/length(which(clusters==i))
  }
  return(clusterCorr)
  
}

cor1<-clusterCorr(BiClass[,3:12],clm)
cor2<-clusterCorr(BiClass[,3:12],clhM)
cor3<-clusterCorr(BiClass[,3:12],clEm$cl)
View(clhM)
plot(clEm$cl)

require("clusteval")
meassureCl<-function(cluster){
  meassureCl<-matrix(0,10,1)
  for(i in 1:10){
    
    meassureCl[i,1]<-cluster_similarity(cluster,BiClass[,i+2])
    
    #meassureCl[i,2]<-comembership_table(cluster,BiClass[,i+2])
  }
  return(meassureCl)
}
meassureCl(clm)
meassureCl(clhM)
meassureCl(clEm$cl)



##########################-----------------------------------------------------------------
