#### Cross validation script by frog

# We need a train dataset and a label variable 

traincv <- cbind(label, train)
traincvs <- cbind(label,trains)


library(verification)

k = 10 # If you touch this you are odd


n=floor(nrow(traincv)/k)
err.vect = rep (NA,k)

for (i in 1:k) {
    s1 <- ((i-1)* n+1)
    s2 <- (i*n)
    subset <- s1:s2
    cv.traincv <- traincv[-subset,]
    cv.traincvs <- traincvs[-subset,]
    cv.test <- traincv[subset,]
    cv.tests <- traincvs[subset,]
    
    #### FITTIN MODELS
    
    #### RANDOM FOREST
    fit <- randomForest(x=cv.traincv[,-1],y=as.factor(cv.traincv[,1]),ntree=300, mtry=8)
    prediction <- predict(fit,cv.test[,-1])
    
    #### C5.0
    #cfit <- C5.0(cv.traincv[,-1],as.factor(cv.traincv[,1]))
    #prediction <- predict(cfit,cv.test[,-1])
    
    #### SVM
    #sfit <- svm(x=cv.traincvs[,-1],y=as.factor(cv.traincvs[,1]),gamma=0.5,cost=8)
    #prediction <- predict(sfit,cv.tests[,-1])
    
    #### Cforests
    #cfit <- cforest(cover_type~Elevation+Horizontal_Distance_To_Hydrology+Vertical_Distance_To_Hydrology+
     #                   Horizontal_Distance_To_Fire_Points + Horizontal_Distance_To_Roadways +
      #                  Hillshade_9am + Hillshade_Noon + Hillshade_3pm,
       #             data=cv.traincv,controls=cforest_unbiased(ntree=20))
    #prediction=predict(cfit, cv.testcv[,-1], OOB=TRUE, type = "response")
    #### KNN
    #prediction <- knn(cv.traincvs[,-1],cv.tests[,-1],as.factor(cv.traincvs[,1]),k=5,prob=TRUE)
    
    #### MAKE PREDICTION USE SCALED OR NORMAL DATA FRAME AND NOT FOR KNN!
    #prediction <- predict(fit,cv.tests[,-1])
    #prediction <- predict(fit,cv.test[,-1])
    
    
    err.vect[i]=sum(cv.test[,1]!=prediction)/length(prediction)
    #err.vect[i]=sum(cv.tests[,1]!=prediction)/length(prediction)
    
    print (paste("Error for fold",i,":",err.vect[i]))
    
}
print (paste("Average Error:",mean(err.vect)))

#rm(traincv)