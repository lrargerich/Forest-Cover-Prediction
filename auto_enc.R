library(randomForest)
library (autoencoder)

learn_tree <- train
#plot(train$Cover_Type, train$Elevation)
set.seed(1332);
nsamples <- 15120; 
sample.learn_tree <- learn_tree[sample(nrow(learn_tree), nsamples), ]; 

#--------------------------------------------------------------------
# split the dataset into training and test
#--------------------------------------------------------------------
ratio <- 0.7;
train.samples <- ratio*nsamples;
train.rows <- c(sample(nrow(sample.learn_tree), trunc(train.samples)));
train.set  <- sample.learn_tree[train.rows, ];
test.set   <- sample.learn_tree[-train.rows, ];

## Set up the autoencoder architecture:
nl=3 ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic" ## specify the network unit type, i.e., the unit
N.input = 44 ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 8 ## number of units in the hidden layer

lambda = 0.0005 ## weight decay parameter
beta = 6 ## weight of sparsity penalty term
rho = 0.01 ## desired sparsity parameter
epsilon <- 0.001 ## a small parameter for initialization of weights
## as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 100000 ## number of iterations 


trainmatrix = data.matrix(train.set[,cols], rownames.force = NA)
allmatrix = data.matrix(learn_tree[,cols], rownames.force = NA)
testmatrix = data.matrix(test.set[,cols], rownames.force = NA)

#cols2 <- names(train.set)[2:55]

autoencoder.object <- autoencode(X.train=trainmatrix,X.test=testmatrix, nl=nl,N.hidden=N.hidden,
                                 unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
                                 optim.method="BFGS",max.iterations=max.iterations)
#                                 ,rescale.flag=TRUE,rescaling.offset=0.001)

#output <- predict(autoencoder.object, X.input=trainmatrix, hidden.output=FALSE)$X.output
output2 <- predict(autoencoder.object, X.input=trainmatrix, hidden.output=TRUE)$X.output
output3 <- predict(autoencoder.object, X.input=allmatrix, hidden.output=TRUE)$X.output
output_test <- predict(autoencoder.object, X.input=testmatrix, hidden.output=TRUE)$X.output

#cols1 <- names(train.set)[1:10]


train2<-learn_tree[,cols1]
train2$p1<-output3[,1]
train2$p2<-output3[,2]
train2$p3<-output3[,3]
train2$p4<-output3[,4]
train2$p5<-output3[,5]
train2$p6<-output3[,6]
train2$p7<-output3[,7]
train2$p8<-output3[,8]


test_tree <- test
testmatrix2 = data.matrix(test_tree[,cols], rownames.force = NA)
output_test2 <- predict(autoencoder.object, X.input=testmatrix2, hidden.output=TRUE)$X.output


test2<-test_tree[,cols1]
test2$p1<-output_test2[,1]
test2$p2<-output_test2[,2]
test2$p3<-output_test2[,3]
test2$p4<-output_test2[,4]
test2$p5<-output_test2[,5]
test2$p6<-output_test2[,6]
test2$p7<-output_test2[,7]
test2$p8<-output_test2[,8]

train <- train2
test <- test2
