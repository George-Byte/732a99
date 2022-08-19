train_percentage=0.4
valid_percentage=0.3
test_percentage=0.3
RNGversion("3.5.1")

glass=read.csv('glass.csv')
glass$ID=c()
glass$Class=as.factor(glass$Class)

n=dim(glass)[1]
set.seed(12345)
id=sample(1:n, floor(n*train_percentage))
train=glass[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*valid_percentage))
valid=glass[id2,]
id3=setdiff(id1,id2)
test=glass[id3,]

valid_train=rbind(train,valid)

glm_model=glm(formula=Class~., data=valid_train, family=binomial)

prediction=predict(glm_model, newdata = test, type="response")
PredC=as.numeric(prediction>0.5)
table(test$Class, PredC)

coefficients(glm_model)
#probabilistic_model: p(x)=8080-4129Ri-13.4Na-4.7Mg-36.1Al-30.8S-4.7Ca-18.9Ba+27.6Be
#Decision boundary= 8080-4129Ri-13.4Na-4.7Mg-36.1Al-30.8S-4.7Ca-18.9Ba+27.6Be=0