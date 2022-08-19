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




#Assignment 2

# Solution 1
library(splines)
mortality_rate=read.csv2('mortality_rate.csv')
M=5

X = matrix(poly(mortality_rate$Day, degree=M-1, raw=TRUE), ncol=M-1)
h.75=ifelse(X[,1]>75, X[,4], 0)

X=cbind(X,h.75)

colnames(X)=as.character(seq(1:5))
df=data.frame(cbind(X, mortality_rate$Rate))

spline.fit=lm("V6~.", data=df)

yPred=predict(spline.fit, data=df)

plot(x=mortality_rate$Day, y=mortality_rate$Rate)
points(x=mortality_rate$Day, y=yPred, col=2)



#Solution2 #PREFERRED
df=read.csv2("mortality_rate.csv")
plot(df$Day, df$Rate, cex=0.5)

X=df$Day
df1=data.frame(X1=X,X2=X^2, X3=X^3, X4=X^4, X5=ifelse(X-75>0, X-75,0)^4, Y=df$Rate)
m2=lm(Y~., df1)
summary(m2)
Pr=predict(m2)
points(df$Day, Pr, col="blue", cex=0.5)



#Assignment 3

geneexp=read.csv("geneexp.csv")

temp_data=geneexp
temp_data$CellType=c()
x=t(temp_data)
temp_data$CellType=as.factor(geneexp$CellType)
library(pamr)
rownames(temp_data)=1:nrow(temp_data)
y=geneexp$CellType
mydata=list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)), genenames=rownames(x))
model=pamr.train(mydata,threshold=seq(0,4, 0.1))
set.seed(12345)
cvmodel=pamr.cv(model,mydata)
print(cvmodel)
#optimal 3.3 

pamr.plotcen(model, mydata, threshold=3.3)
a=pamr.listgenes(model,mydata,threshold=3.3)
cat( paste( colnames(data)[as.numeric(a[1:5,1])], collapse='\n' ) )

print(nrow(a))

