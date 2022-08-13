data =read.csv("C:/Users/SVMY/Downloads/R project/water_potability.csv")
head(data)

colnames(data)

data=na.omit(data)
sum(is.na(data))

typeof(data)
sapply(data, typeof)

library(plotly)
l <- htmltools::tagList()
for(i in 1:9){
l[[i]]=as.widget(plot_ly(y = data[,i], boxpoints = 'outliers', type = "box",name = colnames(data)[i],marker = list(color = 'rgb(107,174,214)'),
        line = list(color = 'rgb(107,174,214)'),fillcolor = palette(rainbow(9))[i]))
}

fig <- subplot(l[[1]], l[[2]],l[[3]],l[[4]],l[[5]],l[[6]],l[[7]],l[[8]],l[[9]]) %>% layout(title = 'viewing outliear')

fig

#remove ph outlier
Q1 <- quantile(data$ph, .25)
Q3 <- quantile(data$ph, .75)
IQR <- IQR(data$ph)
data$ph[which(data$ph < (Q1 - 1.5*IQR) )]=7.08
data$ph[which(data$ph > (Q3 + 1.5*IQR))]=7.08
#boxplot(data$ph)

#remove hardness outlier
Q1 <- quantile(data$Hardness, .25)
Q3 <- quantile(data$Hardness, .75)
IQR <- IQR(data$Hardness)
data$Hardness[which(data$Hardness < (Q1 - 1.5*IQR) )]=197.19
data$Hardness[which(data$Hardness > (Q3 + 1.5*IQR))]=197.19
#boxplot(data$Hardness)

#remove solid outlier
Q1 <- quantile(data$Solids, .25)
Q3 <- quantile(data$Solids, .75)
IQR <- IQR(data$Solids)
data$Solids[which(data$Solids < (Q1 - 1.5*IQR) )]=20.93
data$Solids[which(data$Solids > (Q3 + 1.5*IQR))]=20.93
#boxplot(data$Solids)

#remove chloramines outlier
Q1 <- quantile(data$Chloramines, .25)
Q3 <- quantile(data$Chloramines, .75)
IQR <- IQR(data$Chloramines)
data$Chloramines[which(data$Chloramines < (Q1 - 1.5*IQR) )]=7.14
data$Chloramines[which(data$Chloramines > (Q3 + 1.5*IQR))]=7.14
#boxplot(data$Chloramines)

#remove sulfate outlier
Q1 <- quantile(data$Sulfate, .25)
Q3 <- quantile(data$Sulfate, .75)
IQR <- IQR(data$Sulfate)
data$Sulfate[which(data$Sulfate < (Q1 - 1.5*IQR) )]=332.23
data$Sulfate[which(data$Sulfate > (Q3 + 1.5*IQR))]=332.23
#boxplot(data$Sulfate)

#remove conductivity outlier
Q1 <- quantile(data$Conductivity, .25)
Q3 <- quantile(data$Conductivity, .75)
IQR <- IQR(data$Conductivity)
data$Conductivity[which(data$Conductivity < (Q1 - 1.5*IQR) )]=423.46
data$Conductivity[which(data$Conductivity > (Q3 + 1.5*IQR))]=423.46
#boxplot(data$Conductivity)

#remove organic_cardon outlier
Q1 <- quantile(data$Organic_carbon, .25)
Q3 <- quantile(data$Organic_carbon, .75)
IQR <- IQR(data$Organic_carbon)
data$Organic_carbon[which(data$Organic_carbon < (Q1 - 1.5*IQR) )]=14.32
data$Organic_carbon[which(data$Organic_carbon > (Q3 + 1.5*IQR))]=14.32
#boxplot(data$Organic_carbon)

#remove trihalomethones outlier
Q1 <- quantile(data$Trihalomethanes, .25)
Q3 <- quantile(data$Trihalomethanes, .75)
IQR <- IQR(data$Trihalomethanes)
data$Trihalomethanes[which(data$Trihalomethanes < (Q1 - 1.5*IQR) )]=66.54
data$Trihalomethanes[which(data$Trihalomethanes > (Q3 + 1.5*IQR))]=66.54
#boxplot(data$Trihalomethanes)

#remove turbidity outlier
Q1 <- quantile(data$Turbidity, .25)
Q3 <- quantile(data$Turbidity, .75)
IQR <- IQR(data$Turbidity)
data$Turbidity[which(data$Turbidity < (Q1 - 1.5*IQR) )]=3.97
data$Turbidity[which(data$Turbidity > (Q3 + 1.5*IQR))]=3.97
#boxplot(data$Turbidity)


x=c(length(which(data$ph<6.52)),length(which(data$ph>6.52 & data$ph<6.83)),length(which(data$ph>6.83)))
sum(x)
txt=c(paste0(round((x[1]/sum(x))*100,2), "%"),paste0(round((x[2]/sum(x))*100,2), "%"),paste0(round((x[3]/sum(x))*100,2), "%"))
txt
fig <- plot_ly(
  x = c("low ph", "ideal ph", "high ph"),
  y = x,
  name = "SF Zoo",
  type = "bar",
  text=txt,
  marker = list(color = c('rgba(204,204,204,1)', 'rgba(103,195,66,0.8)',
                          'rgba(204,204,204,1)'))
)

fig


label=c("Soft","Moderately high","Hard","Very Hard")
values=c(length(which(data>0 & data<60)),length(which(data>61 & data<120)),length(which(data>121 & data<180)),length(which(data>180)))
fig <- plot_ly(type='pie', labels=label, values=values, 
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = "Pie chart on water hardness")
fig



label=c("solid","desirable","solid")
values=c(length(which( data$Solids<500)),length(which(data$Solids>500 & data$Solids<1000)),length(which(data$Solids>1000)))
fig <- plot_ly(type='pie', labels=label, values=values,
               textinfo='label+percent',
               insidetextorientation='radial')
fig <- fig %>% layout(title = "Pie chart on water hardness")
fig


length(which(data$Solids<1000))

label=c("safe water","unsafe water")
values=c(length(which( data$Chloramines<4)),length(which(data$Chloramines>4)))
colors <- c('rgba(103,195,66,0.8)','rgba(204,204,204,1)')
fig <- plot_ly(, labels=label, values=values,
               textinfo='label+percent',marker = list(colors = colors))
fig <- fig %>% add_pie(hole = 0.6)
fig <- fig %>% layout(title = "Pie chart on water hardness")

fig


plot(density(data$Conductivity), main="frequency density plot")
polygon(density(data$Conductivity), col="aquamarine3", border="blue")
abline(v=500,col="red")
#locator() #this function is use to locate point in the diagram
text(280, 0.003999010, expression("drinking water"))
text(650.0549, 0.00392039, expression("contsminated water"))

x=c(length(which(data$Conductivity<400)),length(which(data$Conductivity>400)))
txt=c(paste0(round((x[1]/sum(x))*100,2), "%"),paste0(round((x[2]/sum(x))*100,2), "%"))

colfunc <- colorRampPalette(c("#3FFA5E","#3FFA5E", "red","#FA4B3F"))
hist(data$Conductivity,breaks = seq(from=0,to=800,length=50),col=colfunc(50),main = "frequency diagram on Conductivity",xlab = "Conductivity")
abline(v=400,col="blue")
#locator()
text(90, 120, txt[1])
text(570, 134, txt[2])
text(90, 110, "safe")
text(570, 120, "unsafe")


label1=c("drinking water","dirty water")
values1=c(length(which( data$Trihalomethanes<80)),length(which(data$Trihalomethanes>80)))
colors <- c('#3AB4F2','#FFFFFF')
fig <- plot_ly(, labels=label1, values=values1,
               textinfo='label+percent',
               insidetextorientation='radial',marker = list(colors = colors))
fig <- fig %>% add_pie(hole = 0.8)
fig <- fig %>% layout(title = "Pie chart on Trihalomethanes")
fig

#correlation plot
library(psych)
corPlot(data, cex = 1.2)


#split dataset
set.seed(101) 
library(caTools)
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
trainst=scale(train)
testst=scale(test)
x=trainst[,1:9]
y=trainst[,10]

require(leaps)
prostateSubset<-regsubsets(x,y,intercept=FALSE,nbest=1)
plot(prostateSubset,scale="bic")
#ph,Conductivity,Turbidity

#fitting 

model_pro=glm(Potability~ph+Conductivity+Turbidity,train,,family="binomial"(link="cloglog"))
summary(model_pro)

newtest=subset(test, select=c("Solids","Conductivity","Organic_carbon","Turbidity"))
y_hat=predict(model_pro,newtest,type="response")
y_hat[48]
test$Potability[48]
fun=function(x){
if(x<0.5){ return(0)}
else{ return(1)}
}
yhat=sapply(y_hat,fun)
head(test$Potability,30)
d = test$Potability-yhat
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
cat(" MAE:", mae, "\n", "MSE:", mse, "\n", 
    "RMSE:", rmse)

length(which(test$Potability!=yhat))


#deep learning

library(keras)
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)

data <- as.matrix(data)
dimnames(data) <- NULL

set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(.7, .3))
x_training <- data[ind==1,1:9]
x_test <- data[ind==2, 1:9]
y_training <- data[ind==1, 10]
y_test <- data[ind==2, 10]



x_training <- scale(x_training)
x_test <- scale(x_test)


#model creatring
model <- keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(9)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 50,activation = "relu") %>%
  layer_dropout(rate = 0.7) %>%
  layer_dense(units = 50,activation = "relu") %>%
  layer_dropout(rate = 0.8) %>%
  layer_dense(units = 1,activation = "sigmoid")


model %>% compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(lr=0.001), 
                  metrics = 'mae') 

mymodel <- model %>% 
  fit(x_training,y_training,
      epochs = 100,
      batch_size = 32,
      validation_split = 0.2)

model %>% evaluate(x_test,y_test)
pred= model %>% predict(x_test)
#plot(y_test,pred)
#pred
plot((y_test-pred)^2)

yhat=sapply(pred,fun)
length(which(test$Potability==yhat))
