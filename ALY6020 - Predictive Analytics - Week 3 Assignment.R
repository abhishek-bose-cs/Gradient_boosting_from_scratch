library(rpart)

x<-seq(-10, 10, by=0.01)
y<- sin(x) + 5*cos(2*x) - 3*sin(3*x) + (1-exp(-x/3)) + 25

#adjust the learning rate based on the requirement
learning_rate <- 0.1

#Create Data Frame
df<-as.data.frame(x)
df[2]<-as.data.frame(y)

#Initial Prediction
model_init <- rpart(y~.,data = df)
pred_prev <- predict(model_init,df["x"])
fo <- pred_prev

#Continue to predict residual till it becomes minimum
list_of_residual <- list()
list_of_pred <- list()

#Initialize the color and plot the function
color_plot <- rainbow(100)
plot(x,y, type = "line", col="red")
lines(x,pred_prev, col="blue")
#Initialize parameter for running the gradient boosting loop
i <- 0
chk <- 10000              #Any random value greater than 10
plot_chk<-0
plot_res<-0

#loop till error becomes less
while (chk >= 10) {
  i <- i+1
  df["x"]<-as.data.frame(x)
  res <- (y - pred_prev)                                        #Calculating the Pseudo-residual
  chk <- sum(abs(res))                                          #Sum of Residuals
  plot_chk[i]<-chk
  plot_res[i]<-as.data.frame(res)
  df["y"]<-as.data.frame(res)
  #Adjust the minsplit and cp to see the change in number of iteration. 
  pred_res<-rpart(y~.,data = df, 
                  control=rpart.control(minsplit=2,cp=0.001))   #Model for residual with minimum split and cp
  list_of_residual[[i]] <- as.data.frame(predict
                                         (pred_res,df["x"]))    #Predict Residuals for that iteration
  list_of_pred[[i]] <- pred_prev + (learning_rate * 
                                      as.data.frame
                                    (list_of_residual[[i]]))    #Adding the residual of previous iteration 
                                                                #with the currant residual with learinig rate
  lines(x,as.vector(unlist(list_of_pred[[i]])), col=color_plot[i])
  pred_prev <- as.data.frame(list_of_pred[[i]])
}
