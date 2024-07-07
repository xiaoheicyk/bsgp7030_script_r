library(tidyverse)
library(reshape2)
library(dplyr)
df <- read.csv("regrex1.csv")
print("Head of dataframe:")
print(head(df))

orig <-ggplot(mapping = aes(x=df$x,y=df$y)) +
  geom_point(colour="red",size=1) +
  xlab("x") +
  ylab("y") +
  ggtitle("Scatter plot on orginal data")
  
ggsave(orig,file="r_orig.png",width = 4,height = 4)

model <- lm(y~x,data=df)
print("-------------------------------")
print("summary of model:")
print(summary(model))

s <- coef(model)[2]
i <- coef(model)[1]
df$fitted <- model$fitted.values
names(df) <- c("original","test_x","predicted")
dfm <- melt(df,id.var="test_x",variable.name="y_type",value.name = "y_value")

lm <- ggplot(dfm, aes(x = test_x, y =y_value , shape = y_type, color = y_type))+
  geom_point()+
  geom_abline(slope = s,intercept = i,colour="yellow")+
  scale_color_manual(values = c("original" = '#ff00ff','predicted' = '#3399ff')) + 
  scale_shape_manual(values = c('original' = 17, 'predicted' = 16))+
  ggtitle("Scatter Plot and Linear Model") +
  xlab("x value")+
  ylab("y value")

ggsave(lm,file="r_lm.png",width = 4,height = 4)



