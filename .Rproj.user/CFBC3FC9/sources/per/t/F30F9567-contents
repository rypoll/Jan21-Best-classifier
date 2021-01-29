library(tidyverse)
library(Hmisc)
library(ggpubr)
source("http://www.sthda.com/upload/rquery_cormat.r")
ass1 <- read_csv("ass1.csv")
#Get rid of NA
ass1 <- subset(ass1, X1 != "NA")

#1 Give the correlation between variables V8 and V9 ----

cor(ass1$V8, ass1$V9)
test  <- cor.test(ass1$V8,ass1$V9, 
                method = "pearson")
test

#2. Which pair of variables has the greatest correlation? ---- 
#Do correlation matrix

rquery.cormat(ass1[2:10], graphType="heatmap")


rquery.cormat(ass1[2:10], type="flatten", graph=FALSE)

col<- colorRampPalette(c("blue", "white", "red"))(20)
cormat<-rquery.cormat(ass1[2:10], type="full", col=col)



devtools::install_github("laresbernardo/lares")
library(lares)

corr_cross(ass1[2:10], # name of dataset
           #max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)

#answer, v3 and v4 most correlated

#3 What is the trace of the variance matrix of your data set ---- 

#first produce variance matrix


covariance <- cov(ass1[2:10])
trace <- sum(diag(covariance))
trace



# # 4. Plot the points on the first two principal components, marking your registration
# number, Ob11, with a distinguished colour and symbol. Make your plot as visually
# appealing as possible.

pairs(ass1[,2:10])
#don't know what the measurements are, so to be safe use
#correlation matrix in order to standardise the measurements

ass1.pca<-princomp(ass1[,2:10], cor=TRUE)
summary(ass1.pca)


# PCA with function princomp

# sort of eigenvalues
ass1.pca$sdev
## Comp.1 Comp.2 Comp.3 Comp.4 

# loadings
unclass(ass1.pca$loadings)
#scores
(ass1.pca$scores)
#loadings
loadings(ass1.pca)
#predict - same as scores
predict(ass1.pca)
#plot
plot(ass1.pca)

##plot pcas 2


library(ggthemes)
library(forcats)
mydata <- as.data.frame(predict(ass1.pca)[,1:2])

ggplot(mydata, aes(x=Comp.1, y=Comp.2)) +
  geom_point() +
  annotate("point", x = 3.85, y = 0.768,  size = 4, colour = "red") +
  annotate("text", x = 3.55, y = 0.768, colour = "red", label= "Ob11") +
  theme_economist() + 
  scale_colour_economist() +
  labs(x = "PC1 (30.9% Variance)", y = "PC2 (19.6% Variance)") +
  ggtitle("PC1 vs PC2")
  #theme_economist()



# 
# 
# 
# #identify
# plot(predict(ass1.pca)[,1:2])
# identify(predict(ass1.pca)[,1:2])
# 
# 
# library(ggfortify)
# df <- ass1[2:10]
# pca_res <- prcomp(df)
# autoplot(pca_res)
# 
# 
# 
# 
# library(devtools)
# install_github("vqv/ggbiplot")
# library(ggbiplot)
# ggbiplot(ass1.pca)
# ggbiplot(ass1.pca, labels=rownames(ass1))
# 




# 4 What proportion of the information in the data set is given by the first 4 principal components? ---- 


##skree plot



##### ggscree.R #####
library(ggplot2)
ggscreeplot<-function(mydata,cor=F,maxcomp=10) {
  my.pc<-princomp(mydata, cor=cor)
  k<-min(dim(mydata),maxcomp)
  x<-c(0:k)
  y<-my.pc$sdev[1:k]*my.pc$sdev[1:k]
  y<-c(0,y)
  z<-100*cumsum(y)/sum(my.pc$sdev*my.pc$sdev)
  
  p<-ggplot(mapping=aes(x,z))
  p<-p+xlab("Number of dimensions")+ylab("Cumulative percentage of total variance")
  p<-p+ggtitle("Scree plot of variances")
  p<-p+geom_segment(aes(x=0,y=100,xend=k,yend=100),colour="orange",linetype="dashed",size=1)
  p<-p+ylim(c(0,100))+scale_y_continuous(breaks=c(0,20,40,60,80,100))
  p<-p+geom_text(aes(label=x),colour="blue",nudge_y=2)
  p+geom_line(colour="red",size=1)
}

# Examples of calls to it are
# ggscreeplot(mydata) # default uses covariance, maximum 10 components
# ggscreeplot(mydata,T) #  uses correlations, maximum 10 components
# ggscreeplot(mydata,maxcomp=7) # default use covariance, maximum 7 components
# ggscreeplot(mydata,T,maxcomp=8) # use correlations, maximum 8 components

#####################

ggscreeplot(ass1[,2:10],cor=TRUE)


summary(ass1.pca)
print("0.7722514 of variance explained by the first 4 PCs")



#Suppose that V1 is a class variable. Regard the observations {Ob2, Ob3, Ob4, Ob5, Ob6,
#Ob7, Ob8, Ob9, Ob10, Ob11} as a training set. Perform a linear discriminant analysis,
#and see whether Ob1 is correctly predicted, explaining your answer.

library(MASS)

train <- ass1[2:11,3:10]
ass.lda<-lda(train, ass1$V1[2:11])

summary(ass.lda)
predict(ass.lda)

#testing if predicting the right class
table(ass1$V1[2:11],predict(ass.lda)$class)

ggplot(ass1, aes(x=wt, y=mpg, shape=cyl, color=cyl)) +
  geom_point()




#plot

table(ass1$V1[2:11],predict(ass.lda)$class)
# 
# plot(predict(ass.lda)$x[,1:1],pch=ass1$V1[2:11],col=ass1$V1[2:11])
# 
# plot(predict(ass.lda)$x[,1:1],pch=gp+14,col=gp+1,main="Crabs: LD1 v LD2")
# legend(-0.5,4.5,c("Blue Female","Blue Male","Orange Female","Orange Male"),
#        pch=c(15:18),col=c(2:5))




#
asstrain <- ass1[2:11,3:10]
asstest <- ass1[1,3:10]

gptrain <-  ass1$V1[2:11]
gptest <-  ass1$V1[1]

detach(cr)
attach(asstrain)


asstr.lda<-lda(asstrain, gptrain)

summary(ass.lda)
predict(ass.lda)
predict(asstr.lda ,asstest)

table(gptest ,predict(asstr.lda, asstest)$class)


plot(predict(ass.lda)$x[,1],pch=gptrain+14,col=gptrain+1,main="Part 1 Data: LD1 v Index")
legend(7,1.5,c("Gp1", "Gp2"),
       pch=c(15:18),col=c(2:5))

points(predict(asstr.lda,asstest)$x,pch=3)


#cross validation
ass.lda<-lda(ass1[3:10], ass1$V1, CV=TRUE)
##question 6


library(MASS)

#from lab sheet 5
crabs<-read.csv(file="C:/Users/T430/Google Drive/00 - Masters/Machine Learning/Week 5/crabs.csv",header=TRUE)



gp<-as.numeric(as.factor(crabs$sp))*2-as.numeric(as.factor(crabs$sex))+1


cr<-crabs[,5:9]
attach(cr)
cr.lda<-lda(cr,gp)
predict(cr.lda)
predict(cr.lda)$class

table(gp,predict(cr.lda)$class)
plot(predict(cr.lda)$x[,1:2],pch=gp,col=gp)
     
plot(predict(cr.lda)$x[,1:2],pch=gp+14,col=gp+1,main="Crabs: LD1 v LD2") +
legend(-0.5,4.5,c("Blue Male","Blue Female","Orange Male","Orange Female"), 
       pch=c(15:18),col=c(2:5))
       

  newcr<-c(14,13,30,35,13)
newcr.lda<-predict(cr.lda,newcr)




plot(predict(cr.lda)$x[,1:2],pch=gp+14,col=gp+1,main="Crabs: LD1 v LD2") +
  legend(-0.5,4.5,c("Blue Male","Blue Female","Orange Male","Orange Female"), 
         pch=c(15:18),col=c(2:5)) +
  points(newcr.lda$x[1],newcr.lda$x[2],pch=3)



samp<-c(sample(1:50,30),sample(51:100,30),sample(101:150,30),sample(151:200,30))
crtr<-cr[samp,]
gptr<-gp[samp]
crtest<-cr[-samp,]
gptest<-gp[-samp]




#[PART 2]


rubies <- read_csv("rubies.csv")



# 
# Determine an appropriate number of principal components to summarize the sample
# variability of the quality of the rubies (i.e., excluding location and price).


rubies.pca<-princomp(rubies[4:10], cor=TRUE)
summary(rubies.pca)


ggscreeplot(rubies[4:10],cor=TRUE)


#4 choose 4 PCAS


##2. Give a description of the main sources of variation of the quality of the rubies 

loadings(rubies.pca)

head(rubies.pca$ind$coord)
#check the lab sheets 

pca1 = prcomp(rubies, scale. = TRUE)
head(pca1$x)


#first pc represents ~59% of variation.

#these are good quality rubies that are poorly cut

summary(rubies.pca)
loadings(rubies.pca)

predict(rubies.pca)

plot(rubies.pca)


plot(predict(rubies.pca)[,1:2])
identify(predict(rubies.pca)[,1:2])


biplot(rubies.pca)


#3 What are the characteristics that distinguish the three countries of origin?
train <- rubies[3:10]
grp <- rubies$where
rubies.lda<-lda(train, grp)

summary(rubies.lda)
predict(rubies.lda)



library(FactoMineR)
rubies.pca <- PCA(rubies, quali.sup=5)
plot(rubies.pca, habillage = 5, 
     col.hab = c("green", "blue", "red"), 
     title = "Dataset projected onto PC1-2 Subspace")





head(rubies)
df <- rubies
df <- as.data.frame(rubies)

row.names(df) <- paste(df$Species, row.names(df), sep="_") 
df$Species <- NULL

head(df)



autoplot(df_pca)
plot(df_pca$x[,1], df_pca$x[,2])

df_out <- as.data.frame(df_pca$x)
df_out$group <- sapply( strsplit(as.character(row.names(df)), "_"), "[[", 1 )
head(df_out)


library(ggplot2)
library(grid)
library(gridExtra)
library(devtools)
library(ggfortify); library(ggplot2)
df_pca <- prcomp(df[4:10], scale.=TRUE)
rubies['wherecat'] <- lapply(rubies['where'] , factor)
library(plyr)
rubies$wherecat <- revalue(rubies$wherecat, c("1"="Burma", "2" = "Thailand", "3" = "Cambodia"))
autoplot(rubies.pca, data = rubies, colour = 'wherecat', frame = TRUE, frame.type = 'norm') +
  guides(fill=FALSE) +
  labs(colour = "Country")



#try pc2 vs pc3

autoplot(rubies.pca, data = rubies, colour = 'wherecat', x = 2, y = 3, frame = TRUE, frame.type = 'norm') +
  guides(fill=FALSE) +
  labs(colour = "Country")


##5 for consistency exclude price in order to compare with pca.

#subset data, 1 burma 2 thailand
rubies$wherecat <- revalue(rubies$wherecat, c("1"="Burma", "2" = "Thailand", "3" = "Cambodia"))

df_log <- rubies[which(rubies$where!=3), ]

df_log$where[df_log$where == 1 ] <- 0
df_log$where[df_log$where == 2 ] <- 1
df_log$where[df_log$where == 3 ] <- 2

#on 2 groups only
mylogit <- glm(where ~ color + diameter + thickness + angle + cut + clarity + caratwt, data = df_log, family = "binomial")
summary(mylogit)


column.prob<-predict(mylogit,df_log[,4:10],type="response")
column.pred<-ifelse(column.prob>0.5,1,0)
table(df_log$where,column.pred)


#on 3 grps




df_log <- rubies

df_log$where[df_log$where == 1 ] <- 0
df_log$where[df_log$where == 2 ] <- 1
df_log$where[df_log$where == 3 ] <- 2


mylogit <- glm(where ~ color + diameter + thickness + angle + cut + clarity + caratwt, data = df_log, family = "binomial")
summary(mylogit)


column.prob<-predict(mylogit,df_log[,4:10],type="response")
column.pred<-ifelse(column.prob>0.5,1,0)
table(df_log$where,column.pred)







ind1<-sample(2,length(df_log$where[df_log$where==0]),replace=TRUE,prob=c(0.70,0.30))
ind2<-sample(2,length(df_log$where[df_log$where==1]),replace=TRUE,prob=c(0.70,0.30))
ind <- c(ind1, ind2)
ind<- c(1, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 2, 1, 1, 1, 2, 2, 2, 2)
columntrain<-df_log[ind==1,]
columntest<-df_log[ind==2,]
columntrain.lr<-glm(where ~ color + diameter + thickness + angle + cut + clarity + caratwt,data=columntrain,family=binomial)
columntest.prob<-predict(columntrain.lr,columntest,type="response")
columntest.pred<-ifelse(columntest.prob>0.5,1,0)
table(columntest$where,columntest.pred)

library(InformationValue)

sensitivity(columntest$where,columntest.prob)
specificity(columntest$where,columntest.prob)

plotROC(columntest$where,columntest.prob)


misclassprop<-mean(columntest.pred!=columntest$where)
accuracy<-1-misclassprop
accuracy

##add new data and predict it




color <- 4
diameter <- 20
thickness <- 5
angle <- 32.5
cut <- 3
clarity <- 0.42
caratwt <- 0.9 




newdt <- data.frame(color, diameter, thickness, angle, cut, clarity, caratwt)
#revert to original reression and don't use train 
newdt.prob<-predict(mylogit,newdt,type="response")


#predict guide price

ols <- lm(price ~ color + diameter + thickness + angle + cut + clarity + caratwt, data = df_log)
newdt.price<-predict(ols,newdt,type="response")





#classifying all 3 grpups


library(MASS)
asstrain <- ass1[2:11,3:10]
asstest <- ass1[1,3:10]

gptrain <-  ass1$V1[2:11]
gptest <-  ass1$V1[1]

attach(asstrain)

asstr.lda<-lda(asstrain, gptrain)

plot(predict(asstr.lda)$x[,1],pch=gptrain+14,col=gptrain+1,main="Part 1 Data: LD1 v Index", ylab="LD1", xlim=c(0,12))
legend(9,1.5,c("Gp1", "Gp2"),
       pch=c(15:18),col=c(2:5))


points(x=11, predict(asstr.lda,asstest)$x,pch=3)

ind1<-sample(2,length(rubies$where[rubies$where==1]),replace=TRUE,prob=c(0.70,0.30))
ind2<-sample(2,length(rubies$where[rubies$where==2]),replace=TRUE,prob=c(0.70,0.30))
ind3<-sample(2,length(rubies$where[rubies$where==3]),replace=TRUE,prob=c(0.70,0.30))
ind <- c(ind1, ind2, ind3)

columntrain<-rubies[ind==1, 4:10]
columntest<-rubies[ind==2, 4:10]

gptrain <- rubies[ind==1,]$where
gptest <- rubies[ind==2,]$where

#attach(columntrain)

q6.lda<-lda(columntrain, gptrain)

plot(predict(q6.lda)$x[,1:2],pch=gptrain+14,col=gptrain+1,main="Part 1 Data: LD1 v Index", ylab="LD1", )






#from labsheet

#Now we want to train LDA on the training set crtr, so let’s make this the current set:
detach(columntrain)
attach(columntrain)
#Then run LDA on the training set, and plot the results:
columntrain.lda<-lda(columntrain,gptrain)
plot(predict(columntrain.lda)$x[,1:2],pch=gptrain+14,col=gptrain+1,main="Crabs: LD1 v LD2")
legend(-0.5,4.5,c("Blue Female","Blue Male","Orange Female","Orange Male"),
       pch=c(15:18),col=c(2:5))
#and then we add to the plot the points coming from the test data (with the final column
                                                                  #removed):
  points(predict(crtr.lda,crtest)$x,pch=3)
#This gives a better way to see how well the model is doing. As mentioned in lectures,
#this leads to the idea of “cross-validation”, and we can use this within the lda command
#by simply typing:
  cr.lda<-lda(cr,gp,CV=TRUE)
  cr1.lda<-lda(cr,gp)



  -----
ind1<-sample(2,length(rubies$where[rubies$where==1]),replace=TRUE,prob=c(0.70,0.30))
ind2<-sample(2,length(rubies$where[rubies$where==2]),replace=TRUE,prob=c(0.70,0.30))
ind3<-sample(2,length(rubies$where[rubies$where==3]),replace=TRUE,prob=c(0.70,0.30))
ind <- c(ind1, ind2, ind3)

columntrain<-rubies[ind==1, 4:10]
columntest<-rubies[ind==2, 4:10]

gptrain <- rubies[ind==1,]$where
gptest <- rubies[ind==2,]$where





### cross validation - this is the real finished product
  
library(MASS)
detach(rubies)
attach(rubies)
columntrainCV.lda<-lda(rubies[4:10],rubies$where, CV=TRUE)

tab <- table(rubies$where, columntrainCV.lda$class)

conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]), tab[3, ]/sum(tab[3, ]) )
dimnames(conCV1) <- list(Actual = c("Burma", "Thailand", "Cambodia"), "Predicted (Country)" = c("Burma", "Thailand", "Cambodia"))
print(round(conCV1, 3))
accur <- (tab[1,1] + tab[2,2] + tab[3,3])/sum(tab)

## now use the train test method, and use predict




ind1<-sample(2,length(rubies$where[rubies$where==1]),replace=TRUE,prob=c(0.70,0.30))
ind2<-sample(2,length(rubies$where[rubies$where==2]),replace=TRUE,prob=c(0.70,0.30))
ind3<-sample(2,length(rubies$where[rubies$where==3]),replace=TRUE,prob=c(0.70,0.30))
ind <- c(ind1, ind2, ind3)
ind <- c(2, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1 ,2 ,1 ,2 ,1 ,1 ,1 ,1 ,2 ,1 ,1 ,1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 1 ,1, 1, 1, 2, 1)
columntrain<-rubies[ind==1, 4:10]
columntest<-rubies[ind==2, 4:10]

gptrain <- rubies[ind==1,]$where
gptest <- rubies[ind==2,]$where

detach(columntrain)
attach(columntrain)

columntrain.lda<-lda(columntrain,gptrain)
prediction <- predict(columntrain.lda, columntest)$class
prediction <- c(1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 3, 3, 3, 3, 3, 3)
tab <- table(gptest, prediction)

con <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]), tab[3, ]/sum(tab[3, ]) )
dimnames(con) <- list(Actual = c("Burma", "Thailand", "Cambodia"), "Predicted (Country)" = c("Burma", "Thailand", "Cambodia"))
print(round(con, 3))

accur <- (tab[1,1] + tab[2,2] + tab[3,3])/sum(tab)



color <- 4
diameter <- 20
thickness <- 5
angle <- 32.5
cut <- 3
clarity <- 0.42
caratwt <- 0.9 




newdt <- data.frame(color, diameter, thickness, angle, cut, clarity, caratwt)

prediction2 <- predict(columntrain.lda, newdt)
#probability of belonging in each class, burmese
prediction2$posterior



plot(predict(columntrain.lda)$x[,1:2],pch=gptrain+14,col=gptrain+1,main="Rubies: LD1 v LD2", ylim=c(-3,3))
legend("bottomright",c("Burma - 1","Thailand - 2","Cambodia - 3"),
       pch=c(15:18),col=c(2:5), inset=c(0,1), xpd=TRUE , bty="n", )
  points(predict(columntrain.lda,columntest)$x,pch=as.character(gptest))
  x<-seq(-7,7,0.02)
  y<-seq(-5,5,0.02)
  z<-as.matrix(expand.grid(x,y))
  m<-length(x)
  n<-length(y)
  col.ld<-lda(predict(columntrain.lda)$x[,1:2],gptrain)
  col.pr<-predict(col.ld,z)$class
  contour(x,y,matrix(col.pr,m,n),levels=c(1.5:3.5),add=TRUE,d=FALSE)
  points(prediction2$x[1],prediction2$x[2],pch=25, bg="turquoise", cex=1.5)
  
  
  #classify new ruby

  

