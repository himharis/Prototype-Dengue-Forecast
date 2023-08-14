library(foreign)
library(tidyverse)
library(readxl)
library(cowplot)
library(gridExtra)
library(ggeasy)
library(kernlab)



setwd("~/Downloads/dengue phlc/PrediksiDengue")
data_fix <- read_excel("data_fix.xlsx")


#PEMBAGIAN DATA
##set nilai random generator
library(caret)
80/100*60

data_fix$ABJ <- as.factor(data_fix$ABJ)
data_fix$Date <- as.Date(data_fix$Date)

# membagi data 80% training, 20% testing

training<-data_fix[1:48,]
testing<-data_fix[49:60,]

## EXPLORATORY DATA ANALYSIS (EDA) Data Training ##

# dimensi data training
dim(training)

# summary deskriptif
summary(training)

# EDA Variabel Numerik #
# pisahkan variabel numerik
varnum <- select_if(training, is.numeric)
head(varnum)

# distribusi data variabel numerik
varnum %>% 
  gather() %>% 
  ggplot(aes(x=key, y=value)) + 
  geom_boxplot() + 
  facet_wrap( ~ key, scales="free")

# scatter plot antar variabel numerik
pairs(~ . , data=varnum, lower.panel=NULL)



# korelasi antar variabel numerik
# matriks korelasi
library(Hmisc)
korelasi <- rcorr(as.matrix(varnum))
# koefisien korelasi
korelasi_r <- korelasi$r
korelasi_r
# signifikansi korelasi
korelasi_p <- korelasi$P
korelasi_p
diag(korelasi_p) <- 0
# plot korelasi
library(corrplot)
corrplot(korelasi_r, method="circle", type='upper', 
         p.mat=korelasi_p, sig.level = 0.05, addCoef.col='black')


# EDA Variabel Kategorik #
# pisahkan variabel kategorik
varkat <- select_if(training, is.factor)
head(varkat)

# distribusi data variabel kategorik
varkat %>% 
  gather() %>% 
  ggplot(aes(y=value)) + 
  geom_bar() + 
  coord_flip() +
  facet_wrap( ~ key, scales="free")

## GRAFIK TESTING TRAINING
lastDate<- as.Date("2022-12-30")
startDate <- data_fix$Date[49]
x <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = kasus), size = 1.3, colour = "darkcyan") + labs(title ="Bandung City Dengue Cases 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x

#plot
x1 <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = Tavg), size = 1.3, colour = "turquoise4") + labs(title ="Average Temperature 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x1
x2 <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = RH_avg), size = 1.3, colour = "turquoise4") + labs(title ="Humidity 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x2
x3 <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = RR), size = 1.3, colour = "turquoise4") + labs(title ="Precipitation 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x3
grid.arrange(x1, x2, x3, ncol=1)

x4 <- ggplot(data_fix,aes(x = Date, y = ABJ))+  labs(title ="Ovitrap Index 2018-2022", y = " ", x = " ")  + scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4") + geom_point(aes(colour = factor(ABJ)), size = 4) + theme(legend.position="bottom", legend.box = "horizontal") + easy_remove_y_axis()
x4
x5 <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = ss), size = 1.3, colour = "turquoise4") + labs(title ="Sun Exposure 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x5
x6 <- ggplot(data_fix) +
  geom_line(aes(x = Date, y = ff_avg), size = 1.3, colour = "turquoise4") + labs(title ="Average Wind Speed 2018-2022", y = " ", x = " ") +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1')  +
  scale_x_date(date_breaks = "5 month", date_labels = "%b-%y")+ geom_text(x=as.Date("2022-06-01"), y=1250, label="Testing", color = "firebrick") +
  geom_text(x=as.Date("2019-06-01"), y=1250, label="Training", color = "gray4")
x6
grid.arrange(x4, x5, x6, ncol=1)



## METODE VALIDASI ##
# cross-validasi 5 lipat
fit.control <- trainControl(method = "cv", number = 5)



## PEMODELAN REGRESI ##

# REGRESI LINIER #
lr <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, 
            data = training, method = "lm", trControl = fit.control)
lr

# cek asumsi linieritas
plot(lr$finalModel, 1)

# cek asumsi homogenitas
library(lmtest)
bptest(lr$finalModel)

# cek asumsi autokorelasi
library(car)
durbinWatsonTest(lr$finalModel)

# cek asumsi multikolinier
library(car)
vif(lr$finalModel)

# cek asumsi normalitas
residual = training$kasus - predict(lr, training)
shapiro.test(residual)
ks.test(residual, "pnorm")

# deksripsi model yang terbentuk
summary(lr)

# melakukan prediksi terhadap data testing
testing$prediksiLR <- predict(lr, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiLR, testing$kasus)



# RIDGE REGRESSION #
# set range lambda (antara 0.001 sampai 1000)
lambda <- 10^seq(-3, 3, length = 100)

ridge <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, 
               data = training, method = "glmnet", 
               trControl = fit.control,
               tuneGrid = expand.grid(alpha = 0, lambda = lambda))
ridge


# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

# melakukan prediksi terhadap data testing
testing$prediksiRidge <- predict(ridge, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiRidge, testing$kasus)
varImp(ridge)


# Lasso REGRESSION #

lasso <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, 
               data = training, method = "glmnet", 
               trControl = fit.control,
               tuneGrid = expand.grid(alpha = 1, lambda = lambda))
lasso

# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

# melakukan prediksi terhadap data testing
testing$prediksiLasso <- predict(lasso, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiLasso, testing$kasus)



# REGRESSION TREE #

regtree <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, 
                 data = training, method = "rpart", trControl = fit.control)
regtree

# menampilkan plot decision tree
set.seed(1234)
library(rattle)
fancyRpartPlot(regtree$finalModel)

# melakukan prediksi terhadap data testing
testing$prediksiTree <- predict(regtree, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiTree, testing$kasus)

# melihat variabel importance
varImp(regtree)


# RANDOM FOREST #
rf <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, data = training, method = "rf", trControl = fit.control)
rf

# melakukan prediksi terhadap data testing
testing$prediksiForest <- predict(rf, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiForest, testing$kasus)

# melihat variabel importance
varImp(rf)



# SUPPORT VECTOR REGRESSION #
svr <- train(kasus ~ Tavg + RH_avg + RR + ss + ff_avg + ABJ, data = training, method = "svmLinear", trControl = fit.control)
svr

# melakukan prediksi terhadap data testing
testing$prediksiSVR <- predict(svr, testing)
View(testing)

# melihat tingkat error atau akurasi hasil prediksi
postResample(testing$prediksiSVR, testing$kasus)

# melihat variabel importance
varImp(svr)


# menampilkan perbandingan hasil antar model
model_list <- list(LinearRegression = lr,
                   RidgeRegression = ridge,
                   LassoRegression = lasso,
                   RegressionTree = regtree, 
                   RandomForestReg = rf, 
                   SupportVectorReg = svr)
res <- resamples(model_list)
summary(res)


# bandingkan semua model yang sudah dibuat
perbandingan <- data.frame(Linear = round(postResample(testing$prediksiLR, testing$kasus),2),
                           Ridge = round(postResample(testing$prediksiRidge, testing$kasus),2),
                           Lasso = round(postResample(testing$prediksiLasso, testing$kasus),2),
                           RegTree = round(postResample(testing$prediksiTree, testing$kasus),2),
                           RandomForest = round(postResample(testing$prediksiForest, testing$kasus),2),
                           SVR = round(postResample(testing$prediksiSVR, testing$kasus),2))
perbandingan

#grafik
perbandingan
training$prediksiLR <- NA
training$prediksiRidge <- NA
training$prediksiLasso <- NA
training$prediksiTree <- NA
training$prediksiForest <- NA
training$prediksiSVR <- NA
final <- rbind(training, testing)

x <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1.3 ) +
  geom_line(data=final,aes(y=prediksiLR,x= Date,colour="Prediction LR"),size=1) +
  geom_line(data=final,aes(y=prediksiLasso,x= Date,colour="Prediction Lasso"),size=0.5) +
  geom_line(data=final,aes(y=prediksiTree,x= Date,colour="Prediction Reg Tree"),size=0.5) +
  geom_line(data=final,aes(y=prediksiForest,x= Date,colour="Prediction RForest"),size=0.5) +
  geom_line(data=final,aes(y=prediksiSVR,x= Date,colour="Prediction SVR"),size=0.5) +
  scale_color_manual(name = "Methods", values = c("Observation" = "darkcyan", "Prediction LR" = "red", "Prediction Lasso" = "green", "Prediction Reg Tree" = "gray3", "Prediction RForest" = "turquoise", 
                                                  "Prediction SVR" = "blue")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.12, fill = 'salmon1') +
  scale_x_date(date_breaks = "6 month", date_labels = "%b-%y") + theme_minimal() 
x

x1 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiLR,x= Date,colour="Prediction LR"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction LR" = "orchid4")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-LR", y = " ", x = " ") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") 
x2 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiTree,x= Date,colour="Prediction Reg Tree"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction Reg Tree" = "violetred3")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-Reg Tree", y = " ", x = " ") +
  theme(legend.position="bottom", legend.box = "horizontal") +
  scale_x_date(date_breaks = "12 month", date_labels = "%Y") 
x3 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiLasso,x= Date,colour="Prediction Lasso"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction Lasso" = "darkorange4")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-Lasso", y = " ", x = " ") +
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y")
x4 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiForest,x= Date,colour="Prediction RForest"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction RForest" = "midnightblue")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-RForest", y = " ", x = " ")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y")
x5 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiSVR,x= Date,colour="Prediction SVR"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction SVR" = "green4")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-SVR", y = " ", x = " ")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y")
x6 <- ggplot()+
  geom_line(data=final,aes(y=kasus,x= Date,colour="Observation"),size=1, alpha = 0.6) +
  geom_line(data=final,aes(y=prediksiSVR,x= Date,colour="Prediction Ridge Reg"),size=1) +
  scale_color_manual(name = " ", values = c("Observation" = "darkcyan", "Prediction Ridge Reg" = "blue")) +
  annotate("rect", xmin = startDate, xmax = lastDate, ymin=-Inf, ymax=Inf, alpha = 0.2, fill = 'salmon1') +
  labs(title ="Observation-Ridge Reg", y = " ", x = " ")+
  theme(legend.position="bottom", legend.box = "horizontal")+
  scale_x_date(date_breaks = "12 month", date_labels = "%Y")
plot_grid(x1, x3, x6, x2, x4, x5, ncol = 3, nrow = 2, labels = "AUTO")

saveRDS(svr, "dengue_svr.rds")

