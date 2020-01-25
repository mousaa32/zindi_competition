train=read.csv(file.choose(),header = TRUE,sep = ",",quote="\"")
train
train1=train[,-3]
train1
summary(Time.from.Pickup.to.Arrival)
attach(train1)
#nombre de temperature et precipitation qu'on a pas renseigne
sum(is.na(Temperature))
sum(is.na(Precipitation.in.millimeters))

#pour echanger les NA par la moyenne
library(dplyr)
train1=train1 %>% 
  dplyr::mutate (Temperature = ifelse(is.na(Temperature),23.26, Temperature))
#train1=train1 %>% 
  #dplyr::mutate (Temperature = ifelse(is.na(Temperature),median(Temperature), Temperature))
  
summary(train1)
a=median(Temperature)
View(train1) 

train2=train1[,-22]
View(train2) 
str(train2)
train3=train2[,-c(1,2,4,7,10,13,16,19,26)]
attach(train3)

cor(train3) 
             
hist(Temperature)





