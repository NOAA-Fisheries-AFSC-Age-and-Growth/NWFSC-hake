#Depends on plot_opus_files

#Load packages
library(mdatools)
library(tidyverse)

# Split data into calibration/ validation
cal <- dat_sg_or %>%
  group_by(Age)%>%
  slice_sample(prop = .5) #this is first pass, can use approaches like Kennard-Stone algothrim to select represetative calibration

cal <- as.data.frame(cal)
cal$Age <- as.numeric(cal$Age)

val <- anti_join(dat_sg_or, cal, by = "filenames")

val <- as.data.frame(val)
val$Age <- as.numeric(val$Age)

#Build calibration model
m <- pls(cal[,c(48:1134)], cal$Age, 7, scale = TRUE, cv = 1, info = "Age prediction model")

summary(m$res$cal)

#Plotting
plotXVariance(m)
plotXCumVariance(m)

plot(m)
plotPredictions(m)
plotPredictions(m$res$cal, show.stat = TRUE)

#Validate the model
res = predict(m, val[,c(40:1127)], val$Age)
summary(res)

 
plot(res)
plotPredictions(res, show.stat = TRUE)

#Extract predictions
cal <- cbind(cal, m$calres$y.pred)
val <- cbind(val, res$y.pred)

cal<- rename(cal, Comp_3 = cal$`Comp 3.y1`)

#Plot for real

ggplot(cal)+
  geom_point(aes(x = Age, y = `Comp 7.y1`), color = "grey20", alpha = .5)+
  scale_x_continuous(limits = c(0,18), breaks=seq(0,18,2))+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,2))+
  geom_smooth(aes(x = Age, y = `Comp 7.y1`),method = "lm")+
  geom_text(x=0, y=18, label="RMSE(C) = 0.808", hjust = 0, size = 3)+
  geom_text(x=0, y=17, label = "RPD = 3.34", size = 3, hjust = 0)+
  geom_text(x=0, y=16, label = expression(paste(r^{2},"= 0.910")), size = 3, hjust = 0)+
  labs(x = "Traditional age",
       y = "Predicted age",
       subtitle = "Calibration")+
  theme_classic()
  
  
ggplot(val)+
  geom_point(aes(x = Age, y = `Comp 7.y1`), color = "grey20", alpha = .5)+
  scale_x_continuous(limits = c(0,18), breaks=seq(0,18,2))+
  scale_y_continuous(limits = c(0,18), breaks=seq(0,18,2))+
  geom_smooth(aes(x = Age, y = `Comp 7.y1`),method = "lm")+
  geom_text(x=0, y=18, label="RMSE(V) = 0.937", hjust = 0, size = 3)+
  geom_text(x=0, y=17, label = "RPD = 2.90", size = 3, hjust = 0)+
  geom_text(x=0, y=16, label = expression(paste(r^{2},"= 0.881")), size = 3, hjust = 0)+
  labs(x = "Traditional age",
       y = "Predicted age",
       subtitle = "Validation")+
  theme_classic()
  
  
