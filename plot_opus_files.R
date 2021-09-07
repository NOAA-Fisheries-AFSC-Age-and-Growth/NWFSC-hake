## Load packages
library(tidyverse)
library(plotly)
library(mdatools)
library(hyperSpec)
library(prospectr)
library(plotly)

#Load data
all_data <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Hake/Hake_data/hake_all_2019.csv")

all_data_l <- all_data%>%tidyr::pivot_longer(cols=c(3:1114),names_to="wavenumber", values_to= "absorbance_units", names_prefix = "X") 

# Fix up data types
all_data_l$filenames <- factor(all_data_l$filenames)
all_data_l$Age <- factor(all_data_l$Age)
all_data_l$wavenumber <- as.numeric(all_data_l$wavenumber)

#Plot spectra by age
age_cols <- length(unique(all_data_l$Age))
age_colors <- colorRampPalette(c("#2b9348","#B9E769","#EFEA5A","#F29E4C","#dc2f02", "#d00000","#370617"))(age_cols)

t <- ggplot(all_data_l)+
  geom_line(aes(x = wavenumber, y = absorbance_units, group = filenames, color = Age))+
  #scale_color_brewer(palette="RdYlGn")+
  #scale_y_continuous(limits = c(0.6,1.5))+
  #scale_color_manual(values = age_colors)+
  #scale_x_continuous(breaks=seq(3900,11600,100))+
  scale_x_reverse()+
  theme_classic()

t

# PCA
t1 <- pca(all_data[,-c(1:2,1115:1159)], 7, scale = TRUE, info = "Hake otolith PCA")

#Plot PCs
plotVariance(t1$res$cal, show.labels = TRUE, labels = "AssignedLabel")

#Plot Resids
plotResiduals(t1, show.labels = TRUE)

#Plot Scores
#mdaplot(m1$res$cal$scores, type = "p", show.labels = TRUE, show.lines = c(0, 0))
plotScores(t1$res$cal, show.labels = FALSE, cgroup = all_data$Age)

#After SG 1st derivative
nir_mat <- as.matrix(all_data)
nir_mat <- nir_mat[,-c(1:2,1115:1159)]
class(nir_mat) <- "numeric"

nir_sg <- savitzkyGolay(
  X = nir_mat,
  m = 2,  #differentiation order
  p = 2,  #polynomial order
  w = 25 #window size, odd
)

# Add "filename" back
dat_sg <- as.data.frame(cbind(all_data[,c(1:2,1115:1159)],nir_sg))

dat_sg$Age <- factor(dat_sg$Age)

# Plot
dat_sgL<-dat_sg%>%tidyr::pivot_longer(cols=c(49:1134), names_to="wavenumber", values_to= "absorbance_units", names_prefix = "X")

dat_sgL$wavenumber <- as.numeric(dat_sgL$wavenumber)
dat_sgL$Age <- factor(dat_sgL$Age)

d <- ggplot(dat_sgL)+
  geom_line(aes(x = wavenumber, y = absorbance_units, group = filenames, color = Age))+
  #scale_color_manual(values = oo_colors)+
  #scale_x_continuous(breaks=seq(3900,11600,100))+
  scale_x_reverse()+
  theme_classic()

d

# PCA
t2 <- pca(dat_sg[,c(48:1134)], 7, scale = TRUE, info = "2nd Deriv Hake Otolith PCA")

#Plot PCs
plotVariance(t2$res$cal, show.labels = TRUE, labels = "Maturity")

#Plot Resids
plotResiduals(t2, show.labels = TRUE)

#remove outliers
dat_sg_or <- dat_sg[-c(24,46,355,582),]

# PCA
t3 <- pca(dat_sg_or[,c(48:1134)], 7, scale = TRUE, info = "2nd Deriv Hake Otolith PCA")

#Plot Scores
##Scores Comp 1 and 2
ggplot()+
  geom_point(aes(t3$res$cal$scores[,1], t3$res$cal$scores[,2], color = dat_sg_or$Age))+
           labs(title="Scores",
                caption="Source: 2019 Hake",
                x="Comp 1",
                y="Comp 2",
                color = "Age")+
           geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
           geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
           theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

##Scores Comp 1 and 3
ggplot()+
  geom_point(aes(t3$res$cal$scores[,1], t3$res$cal$scores[,3], color = dat_sg_or$Age))+
           labs(title="Scores",
                caption="Source: 2019 Hake",
                x="Comp 1",
                y="Comp 3",
                color = "Age")+
           geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
           geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
           theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

##Scores Comp 2 and 3
ggplot()+
  geom_point(aes(t3$res$cal$scores[,2], t3$res$cal$scores[,3], color = dat_sg_or$Age))+
           labs(title="Scores",
                caption="Source: 2019 Hake",
                x="Comp 2",
                y="Comp 3",
                color = "Age")+
           geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
           geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
           theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

