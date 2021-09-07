## Code for loading OPUS files into r
# load required packages
library(data.table)
library(mdatools)
library(dplyr)
library(hyperSpec)
library(prospectr)


# install and load simplerspec
#install.packages("remotes")
#remotes::install_github("philipp-baumann/simplerspec")
library(simplerspec)

# upload all spectral files
setwd("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Hake/NIR Spectra/2019 Original")# set working directory to folder containing spectral files
ldf <- list() # creates an empty list
listspc <- dir(pattern = "*.0") # creates the list of all the .001 file names in the directory
for (k in 1:length(listspc)){ # loops through and uploads each file, depends on simplerspec package
  ldf[[k]] <- read_opus_bin_univ(listspc[k], extract = c("spc"),
                                 print_progress = TRUE, atm_comp_minus4offset = FALSE)
}
str(ldf[[1]]) # check first element

rdat <- ldf

#Interpolate
## prospectr resample uses spline interpolation method
## First test resample on one spectra
test <- resample(X = rdat[[1]][[2]], wav = rdat[[1]][[8]], rdat[[2852]][[8]])

tesdf <- as.data.frame(test)

##plot to test
plot(as.vector(rdat[[1]]$wavenumbers), as.vector(rdat[[1]]$spc))
plot(as.vector(rdat[[2852]]$wavenumbers), as.vector(test))

# prospectr resample all with loop
rdat <- rdat[lapply(rdat, length) > 2]  ## need a way to filter out spectra that failed to load correctly

ldf_int <- matrix(data = NA, nrow = length(rdat), ncol = length(rdat[[2849]]$wavenumbers)) #make empty matrix for loop

for (j in 1:length(rdat)){ 
  ldf_int[j,] <- resample(X = rdat[[j]][[2]], wav = rdat[[j]][[8]], new.wav = rdat[[2849]][[8]])
}

colnames(ldf_int) <- rdat[[2849]]$wavenumbers

dat_spc <- as.data.frame(ldf_int)

# Add file names back in, could add other variables here as well (age etc.)
metadat <- sapply(rdat,'[[', 1)
filenames <- unlist(metadat[2,])
dat <- cbind(filenames, dat_spc)

scan_data <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Hake/Hake_data/FTNIR_Hake_2019.csv", strip.white=TRUE) #load in ancillary data

int_data <- left_join(dat, scan_data, by = "filenames")

# ADD AGE DATA
age_data <- read.csv("C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Hake/Hake_data/FT_NIR_NWFSC_ProductionAgeScanning_2019.csv", strip.white = TRUE) #load in age data

all_data <- left_join(int_data, age_data, by = "sequence")

# ADD HAUL METADATA
haul_data <- read.csv("~/AFSC A&G Contract/Hake/Hake_data/Hake_haulmetadata_2019.csv")

all_data <- left_join(all_data, haul_data, by = "Barcode")

#output data
write.csv(x = all_data, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Hake/Hake_data/hake_all_2019.csv")

