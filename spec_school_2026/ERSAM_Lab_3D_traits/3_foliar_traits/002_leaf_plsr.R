







# ------------------------------------------------------------------------------
# TRASH OLD SCRIPT FROM KYLA #
# ------------------------------------------------------------------------------

################################################################################

# Partial Least Squares Regression (PLSR) code for exploring leaf traits with 
# hyperspectral data.

# Code was sourced from:
# Dr. Shawn Serbin at Brookhaven National Laboratory -- sserbin@bnl.gov

# Edited and adapted by 
# 1. Dr. Kyla Dahlin at Michigan State University (MSU) -- kdahlin@msu.edu
# 2. Aaron Kamoske, PhD student at MSU - kamoskea@msu.edu

################################################################################

# this code takes leaf level N measurements and connects them to leaf level 
# spectral measurements using PLSR

#load the needed libraries
library(pls)
library(reshape2)
library(dplyr)
#library(soil.spec) # only need this for transformations, which we aren't using

today <- "20190611"

#set working directories

# your local directory
kylas.dir <- "C:/Users/kdahlin/Dropbox/"

# set working dir (for output)
setwd(paste0(kylas.dir, "NEON_hsi_lidar/hsi_lidar_ms1/data/field_data/plsr_LMA"))

# where is leaf chem data stored?
chem.loc <- "X:/shared_data/foliar_chemistry/2017/"

# where is spectroscopy data stored?
field.loc <- "X:/shared_data/NEON_field_data/"

#lets set up an output directory
dir.create(paste0("out_", today))

# so we can point to it later ->
out.dir <- paste0("./out_", today, "/")

################################################################################
# let's read in and clean up our data
###################################################################################

# read in chem data from the lab that processed samples
harv.chem.data <- read.csv(paste0(chem.loc, 
                                  "Lab_Data_Sheets_HARV_20180913.csv"),
                           stringsAsFactors = FALSE)

serc.chem.data <- read.csv(paste0(chem.loc, 
                                  "/Lab_Data_Sheets_SERC_20180913.csv"),
                           stringsAsFactors = FALSE)

sub.harv.data <- harv.chem.data[,c(1,2,7,8,13,14)]
sub.serc.data <- serc.chem.data[,c(1,2,8,9,14,15)]

chem.data <- rbind(sub.harv.data, sub.serc.data)

# rename columns so they're sensible in R format
names(chem.data) <- c("Sample", 
                      "date", 
                      "leafarea_m2", 
                      "wetwt_g", 
                      "drywt_g", 
                      "lma")

# convert all sample names to upper to match other data sets
chem.data[,1] <- toupper(chem.data[,1])

# read in spectral data from SERC
serc.spectra <- read.csv(paste0(field.loc, 
                                "SERC/2017/Lab_Data/Spectra_Data/CSV_Files/",
                                "Clean_Data/SERC_SpectraMean_CSV_20170830.csv"), 
                         stringsAsFactors = FALSE)

# read in spectral data from HARV
harv.spectra <- read.csv(paste0(field.loc,
                                "HARV/2017/Lab_Data/Spectra_Data/CSV_Files/", 
                                "Clean_Files/HARV_SpectraMean_CSV_20170830.csv"), 
                         stringsAsFactors = FALSE)

# row bind the two spectral data sets together
all.spectra <- rbind(serc.spectra, harv.spectra)

# rename the 'ID' column to 'Sample' to match chem.data
names(all.spectra)[1] <- "Sample"

# make sure all sample names are also uppper case
all.spectra[,1] <- toupper(all.spectra[,1])

# read in a file of wavelengths from the SVC so we know which band is which
wavelengths <- read.csv(paste0(field.loc, 
                               "SERC/2017/Lab_Data/Spectra_Data/",
                               "Wavelength_ReadMe_20170806.csv"))

# turn the wavelenths into round numbers to make 'wave.###' column names
wavelengths$wave.num <- as.character(round(wavelengths$WAVELENGTH))
wave.name <- paste0("wave.", wavelengths$wave.num)

# name the spectral data
names(all.spectra)[2:999] <- wave.name

# merge the spectral data with the chem data, keeping all rows
all.data <- merge(chem.data, all.spectra, all = TRUE)

# now write a table of all the clean data
write.csv(all.data, paste0(out.dir, 
                           "SERC_HARV_combined_leaf_LMA_spectra_all.csv"), 
          row.names = FALSE)

# let's make a plot to see how this all looks
x11()
matplot(wavelengths$WAVELENGTH, 
        t(all.data[,7:1004]), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Lab Transflectance*",
        main = "SERC & HARV SVC spectra")

# if your're starting anew, lets read in the csv that contains the leaf trait 
# and hyperspectral data
# all.data <- read.csv(paste0(out.dir,
#                             "/2017_SERC_HARV_CombinedData_20180220.csv"), 
#                     row.names = NULL)

# lets remove the data that we don't need: bands outside 400-2500nm and other 
# leaf traits we don't need right now - note, this is a place to change depending
# on results - might want to try a narrower or wider range or subset of spectra
keep.data <- all.data[,c(1,6, 51:995)]

# also get the spectra values in the same range (this will be useful for 
# plotting later)
keep.spectra <- wavelengths$WAVELENGTH[44:988]

# keep the data that have leaf chem and spectra
chem.spec.data <- subset(keep.data, !is.na(keep.data$lma))

# trying some transformations! NOTE: note of these improved the results much, so
# just sticking with original data
# spec.data <- chem.spec.data[,3:834]

# brighness normalization - NOTE: for foliar N this improved the training data
# R2 but NOT the test data, so not using.
# bn.spec.data <- spec.data / sqrt(apply(spec.data^2, 1, sum)) 
# eqn 3 in Feilhauer et al 2010

# chem.spec.data[,3:834] <- bn.spec.data

# first derivative - NOTE: for foliar N this also improved the training data
# R2 but not the test data (though this worked better than bn), so not using.
# this function needs the colnames to be numbers
# names(spec.data) <- keep.spectra
# x11()
# first.deriv.spec <- soil.spec::trans(spec.data, 
#                                      tr = "derivative",
#                                      order = 1,
#                                      plot.spectrogram = F)
# matplot(t(first.deriv.spec$trans), type = "l")
# 
# chem.spec.data[,3:834] <- first.deriv.spec$trans

# lets take ~20% of our data for testing and leave the other 80% for training
# this should sample more or less evenly across the distribution of N values
test.n <- round(nrow(chem.spec.data)*0.2)

test.data <- sample_n(chem.spec.data, test.n, 
                      replace = FALSE, 
                      weight = chem.spec.data$lma)

# then the rest of the chem.spec.data are training data
training.data <- chem.spec.data[!(chem.spec.data$Sample %in% test.data$Sample),]

# lets subset out the spectra data so we can just use that
# this is getting ready to format the data to work with the PLRS function
spectra.training.data <- training.data[,3:ncol(training.data)]
n.training.data <- training.data[,1:2]

spectra.test.data <- test.data[,3:ncol(test.data)]
n.test.data <- test.data[,1:2]

###################################################################################
#lets set up our data for PLSR
###################################################################################

#first lets set some options inside the pls package
pls.options(plsralg = "oscorespls")
pls.options("plsralg")

#lets save the name of the N variable we are using so that if it changes from 
# csv to csv we don't have to change the code
in.var <- "LMA"

#lets put our dataset into the correct format for running the PLSR
plsr.spectra <- as.matrix(spectra.training.data)
plsr.dataset <- data.frame(LMA = n.training.data$lma, 
                           spectra = I(as.matrix(spectra.training.data)))

################################################################################
#lets take a look at the correlations between the spectra and the biochem data
################################################################################

# lets take a quick look at the correlations between the spectra and biochemical 
# data (note from Kyla - this seems like a clunky way to do this, but it works)
spectra.cor <- cor(plsr.spectra, 
                   plsr.dataset[grep(in.var, names(plsr.dataset), fixed = TRUE)], 
                   use = "complete.obs")

#lets take a look at the spectra again (narrower range)
matplot(keep.spectra, 
        t(plsr.spectra), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Lab Transflectance*",
        main = "SERC & HARV SVC spectra - training data")


#lets take a look at the correlation between the spectra and biochemical data
plot(keep.spectra,
     spectra.cor, 
     xlab = "Wavelength (nm)", 
     ylab = "Correlation", 
     type = "l", 
     lwd = 4)
abline(h = 0,lty = 2, lwd = 1.5, col = "grey80")
box(lwd = 2)

#lets write this data to a csv so we can reference it later on
spectra.cor.df <- data.frame(spectra.cor)
names(spectra.cor.df) <- c("Correlation")
write.csv(spectra.cor.df, paste0(out.dir, in.var, '_Spectra_Correlations.csv', 
                                 sep = ""), 
          row.names = TRUE)

################################################################################
#lets do a jackknife test to find the number of components to include in our 
# PLSR model
################################################################################

#first lets find the dimensions of our dataset and set some parameters
dims <- dim(plsr.dataset)
n.comps <- 15
iterations <- 50
prop <- 0.80

#lets create an empty matrix to store our results in
jk.out <- matrix(data = NA, nrow = iterations, ncol = n.comps) 

#lets start a timer to see how long this takes to run
start.time <- Sys.time()

#lets run through all the different iterations of this
for (i in 1:iterations) {
  
  #remind ourselves what iteration we are on
  print(paste("Iteration: ", i, sep = ""))
  
  #lets take a sample from our dataset to test this on
  rows <- sample(1:nrow(plsr.dataset), floor(prop*nrow(plsr.dataset)))
  sub.data <- plsr.dataset[rows,]
  
  #lets run our PLSR model now
  plsr.out <- plsr(as.formula(paste(in.var,"~","spectra")), scale = FALSE, 
                   ncomp = n.comps, validation = "LOO",
                   trace = TRUE, data = sub.data)
  
  #lets save our press statistic in our empty matrix
  resPRESS <- as.vector(plsr.out$validation$PRESS)
  jk.out[i,seq(plsr.out$validation$ncomp)] = resPRESS
}

#lets find out how long it took the jackknife to run
end.time <- Sys.time()
end.time - start.time

#lets change our output matrix to a dataframe for easier manipulation
pressDF <- as.data.frame(jk.out)

#lets name the columns
names(pressDF) <- as.character(seq(n.comps))

#lets write this as a csv for later use
write.csv(pressDF, file = paste0(out.dir, in.var, 
                                 "_Jackkife_PLSR_Coefficients.csv"), 
          row.names = FALSE)

#lets melt the data for easier plotting
pressDFres <- melt(pressDF)

#lets see what our press statistics look like. small is better for this.
boxplot(pressDFres$value ~ pressDFres$variable, 
        xlab = "n Components",
        ylab = "PRESS",
        main = in.var)

# How many components? Can use this to determine if next largest is sig different
# than lower.  Then lower is best. 
# We can do this with a simple T-Test - a smaller PRESS statistic is better. so 
# lets see where this starts to vary. we want the lowest number of components so
# that we don't over predict our model.
loc.1 <- 8
loc.2 <- 9
ttest <- t.test(pressDFres$value[which(pressDFres$variable == loc.1)], 
                pressDFres$value[which(pressDFres$variable == loc.2)])

#By examining the out put we can determine what the best number of components are to avoid overfitting.
ttest

#since we see a low p-value we can see that there is no difference between the 
# two variables now. so lets go with the smaller value.
# Now that we know the number of test components lets run our PLSR model again 
# with that number of components.
nComps <- 8

plsr.out <- plsr(as.formula(paste(in.var, "~", "spectra")), scale = FALSE,
                 ncomp = n.comps, validation = "LOO",
                 trace = TRUE, data = plsr.dataset)

#lets save our fitted values
fit1 <- plsr.out$fitted.values[, 1, nComps]

#lets plot them to see what they look like
plot(c(20, 202), c(20, 202),
     xlab = "PLSR Fitted Values LMA",
     ylab = "Training Data LMA")
points(fit1, plsr.dataset[,in.var])
abline(lm(plsr.dataset[,in.var] ~ fit1), lwd = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)
summary(lm(plsr.dataset[,in.var] ~ fit1))

################################################################################
#lets run a standardized PLSR model  - WHAT IS THE POINT OF THIS? (Kyla)
################################################################################

#Now lets run a standardized model (scale = TRUE)
plsr.out.stand <- plsr(as.formula(paste(in.var, "~", "spectra")), scale = TRUE,
                       ncomp = nComps, validation = "LOO",
                       trace = TRUE, data = plsr.dataset)

################################################################################
#lets calculate some model statistics
################################################################################

#Generate some model statistics
PRESS <- plsr.out$validation$PRESS
SS <- sum((plsr.dataset[,in.var])^2)
TSS <- sum((plsr.dataset[,in.var] - mean(plsr.dataset[,in.var]))^2)
Q2 <- 1 - (PRESS/TSS)

#lets find the minimum PRESS statistic
minpress <- which.min(as.vector(PRESS))
minpress

#lets make a plot of our PRESS statistic
plot(as.vector(PRESS), main = "Model PRESS", xlab = "Num of Components",
     ylab = "PRESS", cex = 1.5, lty = 1)
abline(v = nComps, lty = 2, col = "black", lwd = 2)
abline(v = minpress, lty = 2, col = "dark grey", lwd=2)
legend("bottomleft", legend = c("Best", "Min/Max"), lty = 2,
       col = c("black", "dark grey"), lwd=2, bty = "n")

#Lets plot our RMSEP statistics
plot(RMSEP(plsr.out,estimate = c("train", "CV")), main = "Model RMSEP",
     xlab = "Num of Components", ylab = "Model RMSEP", lty = c(1, 2, 2),
     col = c("black", "red"), cex = 1.5, lwd = 2)
legend("bottomleft", legend = c("Train", "CV RMSEP (use this)"),
       col = c("black", "red"), lty = c(1, 2), lwd = c(2, 2), bty = "n")
abline(v = nComps, lty = 2, col = "black", lwd = 2)
abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)

#Q2 Plot
plot(as.vector(Q2), 
     main = "Model Q2", 
     xlab = "Num of Components", 
     ylab = "Q2", 
     cex = 1.5)
abline(v = nComps, lty = 2, col = "black", lwd = 2)
abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)

#R2 plot
plot(R2(plsr.out,estimate = c("train","CV")), main = "Model R2",
     xlab = "Num of Components", ylab = "Model R2", lty = c(1, 2), 
     col = c("black", "red"), cex = 1.5, lwd = 2)
legend("bottomright", legend = c("Train", "CV R2 (use this)"),
       col = c("black", "red", "blue"), lty = c(1, 2, 2), 
       lwd = c(2, 2, 2), bty = "n")
abline(v = nComps, lty = 2, col = "black", lwd = 2)
abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)


#find the dimensions of our plsr dataset
dims <- dim(plsr.dataset)

#Model fitted values. Predicted values
cal.plsr.pred <- as.vector(plsr.out$fitted.values[,,nComps])

#CV pred values
cal.plsr.CVpred <- as.vector(plsr.out$validation$pred[,,nComps]) 

#CV pred residuals
cal.CVresiduals <- as.vector(plsr.out$residuals[,,nComps]) 

#lets combine all this together into one data frame
plsr.output <- data.frame(plsr.dataset,cal.plsr.pred,cal.plsr.CVpred,cal.CVresiduals)
dims <- dim(plsr.output)

#lets calculate some model statistics
MSECV <- mean(cal.CVresiduals^2)
RMSECV <- sqrt(MSECV)
PERC_RMSE <- (RMSECV/(max(plsr.output[,in.var]) - min(plsr.output[,in.var])))*100
Train.Rsq <- R2(plsr.out,estimate = "train")$val[,,nComps + 1]
CV.Rsq <- R2(plsr.out, estimate = "CV")$val[,,nComps + 1]
Model.bias <- mean(cal.plsr.CVpred) - mean(plsr.dataset[,in.var])
names(Model.bias) <- "Model_bias"

#lets write all of this to a dataframe
cal_sum_stats = data.frame(Train_Rsq = Train.Rsq, CV_Train_Rsq = CV.Rsq,
                           RMSECV = RMSECV, PERC_RMSE = PERC_RMSE,
                           Model_bias = Model.bias)

#lets take a look at this dataframe now
cal_sum_stats

# PLSR Observed versus predicted plot & independent val plot using withheld 
# samples. Cal plot
plot(cal.plsr.pred, plsr.dataset[,in.var], pch = 21, bg = "grey60",
     cex = 1.5, xlab = "Predicted", ylab = "Observed", 
     main = paste0("Leaf ", in.var, " Calibration -- n: ", dim(plsr.dataset)[1]))
points(cal.plsr.CVpred, plsr.dataset[,in.var], pch = 21, cex = 1.5, 
       bg = "black")
legend("topleft", legend = c(paste("Train R2 = ", round(Train.Rsq, 2)), 
                             paste("CV R2 = ", round(CV.Rsq, 2)),
                             paste("RMSECV = ", round(RMSECV, 2)), 
                             paste("Perc RMSECV = ", round(PERC_RMSE, 2)),
                             paste("Bias = ", round(Model.bias, 4))), 
       cex = 0.75, 
       bty = "n")
abline(0, 1, lty = 2)
box(lwd = 2)

#Cal Residuals plot
plot(plsr.dataset[,in.var], 
     cal.CVresiduals, 
     xlab = paste0(in.var), 
     ylab = "PLSR Residuals", 
     pch = 21,
     bg = "grey60", cex = 1.5)
abline(h = 0, lty = 2, col = "grey60")
box(lwd = 2)

#lets make a histogram of the residuals
hist(cal.CVresiduals)

#Scores plot
plot(plsr.out, plottype = "scores", comps = 1:nComps)

#Loadings plot
plot(plsr.out, plottype = "loadings", comps = 1:4,
     legendpos = "topleft", xlab = "INDEX (400-2500nm)")
plot(plsr.out, plottype = "loadings", comps = 5:nComps,
     legendpos = "topleft",xlab = "INDEX (400-2500nm)")

#Loading weights and coefficients plot
weights <- loading.weights(plsr.out)[,1]
coefs <- coef(plsr.out, ncomp = nComps, intercept = FALSE)
plot(weights, lwd = 3, xlab = "INDEX (400-2500nm)", cex = 0.01)
lines(weights, lwd = 3)
abline(h = 0, lty = 2, lwd = 1.5, col = "grey60")

# kyla messed with this a bit but it should be right.
plot(keep.spectra, coefs[,1,], lwd = 3, xlab = "WAVELENGTH (nm)", cex = 0.01, 
     type = "l")
abline(h = 0, lty = 2, lwd = 1.5, col = "grey60")

################################################################################
#lets export our model results
################################################################################

#Observed versus predicted
write.csv(plsr.output, file = paste0(out.dir, in.var, '_Observed_PLSR_CV_Pred_', nComps, 
                                     'comp.csv', sep = ""), row.names = FALSE)

#Model coefficients
coefs <- coef(plsr.out, ncomp = nComps, intercept = TRUE)
write.csv(coefs, file = paste0(out.dir, in.var, '_PLSR_Coefficients_', nComps, 
                               'comp.csv', sep = ""),
          row.names = TRUE)

#standardized
coefs <- coef(plsr.out.stand, ncomp = nComps, intercept = TRUE)
write.csv(coefs,file = paste0(out.dir, in.var, '_Standardized_PLSR_Coefficients_', 
                              nComps, 'comp.csv', sep = ""),
          row.names = TRUE)

#Model loading weights
write.csv(weights, file = paste0(out.dir, in.var, '_PLSR_Loading_Weights_Comp1.csv', sep = ""))

#PLSR Model stats
write.csv(cal_sum_stats, file = paste0(out.dir, in.var, '_PLSR_Statistics_', nComps, 'comp.csv', sep = ""), row.names = FALSE)

################################################################################
#apply the PLSR results to spectra test data 
################################################################################

#lets read in the leaf nitrogen PLSR Coefficients
LMA.plsr.coeffs <- read.csv(paste0(out.dir, in.var, '_PLSR_Coefficients_8comp.csv'))

#lets find the dimensions of this dataset
dims <- dim(LMA.plsr.coeffs)

#lets find the intercept
LMA.plsr.intercept <-  LMA.plsr.coeffs[1,]

#lets find the coefficients
LMA.plsr.coeffs <- data.frame(LMA.plsr.coeffs[2:dims[1],])

#lets name these columns
names(LMA.plsr.coeffs) <- c("wavelength", "coefs")

#lets turn this into a vector
LMA.plsr.coeffs.vec <- as.vector(LMA.plsr.coeffs[,2])

#lets find the length of the vector
length(LMA.plsr.coeffs.vec)

#Estimate leaf N
temp <- as.matrix(spectra.test.data) %*% LMA.plsr.coeffs.vec
LMA <-  data.frame(rowSums(temp)) + LMA.plsr.intercept[,2]
hist(LMA[,1])
names(LMA) <- "PLSR.lma"

min(LMA)
max(LMA)

#lets put this all together into a data frame
ind <- names(spectra.test.data)
LMA.PLSR.dataset <- data.frame(test.data$Sample, LMA)
names(LMA.PLSR.dataset) <- c("Sample", "PLSR.lma")

#lets add a column with our lab tested nitrogen data
leaf.LMA.output <- merge(LMA.PLSR.dataset, n.test.data, by = "Sample")

#lets take a look at this real fast
plsr.LMA.lm <- lm(lma ~ PLSR.lma, data = leaf.LMA.output)
summary(plsr.LMA.lm)

plot(leaf.LMA.output$lma, leaf.LMA.output$lma, 
     main = "Lab LMA vs. PLSR LMA: Test Data (n = 48) -- adj R2 = 0.92",
     type = "n", ylab = "Field LMA", xlab = "PLSR LMA")
points(PLSR.lma ~ lma, data = leaf.LMA.output)
abline(plsr.LMA.lm, lty = 2, lwd = 1.5, col = "grey60")
abline(0,1)
box(lwd = 2)

#lets write our results to a csv
write.csv(leaf.LMA.output, file = paste0(out.dir,'PLSR_Leaf_LMA_Estimates.csv'), 
          row.names = FALSE)


################################################################################
# lets apply the PLSR coefficients to the other spectra data that was not tested 
# in the lab
################################################################################

#first lets rename the spectra variable
spec.data <- keep.data[,3:ncol(keep.data)]

#lets read in the leaf nitrogen PLSR Coefficients
LMA.plsr.coeffs <- read.csv(paste0(out.dir,'LMA_PLSR_Coefficients_8comp.csv'))

#lets find the dimensions of this dataset
dims <- dim(LMA.plsr.coeffs)

#lets find the intercept
LMA.plsr.intercept <-  LMA.plsr.coeffs[1,]

#lets find the coefficients
LMA.plsr.coeffs <- data.frame(LMA.plsr.coeffs[2:dims[1],])

#lets name these columns
names(LMA.plsr.coeffs) <- c("wavelength", "coefs")

#lets turn this into a vector
LMA.plsr.coeffs.vec <- as.vector(LMA.plsr.coeffs[,2])

#lets find the length of the vector
length(LMA.plsr.coeffs.vec)

#Estimate leaf N
temp <- as.matrix(spec.data) %*% LMA.plsr.coeffs.vec
LMA <-  data.frame(rowSums(temp)) + LMA.plsr.intercept[,2]
hist(LMA[,1])
names(LMA) <- "PLSR.lma"

min(LMA)
max(LMA)

#lets put this all together into a data frame
LMA.PLSR.dataset <- data.frame(all.data$Sample, LMA)
names(LMA.PLSR.dataset) <- c("Sample", "PLSR.lma")

# let's plot all the data for fun
plot(keep.data$lma, 
     keep.data$lma, 
     type = "n",
     xlab = "PLSR LMA",
     ylab = "Field LMA",
     main = "All HARV + SERC data (n = 93)")
points(LMA.PLSR.dataset$PLSR.lma, 
       keep.data$lma, 
       col = "darkgreen",
       pch = 20)
abline(0,1)
summary(lm(keep.data$lma ~ LMA.PLSR.dataset$PLSR.lma, 
           na.action = "na.exclude"))

#lets write our results to a csv
write.csv(LMA.PLSR.dataset, file = paste0(out.dir,'PLSR_Leaf_LMA_ALL_Estimates.csv'), row.names = FALSE)

# let's compare our lab measured values to the PLSR estimates

all.LMA <- merge(LMA.PLSR.dataset, chem.data[,c(1,6)], all = TRUE)

all.LMA$PLSR.LMA.only <- is.na(all.LMA$lma)*all.LMA$PLSR.lma
all.LMA$PLSR.LMA.only[all.LMA$PLSR.LMA.only == 0] <- NA

min(all.LMA$lma, na.rm = T)
max(all.LMA$lma, na.rm = T)

min(all.LMA$PLSR.LMA.only, na.rm = T)
max(all.LMA$PLSR.LMA.only, na.rm = T)


repl <- which(!is.na(all.LMA$lma))
all.LMA$out.lma <- replace(all.LMA$PLSR.LMA.only, 
                           repl, 
                           subset(all.LMA$lma, 
                                  !is.na(all.LMA$lma)))


all.LMA$out.lma[is.na(all.LMA$out.lma)] <- all.LMA$lma[!is.na(all.LMA$lma)]

write.csv(all.LMA, 
          file = paste0(out.dir, 'PLSR_lab_LMA_Estimates_FINAL.csv'),
          row.names = FALSE)

