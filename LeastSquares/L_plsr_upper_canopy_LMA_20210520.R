### now to do plsr to predict leaf mass per area in airborne HSI data
### note: this code is cribbed from Aaron's code for PLSR on the field spec
### data - which is a mashup of my old code and Shawn's

library(pls)
library(reshape2)
library(dplyr)
library(prospectr)
library(smoother)
options(digits = 10) #i just do this because it's nice to see the big numbers

rm(list = ls())

###############################################################################

# this code will produce some stuff
# where do you want to save that info?
setwd("/Users/gracemcleod/Documents/SPEC/In_person_week/LeastSquares")
today <- format(Sys.Date(), "%Y%m%d")
dir.create(paste0("./output_", today))
dir.create(paste0("./figures_", today))
fig.dir <- paste0("./figures_", today, "/")
loc <- "UMBS"

# read in wavelengths file for plotting
wavelengths <- read.csv("NEON_HSI_wavelengths_20210528.csv",
                        stringsAsFactors = FALSE)

wavelengths <- wavelengths$wavelengths

# merge dataframes
all.data <- read.csv("UMBS2019_ALL_point_DATA_merged_20210616.csv")
             

# now NA-out 'bad bands' (noisy bands, as determined by visual inspection of the
# hsi data)
bad.band.nums <- c("wave.384", "wave.389", "wave.394", "wave.1355",  "wave.1360", 
                   "wave.1365", "wave.1370", "wave.1375", "wave.1380", "wave.1385",
                   "wave.1390", "wave.1395", "wave.1400", "wave.1405", "wave.1410",
                   "wave.1415", "wave.1420", "wave.1425", "wave.1811", "wave.1816",
                   "wave.1821", "wave.1826", "wave.1831", "wave.1836", "wave.1841",
                   "wave.1846", "wave.1851", "wave.1856", "wave.1861", "wave.1866",
                   "wave.1871", "wave.1876", "wave.1881", "wave.1886", "wave.1891",
                   "wave.1896", "wave.1901", "wave.1906", "wave.1911", "wave.1916",
                   "wave.1921", "wave.1926", "wave.1931", "wave.1936", "wave.1941",
                   "wave.1946", "wave.1951", "wave.1956", "wave.1961", "wave.2477",
                   "wave.2482", "wave.2487", "wave.2492", "wave.2497", "wave.2502",
                   "wave.2507", "wave.2512")

# sub NAs for all of the bad band columns
for (i in 1:length(bad.band.nums)) {
  x <- which(names(all.data) == bad.band.nums[i])
  all.data[,x] <- NA
}

# combine the sample info with the reflectance values scaled to 0 to 1
# first subset to only observations (rows) 1:15
# then divide the wavelength columns (16:441) by 100
clean.data.1.0 <- cbind(all.data[,1:15], all.data[16:441]/100)

# try some gaussian filtering
# tried to do this fancier but gave up and wrote a for-loop (only takes a few
# seconds to run anyway)
# gets rid of noise while keeping a gausian distribution
clean.data.smth <- clean.data.1.0

for (i in 1:dim(clean.data.1.0)[1]) {
  x <- as.numeric(clean.data.1.0[i,16:441])
  clean.data.smth[i,16:441] <- smth.gaussian(x,
                                             window = 7,
                                             alpha = 1,
                                             tails = FALSE)
  print(i)
}


# subset out just the upper canopy samples
upper.clean.data <- subset(clean.data.smth, 
                           clean.data.1.0$canopy_position == "T")

# take a look!
x11()
matplot(wavelengths, 
        t(upper.clean.data[,16:441]), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Reflectance",
        main = "UMBS upper canopy sample refl")

# use this set of plots to troubleshoot individual species/points based on later
# results
matplot(wavelengths, 
        t(subset(upper.clean.data[10:435], upper.clean.data$SP == "PIST")), 
        type = "l", 
        col = rgb(231,138,195, maxColorValue = 255),
        xlab = "Wavelength (nm)",
        ylab = "Reflectance",
        main = "SERC upper canopy sample refl (6 species, n = 65)")

# average bands together to improve signal:noise

# first get the new wavelength names by averaging the old ones
waves.avg <- NA

# create an index of numbers counting up by 2 (for each of the sets of averaged
# wavelengths)
count.2 <- seq(from = 2, to = 426, by = 2)
for (i in 1:length(count.2)) {
  x <- count.2[i]
  waves.avg[i] <- mean(c(wavelengths[x], wavelengths[x-1]), na.rm = TRUE)
}

# make the new names
waves.avg.r <- round(waves.avg, 0)
wave.names <- paste0("wave.", waves.avg.r)

# now actually average each pair of bands
# set up output data
clean.data.avg <- as.data.frame(matrix(NA, 
                                       nrow = nrow(all.data), 
                                       ncol = (15+length(wave.names))))
# write the names
names(clean.data.avg) <- c(names(all.data)[1:15], 
                           wave.names)
# keep all of the non spectral info the same
clean.data.avg[,1:15] <- all.data[,1:15]

# now create a new index that will call each column from 'clean.data' starting
# with the first wavelength column (in this case column 10)
count.3 <- count.2+14
# count.3 should run from 10 to 434 counting by 2s (dim(all.data) = 331 435)

for (k in 1:length(wave.names)) {
  clean.data.avg[,k+15] <- rowMeans(cbind(clean.data.smth[,count.3[k]],
                                clean.data.smth[,count.3[k]+1]),
                                na.rm = T)
}

matplot(waves.avg, 
        t(subset(clean.data.avg[,16:228], clean.data.avg$canopy_position == "T")), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "Reflectance",
        main = "UMBS upper canopy sample refl (10 nm)"
        )

## for now not averaging
#clean.data.avg <- clean.data.smth

## now let's remove the NA columns ##
no.nas <- !is.na(clean.data.avg)
keep.cols <- colSums(no.nas)
which.keep.cols <- which(keep.cols > 0)

# overwriting the clean.data file here!
clean.data <- clean.data.avg[,which.keep.cols]

# also get the wavelengths remaining for plotting, etc
keep.waves <- as.numeric(which.keep.cols[16:length(which.keep.cols)]) - 15
clean.waves <- waves.avg[keep.waves]

# take a look
x11()
matplot(clean.waves, t(clean.data[16:ncol(clean.data)]), type = "l")

# write this out to a file for safe keeping
write.csv(clean.data, paste0("./output_", today, "/",
                           loc, "_averaged_data_ALL_", today, ".csv"))

# shorten wavelength range to 500 to 2400 nm
# clean.data.short <- clean.data[,c(1:9, 21:188)]

# # now let's try continuum removal (using subtraction, which is misspelled in this
# # R package, which I am more annoyed by than is probably reasonable)
# clean.data.CR <- continuumRemoval(clean.data[,10:360],
#                                   wav = clean.waves,
#                                   type = "R",
#                                   interpol = "linear",
#                                   method = 'substraction')
# # 
# # # take a look at the difference
# matplot(clean.waves, t(clean.data.CR), type = "l")
# # 
# # # check and make sure there are no NAs
# which(rowSums(is.na(clean.data.CR)) > 0)
# # 
# # # check to remove columns of all 1s (should just be first and last)
# which(colSums(clean.data.CR) == 331)
# # 
# # # new dataset
# CR.data <- cbind(clean.data[,1:9], clean.data.CR[,-c(1,351)])
# clean.waves <- clean.waves[-c(1,351)]

# next up is brightness normalization (Feilhauer et al 2010)

spectra <- clean.data[,16:ncol(clean.data)]
bn.spectra <- spectra / sqrt(apply(spectra^2, 1, sum)) 
# eqn 3 in Feilhauer et al 2010

matplot(clean.waves, t(bn.spectra), type = "l")

BN.data <- cbind(clean.data[,1:15], bn.spectra)

write.csv(BN.data, paste0("./output_", today, "/",
                          loc, "_brightness_norm_avg_data_ALL_", today, ".csv"),
          row.names = FALSE)

# # for now removing some weird spectra - NOT DOING THIS FOR NOW...
# rem <- which(BN.data$pointIDs == "harv012" | BN.data$pointIDs == "harv120" 
#              | BN.data$pointIDs == "harv113" | BN.data$pointIDs == "serc042")
# 
# BN.data <- BN.data[-rem,]

### for algorithm development we're only going to use the upper canopy samples
upper.BN.data <- subset(BN.data, BN.data$canopy_position == "T")

#upper.data <- subset(clean.data.0.1, clean.data.0.1$CANP == "U")

# take another look
matplot(clean.waves, 
        t(upper.BN.data[,16:ncol(upper.BN.data)]), 
        type = "l",
        xlab = "Wavelength (nm)",
        ylab = "Reflectance (BN)",
        main = "UMBS upper canopy Brightness Normalized refl (10 nm)")

# drop two weird samples
# upper.CR.data <- subset(upper.CR.data, upper.CR.data$Sample != "HARV018")
# upper.CR.data <- subset(upper.CR.data, upper.CR.data$Sample != "HARV022")

### before we do anything else we'll pull out a subset test data set to keep for
### testing the algorithms. Because we want an even sample across species this
### will be semi-random. we're going to pull out 2 per species, or a ~18% subsample

species <- unique(upper.BN.data$species)

upper.data <- upper.BN.data
upper.train <- NA
upper.test <- NA

for (j in 1:length(species)) {
  upper.sp <- upper.data[upper.data$species == species[j],]
  n <- dim(upper.sp)[1]
  rand <- runif(n)
  rand.sort <- sort(rand)
  test.loc <- rand == rand.sort[1] | rand == rand.sort[2]
  print(n - sum(test.loc == 0))
  train.out <- subset(upper.sp, test.loc == 0)
  test.out <- subset(upper.sp, test.loc)
  upper.train <- rbind(upper.train, train.out)
  upper.test <- rbind(upper.test, test.out)
}

upper.train <- upper.train[-1,]
upper.test <- upper.test[-1,]

# make a df of who is test and who is training
train.test <- upper.train[,c(1,3)]
train.test$train <- 1
train.test <- merge(train.test, upper.test[,c(1,3)],
                    all = TRUE)
write.csv(train.test, paste0("./output_", today,"/UMBS_train_test_IDs_", today,".csv"),
          row.names = FALSE)

# writing these to file since we don't want to re-run this, as then everything
# else will change (if the test/train subsets change)

write.csv(upper.train, paste0("./output_", today,"/UMBS_training_data_HSI_avg_PLSR_", today,".csv"),
          row.names = FALSE)

write.csv(upper.test, paste0("./output_", today,"/UMBS_test_data_HSI_avg_PLSR_", today,".csv"),
          row.names = FALSE)

## for LMA removing UMBS017 (PIST) as it leads to weird results
#upper.train <- subset(upper.train, upper.train$ID != "UMBS017")

##### NOW to start the plsr ####

#first lets set some options inside the pls package based on Shawn's code
## will need to ask him why
pls.options(plsralg = "oscorespls")
pls.options("plsralg")

#lets save the name of the LMA variable we are using so that if it changes from csv to csv we don't have to change the code
in.var <- "PLSR_LMA"

#lets put our dataset into the correct format for running the PLSR
# doing some cleaning first
# ditch <- which(upper.data$PLSR_N < 1.6)
# upper.data <- upper.data[-ditch,]

plsr.spectra <- as.matrix(upper.train[,16:ncol(upper.train)])
plsr.dataset <- data.frame(PLSR_LMA = (upper.train$LMA_gm2), 
                           spectra = I(plsr.spectra))

test.spectra <- as.matrix(upper.test[,16:ncol(upper.test)])
test.dataset <- data.frame(PLSR_N = upper.test$LMA_gm2, spectra = I(test.spectra))


###############################################################################
#lets take a look at the correlations between the spectra and the biochemical data
###############################################################################

#lets take a quick look at the correlations between the spectra and biochemical data
# Kyla note: I don't understand why this grep fcn is so complicated...
# spectra.cor <- cor(plsr.spectra, 
#                    plsr.dataset[grep(in.var, names(plsr.dataset), fixed = TRUE)], 
#                    use = "complete.obs")

spectra.cor <- cor(plsr.dataset$PLSR_LMA, 
                   plsr.dataset$spectra, 
                   use = "complete.obs")

#lets take a look at the spectra - due to the naming of the bands in the csv there is not an x axis range
plot(clean.waves, 
     seq(from = -0.8, to = 0.8, length = length(clean.waves)),
     type = "n",
     xlab = "Wavelength (nm)", 
     ylab = "Correlation / Refl (scaled)")

matplot(clean.waves, t(plsr.spectra)*2, 
        type = "l", 
        lty = 1, 
        ylab = "Reflectance (%)", 
        xlab = "Wavelength (nm)",
        add = TRUE)

#lets take a look at the correlation between the spectra and biochemical data
points(clean.waves,
      spectra.cor, 
      lwd = 4)
abline(h = 0,lty = 2, lwd = 1.5, col = "grey80")

# now let's figure out which spectra to keep
spec.cors <- as.data.frame(cbind(clean.waves, t(spectra.cor)))
names(spec.cors) <- c("wave","cor")

#lets write this data to a csv so we can reference it later on
write.csv(spec.cors, 
          paste0("./output_", today, "/",
                 in.var, '_Spectra_Correlations_', today, ".csv"), 
          row.names = TRUE)

# # now let's throw out some wavelengths!
# keep.cors <- which(spec.cors$cor > 0.2 | spec.cors$cor < -0.1)
# 
# plsr.dataset$spectra <- plsr.dataset$spectra[,keep.cors]


############################################################################################################################
#lets do a jackknife test to find the number of components to include in our PLSR model
############################################################################################################################

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
# write.csv(pressDF, file = paste0(in.var, "_", all, 
#                                  "_Jackkife_PLSR_Coefficients",
#                                  today,
#                                  ".csv"), 
#           row.names = FALSE)

#lets melt the data for easier plotting
pressDFres <- melt(pressDF)

#lets see what our press statistics look like. small is better for this.
boxplot(pressDFres$value ~ pressDFres$variable, 
        xlab = "n Components",
        ylab = "PRESS",
        main = in.var)

#I have no idea how to determine what works here so I'm just guessing. Need to ask Shawn or reread his paper.
#How many components? Can use this to determine if next largest is sig different than lower.  Then lower is best. 
#We can do this with a simple T-Test
#a smaller PRESS statistic is better. so lets see where this starts to vary. we want the lowest number of components so that
#we don't over predict our model.
loc.1 <- 2
loc.2 <- 3
ttest <- t.test(pressDFres$value[which(pressDFres$variable == loc.1)], 
                pressDFres$value[which(pressDFres$variable == loc.2)])

#By examining the out put we can determine what the best number of components are to avoid overfitting. Need to ask Shawn about this.
ttest

#since we see a low p-value we can see that there is no difference between the two variables now. so lets go with the smaller value.
#Now that we know the number of test components lets run our PLSR model again with that number of components.

nComps <- 2

plsr.out <- plsr(PLSR_LMA ~ spectra, scale = FALSE,
                 ncomp = nComps, validation = "LOO",
                 trace = TRUE, data = plsr.dataset)
plot(plsr.out)

# plot(plsr.dataset$PLSR_N, plsr.out$fitted.values[,,1], type = "n")
# graphics::text(plsr.dataset$PLSR_N, plsr.out$fitted.values[,,1], 
#                labels = upper.data$pointIDs, 
#                cex = 0.8)

validationplot(plsr.out)

#lets save our fitted values
fit1 <- (plsr.out$fitted.values[, , nComps])

#lets plot them to see what they look like
png(filename = paste0(fig.dir, "toc_LMA_obs_pred_", today, ".png"),
    width = 7, height = 7, units = "in", res = 300)

par(mar = c(4,4,3,3))
plot(upper.train$LMA_gm2, upper.train$LMA_gm2, 
     type = "n",
     xlab = "Modelled LMA (g/m2)",
     ylab = "Measured LMA (g/m2)",
     main = "PLSR Modelled Top of Canopy LMA - TRAINING DATA")
text(fit1, upper.train$LMA_gm2,
     labels = upper.train$ID)
abline(lm(upper.train$LMA_gm2 ~ fit1), lwd = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)
graphics::text(fit1, 
               upper.train$LMA_gm2, fit1, 
               labels = upper.BN.data$SP, 
               cex = 0.6)

dev.off()

summary(lm(upper.train$LMA_gm2 ~ (fit1)))

# predict LMA on the test data then plot against measured values
plsr.predicted.test <- predict(plsr.out, 
                          ncomp = nComps, 
                          newdata = test.spectra)

plot(upper.train$LMA_gm2, upper.train$LMA_gm2, 
     type = "n",
     xlab = "Modelled LMA (g/m2)",
     ylab = "Measured LMA (g/m2)",
     main = "PLSR Modelled Top of Canopy LMA - TEST DATA")
points(plsr.predicted.test, upper.test$LMA_gm2)
abline(lm(upper.test$LMA_gm2 ~ plsr.predicted.test), lwd = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)


summary(lm(upper.test$LMA_gm2 ~ (plsr.predicted.test)))

############################## OLD STUFF BELOW #################################


out.predicted.lma <- as.data.frame(cbind(BN.data$Sample, plsr.predicted))
names(out.predicted.lma) <- c("Sample", "plsr.pred.lma")

write.csv(out.predicted.lma, paste0("./output_", today, "/",
                 in.var, '_predicted_values_', today, ".csv"), 
          row.names = TRUE)

png(filename = paste0(fig.dir, "toc_LMA_pred_hist_", today, ".png"),
    width = 7, height = 5, units = "in", res = 300)
hist(plsr.predicted, col = "cadetblue3", 
     main = "Histogram of Predicted TOC LMA (n = 331)",
     xlab = "TOC LMA (g/m2)")
dev.off()





################# FOR LATER ####################################################

plot(upper.test$PLSR_N, upper.test$PLSR_N, type = "n",
     xlab = "Predicted Leaf N (%)",
     ylab = "Measured Leaf N (%)")
graphics::text(sqrt(plsr.predicted), 
               upper.test$PLSR_N, 
               labels = upper.test$SP, 
               cex = 0.8)
abline(lm(upper.test$PLSR_N ~ sqrt(plsr.predicted)), lwd = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)

summary(lm(upper.test$PLSR_N ~ sqrt(plsr.predicted)))


# NOW APPLY TO ALL VALUES

TOC.N <- predict(plsr.out,
                 ncomp = 7,
                 newdata = as.matrix(BN.data[,11:196]))

BN.data$TOC.N <- sqrt(TOC.N)




