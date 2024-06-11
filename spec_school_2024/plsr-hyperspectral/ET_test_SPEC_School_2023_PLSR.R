#################################################################################

# Partial Least Squares Regression (PLSR) code for exploring leaf traits with 
# hyperspectral data ***USING handheld spectra***

# PLSR code was originally sourced from:
# Dr. Shawn Serbin at NASA Goddard (shawn.p.serbin@nasa.gov)

# Data wrangling code for SPEC School 2023 data:
# Alanna Post (MSU ERSAM Lab)

# Edited and adapted by 
# 1. Dr. Kyla Dahlin at Michigan State University (MSU) -- kdahlin@msu.edu
# 2. Aaron Kamoske, PhD, when at MSU - kamoskea@msu.edu
# 3. Adriana Uscanga, Alanna Post, Sandhya Sharma, Tony Bowman (all MSU)
#
# latest update: 20240610

################################################################################

# this code takes leaf level N measurements and connects them to leaf level 
# spectral measurements using PLSR

#load the needed libraries
library(ggplot2)
library(tidyverse)
library(pls)
library(reshape2)
library(prospectr)
library(smoother)
library(GGally)

#todays date
today <- format(Sys.time(), "%Y%m%d")

# directory where you want to store/write stuff
home.dir <- "K:/SPEC_School_2024/"

# HPCC directory where data is stored
hpcc.dir <- "Z:/shared_data/foliar_chemistry/2023_SPEC_School/"

# set this as our working directory so we can easily pull in data
setwd(hpcc.dir)

#lets set up an output directory
dir.create(paste0(home.dir, "/N_practice_2023"))
dir.create(paste0(home.dir, "/N_practice_2023/outputFiles_", today, "_SCRATCH"))

# so we can point to it later ->
out.dir <- paste0(home.dir, "/N_practice_2023/outputFiles_", today, "_SCRATCH/")

#-------------------------------------------------------------------------------
# let's read in and clean up our data
#-------------------------------------------------------------------------------

field_data <- read.csv("MLBS_foliar_data_2023.csv")
lab_data <- read.csv("solid_samples_MLBS_2-2-24.csv")
spec_data <- read.csv("MLBS2023_SPEC_School_average_Spectra_20230623.csv")

View(field_data) #appears to be an issue with this data table, we'll fix this 
#in the data wrangling chunk
View(lab_data) #looks fine, but there are some odd variable names carried 
#over from the original .xlsx
view(spec_data) # these data were preprocessed so they look good except for a
# capitalization issue

#remove extra columns and rows that only contain NAs, 
##the select function has to go first to prevent the drop_na function from 
#removing everything due to the easting and northing columns
field_data <- field_data %>% 
  select(1:3, 6:10) %>% 
  drop_na() %>% 
  rename(sample_ID = "Unique.ID", date = "Date", species = "Species", 
         wet_mass_g = "Wet.Mass..g.", dry_mass_g = "Dry.Mass..g.", 
         area_m2 = "Area..m2.", lma_m2 = "LMA..g.m2.")

#make Date a date variable rather than a numeric one
field_data$date <- ymd(field_data$date)

head(field_data) #check the formatting

#similar process for the lab data, but we need to ensure that the sample ID 
#columns match so we can join the 2 datasets

lab_data <- lab_data %>% 
  select(3:7, 9:10) %>% 
  rename(sample_ID = "Sample.ID", mass_mg = "mass..mg.", 
         sample_type = "Sample.Type", expected_N = "X..N..Expected.", 
         expected_C = "X..C..Expected.", nitrogen = "X.N", carbon = "X.C")
#replace the space in the sample ID with an underscore
lab_data$sample_ID <- gsub(x = lab_data$sample_ID, pattern = "MLBS ", 
                           replacement = "MLBS_")

head(lab_data) #check formatting

# now let's capitalize the sample names for the spectral data
spec_data$ID <- toupper(spec_data$ID)
names(spec_data)[1] <- "sample_ID"

#merge/join the field and lab data frames using the common 'sample_ID' field
trait_data <- left_join(field_data, lab_data, by = "sample_ID")

# merge/join the spectral data
all_data <- left_join(trait_data, spec_data, by = "sample_ID")

# let's have a look
view(all_data)

# let's take a look at the trait data!
#correlation plots
ggpairs(select(.data = all_data, lma_m2, nitrogen, carbon, species))

#pull out the 3 scatterplots so they're easier to see (and color by species)
ggplot(data = trait_data, aes(x = nitrogen, y = carbon, color = species)) +
  geom_point()

ggplot(data = trait_data, aes(x = nitrogen, y = lma_m2, color = species)) +
  geom_point()

ggplot(data = trait_data, aes(x = lma_m2, y = carbon, color = species)) +
  geom_point()

# get wavelength names for clean plotting - note that spectra columns are hard
# coded so need to check with new data sets
wavelengths <- as.numeric(substring(colnames(all_data[,15:1006]),3))

# let's make a plot to see how this all looks - note that spectra columns are
# hard coded
matplot(wavelengths,
        t(all_data[15:1006]), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "HSI Reflectance",
        main = "ALL 2018 leaf level data"
          )

# since some of these look funky, let's figure out who they are...
text(rep(1000, nrow(all_data)), 
     all_data[,508],
     labels = all_data$sample_ID,
     cex = 0.7)

# remove the funky spectra
funky <- c("MLBS_026", "MLBS_027", "MLBS_015")
rem <- which(all_data$sample_ID == funky[1] | 
               all_data$sample_ID == funky[2] | 
               all_data$sample_ID == funky[3])

print(all_data[rem, 1:15])

all_data <- all_data[-rem,] # remove those rows

# now let's clean up the data so we just have plot IDs, the trait values, and
# the spectra. You might also want to try subsetting the spectral range here if
# the full spectra gives not great results
keep.data <- all_data[,c(1, 13, 15:1006)]

# keep the data that have leaf chem and spectra
chem.spec.data <- subset(keep.data, !is.na(keep.data$nitrogen))

# let's make a generic copy of our data set so we don't have to re-write this 
# every time we use a different data source
data.plsr <- chem.spec.data

# lets take ~20% of our data for testing and leave the other 80% for training
# this should sample more or less evenly across the distribution of N values
# note that you might want to do this differently depending on the trait you
# are predicting
test.n <- round(nrow(data.plsr)*0.2)

test.data <- sample_n(data.plsr, test.n, 
                      replace = FALSE, 
                      weight = data.plsr$nitrogen)

# then the rest of the chem.spec.data are training data
training.data <- data.plsr[!(data.plsr$sample_ID %in% test.data$sample_ID),]

# lets subset out the spectra data so we can just use that
# this is getting ready to format the data to work with the PLRS function
spectra.training.data <- training.data[,3:ncol(training.data)]
n.training.data <- training.data[,1:2]

spectra.test.data <- test.data[,3:ncol(test.data)]
n.test.data <- test.data[,1:2]

################################################################################
#lets set up our data for PLSR
################################################################################

#first lets set some options inside the pls package
pls.options(plsralg = "oscorespls")
pls.options("plsralg")

#lets save the name of the N variable we are using so that if it changes from 
# csv to csv we don't have to change the code
in.var <- "leaf.N"

#lets put our dataset into the correct format for running the PLSR
plsr.spectra <- as.matrix(spectra.training.data)
plsr.dataset <- data.frame(leaf.N = n.training.data$nitrogen, 
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
matplot(wavelengths,
        t(plsr.spectra), 
        type = "l", 
        xlab = "Wavelength (nm)",
        ylab = "HSI Reflectance",
        main = "2018 All Sites HSI spectra - training data")

#lets take a look at the correlation between the spectra and biochemical data
plot(wavelengths,
     spectra.cor, 
     xlab = "Wavelength (nm)", 
     ylab = "Correlation", 
     type = "l", 
     lwd = 4)
abline(h = 0,lty = 2, lwd = 1.5, col = "grey80")

#lets write this data to a csv so we can reference it later on if we need to
spectra.cor.df <- data.frame(spectra.cor)
names(spectra.cor.df) <- c("Correlation")
write.csv(spectra.cor.df, paste0(out.dir, in.var, '_Spectra_Correlations.csv', 
                                 sep = ""), 
          row.names = TRUE)

################################################################################
#lets do a jackknife test to find the number of components to include in our 
# PLSR model (this will take a few minutes to run)
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

# lets see what our press statistics look like. small is better for this. the 
# goal here is to find the lowest number of components that minimizes the PRESS
# statistic. Lower number will avoid over fitting
boxplot(pressDFres$value ~ pressDFres$variable, 
        xlab = "n Components",
        ylab = "PRESS",
        main = in.var)

# How many components? Can use this to determine if next largest is sig different
# than lower.  Then lower is best. 
# We can do this with a simple T-Test - a smaller PRESS statistic is better. so 
# lets see where this starts to vary. we want the lowest number of components so
# that we don't over predict our model.
loc.1 <- 2
loc.2 <- 3
ttest <- t.test(pressDFres$value[which(pressDFres$variable == loc.1)], 
                pressDFres$value[which(pressDFres$variable == loc.2)])

# By examining the out put we can determine what the best number of components 
# are to avoid overfitting.
ttest

# if we see a low p-value we can see that there is no difference between the 
# two variables now. then lets go with the smaller value.
# Now that we know the number of test components lets run our PLSR model again 
# with that number of components.
nComps <- 3

plsr.out <- plsr(as.formula(paste(in.var, "~", "spectra")), scale = FALSE,
                 ncomp = n.comps, validation = "LOO",
                 trace = TRUE, data = plsr.dataset)

#lets save our fitted values
fit1 <- plsr.out$fitted.values[, 1, nComps]

#lets plot them to see what they look like
plot(c(0,4), c(0,4),
     type = "n",
     xlab = "PLSR Fitted Values %N",
     ylab = "Training Data %N")
points(fit1, plsr.dataset[,in.var])
abline(lm(plsr.dataset[,in.var] ~ fit1), lwd = 2)
abline(0, 1, col = "red", lwd = 2, lty = 2)
summary(lm(plsr.dataset[,in.var] ~ fit1))

# store the model coefficients to apply to test data
coefs <- coef(plsr.out, ncomp = nComps, intercept = TRUE)

################################################################################
#lets calculate some model statistics - save these figures for near final model
################################################################################
# 
# #Generate some model statistics
# PRESS <- plsr.out$validation$PRESS
# SS <- sum((plsr.dataset[,in.var])^2)
# TSS <- sum((plsr.dataset[,in.var] - mean(plsr.dataset[,in.var]))^2)
# Q2 <- 1 - (PRESS/TSS)
# 
# #lets find the minimum PRESS statistic
# minpress <- which.min(as.vector(PRESS))
# minpress
# 
# #lets make a plot of our PRESS statistic
# plot(as.vector(PRESS), main = "Model PRESS", xlab = "Num of Components",
#      ylab = "PRESS", cex = 1.5, lty = 1)
# abline(v = nComps, lty = 2, col = "black", lwd = 2)
# abline(v = minpress, lty = 2, col = "dark grey", lwd=2)
# legend("bottomleft", legend = c("Best", "Min/Max"), lty = 2,
#        col = c("black", "dark grey"), lwd=2, bty = "n")
# 
# #Lets plot our RMSEP statistics
# plot(RMSEP(plsr.out,estimate = c("train", "CV")), main = "Model RMSEP",
#      xlab = "Num of Components", ylab = "Model RMSEP", lty = c(1, 2, 2),
#      col = c("black", "red"), cex = 1.5, lwd = 2)
# legend("bottomleft", legend = c("Train", "CV RMSEP (use this)"),
#        col = c("black", "red"), lty = c(1, 2), lwd = c(2, 2), bty = "n")
# abline(v = nComps, lty = 2, col = "black", lwd = 2)
# abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)
# 
# #Q2 Plot
# plot(as.vector(Q2), main = "Model Q2", xlab = "Num of Components", ylab = "Q2", 
#cex = 1.5)
# abline(v = nComps, lty = 2, col = "black", lwd = 2)
# abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)
# 
# #R2 plot
# plot(R2(plsr.out,estimate = c("train","CV")), main = "Model R2",
#      xlab = "Num of Components", ylab = "Model R2", lty = c(1, 2), 
#      col = c("black", "red"), cex = 1.5, lwd = 2)
# legend("bottomright", legend = c("Train", "CV R2 (use this)"),
#        col = c("black", "red", "blue"), lty = c(1, 2, 2), 
#        lwd = c(2, 2, 2), bty = "n")
# abline(v = nComps, lty = 2, col = "black", lwd = 2)
# abline(v = minpress, lty = 2, col = "dark grey", lwd = 2)
# 
# #find the dimensions of our plsr dataset
# dims <- dim(plsr.dataset)
# 
# #Model fitted values. Predicted values
# cal.plsr.pred <- as.vector(plsr.out$fitted.values[,,nComps])
# 
# #CV pred values
# cal.plsr.CVpred <- as.vector(plsr.out$validation$pred[,,nComps]) 
# 
# #CV pred residuals
# cal.CVresiduals <- as.vector(plsr.out$residuals[,,nComps]) 
# 
# #lets combine all this together into one data frame
# plsr.output <- data.frame(plsr.dataset, cal.plsr.pred, cal.plsr.CVpred,
#                           cal.CVresiduals)
# dims <- dim(plsr.output)
# 
# #lets calculate some model statistics
# MSECV <- mean(cal.CVresiduals^2)
# RMSECV <- sqrt(MSECV)
# PERC_RMSE <- (RMSECV/(max(plsr.output[,in.var]) - min(plsr.output[,in.var])))*100
# Train.Rsq <- R2(plsr.out,estimate = "train")$val[,,nComps + 1]
# CV.Rsq <- R2(plsr.out, estimate = "CV")$val[,,nComps + 1]
# Model.bias <- mean(cal.plsr.CVpred) - mean(plsr.dataset[,in.var])
# names(Model.bias) <- "Model_bias"
# 
# #lets write all of this to a dataframe
# cal_sum_stats = data.frame(Train_Rsq = Train.Rsq, CV_Train_Rsq = CV.Rsq,
#                            RMSECV = RMSECV, PERC_RMSE = PERC_RMSE,
#                            Model_bias = Model.bias)
# 
# #lets take a look at this dataframe now
# cal_sum_stats
# 
# #PLSR Observed versus predicted plot
# plot(cal.plsr.pred, plsr.dataset[,in.var], pch = 21, bg = "grey60",
#      cex = 1.5, xlab = "Predicted", ylab = "Observed", 
#      main = paste0("Leaf ", in.var, " Calibration -- n: ", dim(plsr.dataset)[1]))
# points(cal.plsr.CVpred, plsr.dataset[,in.var], pch = 21, cex = 1.5, 
#        bg = "black")
# legend("topleft", legend = c(paste("Train R2 = ", round(Train.Rsq, 2)), 
#                              paste("CV R2 = ", round(CV.Rsq, 2)),
#                              paste("RMSECV = ", round(RMSECV, 2)), 
#                              paste("Perc RMSECV = ", round(PERC_RMSE, 2)),
#                              paste("Bias = ", round(Model.bias, 4))), 
#        cex = 0.75, 
#        bty = "n")
# abline(0, 1, lty = 2)
# box(lwd = 2)
# 
# #Cal Residuals plot
# plot(plsr.dataset[,in.var], 
#      cal.CVresiduals, 
#      xlab = paste0(in.var), 
#      ylab = "PLSR Residuals", 
#      pch = 21,
#      bg = "grey60", cex = 1.5)
# abline(h = 0, lty = 2, col = "grey60")
# box(lwd = 2)
# 
# #lets make a histogram of the residuals
# hist(cal.CVresiduals)
# 
# #Scores plot
# plot(plsr.out, plottype = "scores", comps = 1:nComps)
# 
# #Loadings plot
# plot(plsr.out, plottype = "loadings", comps = 1:4,
#      legendpos = "topleft", xlab = "INDEX (400-2500nm)")
# plot(plsr.out, plottype = "loadings", comps = 5:nComps,
#      legendpos = "topleft",xlab = "INDEX (400-2500nm)")
# 
# #Loading weights and coefficients plot
# weights <- loading.weights(plsr.out)[,1]
# coefs <- coef(plsr.out, ncomp = nComps, intercept = FALSE)
# plot(weights, lwd = 3, xlab = "INDEX (500-2400nm)", cex = 0.01)
# lines(weights, lwd = 3)
# abline(h = 0, lty = 2, lwd = 1.5, col = "grey60")


################################################################################
#lets export our model results if we like them
################################################################################
# 
# #Observed versus predicted
# write.csv(plsr.output, file = paste0(out.dir, in.var, '_Observed_PLSR_CV_Pred_', 
#                                       nComps, 
#                                      'comp.csv', sep = ""), row.names = FALSE)
# 
# #Model coefficients
# coefs <- coef(plsr.out, ncomp = nComps, intercept = TRUE)
# write.csv(coefs, file = paste0(out.dir, in.var, '_PLSR_Coefficients_', nComps, 
#                                'comp.csv', sep = ""),
#           row.names = TRUE)
# 
# #standardized
# coefs <- coef(plsr.out.stand, ncomp = nComps, intercept = TRUE)
# write.csv(coefs,file = paste0(out.dir, in.var, '_Standardized_PLSR_Coefficients_', 
#                               nComps, 'comp.csv', sep = ""),
#           row.names = TRUE)
# 
# #Model loading weights
# write.csv(weights, file = paste0(out.dir, in.var, 
#                                  '_PLSR_Loading_Weights_Comp1.csv', sep = ""))
# 
# #PLSR Model stats
# write.csv(cal_sum_stats, file = paste0(out.dir, in.var, '_PLSR_Statistics_', 
#                                        nComps, 'comp.csv', sep = ""), 
#           row.names = FALSE)

################################################################################
#apply the PLSR results to spectra test data 
################################################################################

#lets read in the leaf nitrogen PLSR Coefficients
LeafN.plsr.coeffs <- as.data.frame(coefs)

#lets find the dimensions of this dataset
dims <- dim(LeafN.plsr.coeffs)

#lets find the intercept
LeafN.plsr.intercept <-  LeafN.plsr.coeffs[1,]

#lets find the coefficients
LeafN.plsr.coeffs <- data.frame(LeafN.plsr.coeffs[2:dims[1],])

# add a wavelength column
LeafN.plsr.coeffs$wavelength <- wavelengths

#lets name these columns
names(LeafN.plsr.coeffs)[1] <- "coefs"

#lets turn this into a vector
LeafN.plsr.coeffs.vec <- as.vector(LeafN.plsr.coeffs[,1])

#lets find the length of the vector
length(LeafN.plsr.coeffs.vec)

# now let's throw out some wavelengths to match the above
#spectra.test.data <- spectra.test.data[,keep.cors]

#Estimate leaf N
temp <- as.matrix(spectra.test.data) %*% LeafN.plsr.coeffs.vec
leafN <-  data.frame(rowSums(temp)) + LeafN.plsr.intercept
hist(leafN[,1])
names(leafN) <- "PLSR.N"

min(leafN)
max(leafN)

#lets put this all together into a data frame
ind <- names(spectra.test.data)
LeafN.PLSR.dataset <- data.frame(test.data$sample_ID, leafN)
names(LeafN.PLSR.dataset) <- c("sample_ID", "PLSR.N")

#lets add a column with our lab tested nitrogen data
leaf.n.output <- merge(LeafN.PLSR.dataset, n.test.data, by = "sample_ID")

#lets take a look at this real fast
plsr.n.lm <- lm(nitrogen ~ PLSR.N, data = leaf.n.output)
summary(plsr.n.lm)

plot(c(0,3), c(0,3), 
     main = "Lab N vs. PLSR N: Test Data (n = 6) -- adj R2 = ??? n.s.",
     type = "n", ylab = "Field %N", xlab = "PLSR %N")
points(nitrogen ~ PLSR.N, data = leaf.n.output)
abline(plsr.n.lm, lty = 2, lwd = 1.5, col = "grey60")
abline(0,1)
box(lwd = 2)

#lets write our results to a csv (if we like them)
write.csv(leaf.n.output, file = paste0(out.dir,'PLSR_Leaf_N_Estimates.csv'), 
          row.names = FALSE)

