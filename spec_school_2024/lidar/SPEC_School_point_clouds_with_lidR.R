###############################################################################
# Looking at NEON Lidar point clouds with Lidr
###############################################################################

library(lidR)

# we're not going to write anything today, so we can just set our working
# directory to the HPCC - looking at the 2018 data because we have all of that
# downloaded from NEON already

#setwd("Z:/shared_data/NEON_AOP_data/MLBS/2018/lidar/ClassifiedPointCloud/")

setwd("/Volumes/ersamlab/shared_data/NEON_AOP_data/MLBS/2018/lidar/ClassifiedPointCloud/")

# let's look at the list of lidar tile names
files <- list.files(".")

print(files)

# now let's read one in! pick any index number - I just know 153 is a good tile
in.laz <- readLAS(files[153])

# what type of file is this?
class(in.laz)

# how is it organized?
names(in.laz)

# how big is it?
dim(in.laz)

# let's take a look (this will be slow)
plot(in.laz)

# lots of noise! how do we remove? let's look at the classification info:
unique(in.laz$Classification)

# what do these numbers mean? ask google (these are standardized)
# 0 = not classified
# 1 = unassigned
# 2 = ground
# 3 = low vegetation
# 4 = medium vegetation
# 5 = high vegetation
# 6 = building
# 7 = noise
# etc (up to 18 but we will mainly see 1-7)

# let's remove the noise values
laz.nonoise <- subset(in.laz, in.laz$Classification != 7)

# now let's look
plot(laz.nonoise)

# how many points are unassigned?
sum(laz.nonoise$Classification == 1)

# is that a big fraction?
sum(laz.nonoise$Classification == 1) / nrow(laz.nonoise)

# Congratulations! Now you know how to visualize lidar point clouds :)
