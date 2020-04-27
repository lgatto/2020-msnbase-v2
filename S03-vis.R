library("MSnbase")
library("msdata")
library("magrittr")

fl <- dir(system.file("sciex", package = "msdata"), full.names = TRUE)[2]
basename(fl)
data_prof <- readMSData(fl, mode = "onDisk", centroided = FALSE)

## Define the mz and retention time ranges
serine_mz <- 106.049871
mzr <- c(serine_mz - 0.01, serine_mz + 0.01)
rtr <- c(175, 187)

## Filtering the object
serine <- data_prof %>%
    filterRt(rtr) %>%
    filterMz(mzr)


data_sg <- data_prof %>%
    smooth(method = "SavitzkyGolay", halfWindowSize = 4L)


## Use pickPeaks with descendPeak m/z refinement
data_sg_cent_mz <- data_sg %>%
    pickPeaks(refineMz = "descendPeak")

serine_sg_cent_mz <- data_sg_cent_mz %>%
    filterRt(rtr) %>%
    filterMz(mzr)


pdf("./figure/centroiding.pdf", width = 12, height = 7)
layout(matrix(1:4, ncol = 2))
plot(serine, type = "XIC", layout = NULL)
abline(h = serine_mz, col = "red", lty = 2)
plot(serine_sg_cent_mz, type = "XIC", layout = NULL)
abline(h = serine_mz, col = "red", lty = 2)
abline(v = rtime(serine_sg_cent_mz)[22], col = "red", lty = 3)
dev.off()
