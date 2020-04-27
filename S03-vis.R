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

## Raw MS profile
serine <- data_prof %>%
    filterRt(rtr) %>%
    filterMz(mzr)

## Processing
serine_proc <- data_prof %>%
    smooth(method = "SavitzkyGolay", halfWindowSize = 4L) %>% 
    pickPeaks(refineMz = "descendPeak") %>%
    filterRt(rtr) %>%
    filterMz(mzr)


pdf("./figure/centroiding.pdf", width = 9, height = 6)
layout(matrix(1:4, ncol = 2))
plot(serine, type = "XIC", layout = NULL)
abline(h = serine_mz, col = "red", lty = 2)
plot(serine_proc, type = "XIC", layout = NULL)
abline(h = serine_mz, col = "red", lty = 2)
abline(v = rtime(serine_sg_cent_mz)[22], col = "red", lty = 3)
dev.off()

