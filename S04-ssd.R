## This script assumes that the same files are available in on an SSD drive
## (ssd_dir) and hard drive (hd_dir).

ssd_dir <- "./data"
hd_dir <- "~/disk/tmp/data/"


library("MSnbase")

###############################
## Compare object creation

res <- data.frame(n = c(1, 10, 50, 100))
res$sata <- res$ssd <- rep(NA, 4)

for (i in seq_len(nrow(res))) {
    n <- res[i, "n"]
    fls_ssd <- dir(ssd_dir, full.names = TRUE)[1:n]
    fls_hd <- dir(hd_dir, full.names = TRUE)[1:n]
    res[i, "ssd"] <- system.time(readMSData(fls_ssd, mode = "onDisk"))[["elapsed"]]
    res[i, "sata"] <- system.time(readMSData(fls_hd, mode = "onDisk"))[["elapsed"]]
}

res$ratio <- res$ssd / res$sata

saveRDS(res, file = "bench_ssh.rds")

res
##     n     sata     ssd     ratio
## 1   1    7.917   7.936 1.0023999
## 2  10   87.955  79.196 0.9004150
## 3  50  482.165 396.744 0.8228387
## 4 100 1001.587 807.840 0.8065600


###############################
## Compare access to data

library(microbenchmark)

#########################################
## Access spectra from a single file

x1_ssd <- readMSData(dir(ssd_dir, full.names = TRUE)[1], mode = "onDisk")
x1_hd <- readMSData(dir(hd_dir, full.names = TRUE)[1], mode = "onDisk")

b <- microbenchmark(spectra(x1_ssd[1:100]),
                    spectra(x1_hd[1:100]),
                    times = 10L)

## Results
## No noticeable difference when accessing 100 spectra from a single file
## > Unit: seconds
##                expr      min       lq     mean   median       uq      max    neval
## spectra(x1_ssd[1:100]) 1.858352 1.864124 1.882133 1.871416 1.902859 1.923378    10
##  spectra(x1_hd[1:100]) 1.855695 1.860831 1.896199 1.881418 1.889071 2.084152    10

#########################################
## Access spectra from different files

x_ssd <- readMSData(dir(ssd_dir, full.names = TRUE)[1:50], mode = "onDisk")
x_hd <- readMSData(dir(hd_dir, full.names = TRUE)[1:50], mode = "onDisk")
## access one spectrum per file
i <- which(!duplicated(fromFile(x_ssd)))

b <- microbenchmark(spectra(x_ssd[i]),
                    spectra(x_hd[i]),
                    times = 1L)

## 10 files/spectra
## Unit: seconds
##               expr      min       lq     mean   median       uq      max neval
##  spectra(x_ssd[i]) 13.75523 13.75523 13.75523 13.75523 13.75523 13.75523     1
##   spectra(x_hd[i]) 14.17998 14.17998 14.17998 14.17998 14.17998 14.17998     1

## 50 files/spectra
## Unit: seconds
##               expr      min       lq     mean   median       uq      max neval
##  spectra(x_ssd[i])  67.1182  67.1182  67.1182  67.1182  67.1182  67.1182     1
##   spectra(x_hd[i]) 105.2481 105.2481 105.2481 105.2481 105.2481 105.2481     1

b <- microbenchmark(spectra(x_ssd[i]),
                    spectra(x_hd[i]),
                    times = 5L)
