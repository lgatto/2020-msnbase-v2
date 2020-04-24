library("MSnbase")

prep_data <- function(n,
                      f = msdata::proteomics(full.names = TRUE, pattern = "20141210"),
                      data_dir = "./data",
                      clean = TRUE) {
    if (clean && dir.exists(data_dir))
        unlink(data_dir, recursive = TRUE)
    if (!dir.exists(data_dir))
        dir.create(data_dir)
    fls <- paste0(data_dir, "/f", seq_len(n), ".mzML")
    stopifnot(all(file.link(f, fls)))
    fls
}

fls <- prep_data(100)


## ----------------------------------------------------------------------
## BENCHMARK:
## - Reading time (in seconds) for a 1, 5, 10, ... files (default
##   settings, i.e. 6103 MS2 spectra for in memory and 1431 MS1 + 6103
##   MS2 spectra for on disk)
## - Memory consumption, compareing on disk and in memory for 1, 5,
##   10, ... files
## ----------------------------------------------------------------------
n <- c(1, 5, 10)
time_sz <- lapply(n, function(i) {
    f <- fls[seq_len(i)]
    time <- c(in_mem = system.time(x_mem <- readMSData(f, mode = "inMemory"))[["elapsed"]],
              on_disk = system.time(x_dsk <- readMSData(f, mode = "onDisk"))[["elapsed"]])
    sz <- c(in_mem = pryr::object_size(x_mem),
            on_disk = pryr::object_size(x_dsk))
    cbind(time, sz, n = i)
})

save(time_sz, file = "bench_time_sz.rda")

## ----------------------------------------------------------------------
## BENCHMARK: filtering
## ----------------------------------------------------------------------

library(tidyverse)
library(microbenchmark)

## Documenting the filters used:
x <- readMSData(fls[1], mode = "onDisk")
chr <- chromatogram(x)

## 1. keep 1000 < rtimes < 3000
plot(chr, col = "black")

## 2. 50% of MS2 spectra with most intense precuror intensity
median(precursorIntensity(x), na.rm = TRUE)

## 3. Focus on TMT
plot(x_dsk[[1227]], full = TRUE, reporters = TMT6)
TMT6
range(mz(TMT6))

x_dsk <- readMSData(fls[1], mode = "onDisk", msLevel = 2L)
x_mem <- readMSData(fls[1], mode = "inMemory", msLevel = 2L)

filter_ms <- function(x, i = NULL) {
    x <- x %>%
        filterRt(c(1000, 3000)) %>%
        filterMz(120, 135)
    x <- x[precursorIntensity(x) > 11e6, ]
    if (!is.null(i))
        x <- x[seq_len(i), ]
    x
}


t_filt <- microbenchmark(dsk = filter_ms(x_dsk),
                         mem = filter_ms(x_mem),
                         times = 10)

save(t_filt, file = "bench_t_filt.rda")

autoplot(t_filt)

## ----------------------------------------------------------------------
## BENCHMARK: accessing 1, 10, ..., all spectra
## ----------------------------------------------------------------------

t_access <- microbenchmark(
    access_mem_1 = spectra(x_mem[1]),
    access_mem_10 = spectra(x_mem[1:10]),
    access_mem_100 = spectra(x_mem[1:100]),
    access_mem_1000 = spectra(x_mem[1:1000]),
    access_mem_5000 = spectra(x_mem[1:5000]),
    access_mem_all = spectra(x_mem),    
    access_dsk_1 = spectra(x_dsk[1]),
    access_dsk_10 = spectra(x_dsk[1:10]),
    access_dsk_100 = spectra(x_dsk[1:100]),
    access_dsk_1000 = spectra(x_dsk[1:1000]),
    access_dsk_5000 = spectra(x_dsk[1:5000]),
    access_dsk_all = spectra(x_dsk),
    times = 10)

save(t_access, file = "bench_t_access.rda")

tibble(time = microbenchmark:::convert_to_unit(t_access$time, "ms"),
       expr = as.character(t_access$expr)) %>%
    mutate(n = sub("^access_.+_", "", expr)) %>%
    mutate(n = sub("all", length(x_dsk), n)) %>%
    mutate(mode = if_else(grepl("mem", expr), "inMem", "onDisk")) %>%
    ggplot(aes(x = n, y = time, fill = log10(as.numeric(n)))) + 
    ggplot2::geom_violin() + 
    ggplot2::scale_y_log10() +
    ylab("Time [millisec]") +
    xlab("Number of spectra (out of 6103)") +
    facet_wrap(~ mode) +
    theme(legend.position = "none")

## ----------------------------------------------------------------------
## BENCHMARK: All together for 1, 5000, all spectra
## ----------------------------------------------------------------------

t_full <- microbenchmark(mem_all = readMSData(fls[1], mode = "inMemory", msLevel = 2L) %>%
                             filter_ms() %>%
                             spectra(),
                         mem_1 = readMSData(fls[1], mode = "inMemory", msLevel = 2L) %>%
                             filter_ms(i = 1) %>%
                             spectra(),
                         mem_1000 = readMSData(fls[1], mode = "inMemory", msLevel = 2L) %>%
                             filter_ms(i = 1000) %>%
                             spectra(),               
                         dsk_all = readMSData(fls[1], mode = "onDisk", msLevel = 2L) %>%
                             filter_ms() %>%
                             spectra(),               
                         dsk_1 = readMSData(fls[1], mode = "onDisk", msLevel = 2L) %>%
                             filter_ms(i = 1) %>%
                             spectra(),
                         dsk_1000 = readMSData(fls[1], mode = "onDisk", msLevel = 2L) %>%
                             filter_ms(i = 1000) %>%
                             spectra(),
                         times = 10)

save(t_full, file = "bench_t_full.rda")

tibble(time = microbenchmark:::convert_to_unit(t_full$time, "s"),
       expr = as.character(t_full$expr)) %>%
    mutate(n = sub("^.+_", "", expr)) %>%
    mutate(n = sub("all", length(x_dsk), n)) %>%
    mutate(mode = if_else(grepl("mem", expr), "inMem", "onDisk")) %>%
    ggplot(aes(x = n, y = time, fill = log10(as.numeric(n)))) + 
    ggplot2::geom_violin() + 
    ggplot2::scale_y_log10() +
    ylab("Time [seconds]") +
    xlab("Number of spectra (out of 6103)") +    
    facet_wrap(~ mode) +
    theme(legend.position = "none")




