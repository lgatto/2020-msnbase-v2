## -------------------------------------
## Based on the MSnbaseBoxCar vignette
## -------------------------------------

library("MSnbaseBoxCar")
library("magrittr")
library("ggplot2")
library("patchwork")

f <- dir(system.file("extdata", package = "MSnbaseBoxCar"),
         pattern = "boxcar.mzML",
         full.names = TRUE)
basename(f)
x <- readMSData(f, mode = "onDisk") %>%
    filterMz(c(299, 555))

fData(x)$groups <- bc_groups(x)
xbc <- x[bc_is_boxcar(x)]
xbc2 <- xbc %>%
    bc_zero_out_box(offset = 0)

res  <- combineSpectra(xbc,
                       fcol = "groups",
                       method = boxcarCombine)


p1 <- plot(x[[1]], plot = FALSE) +
    labs(title = element_text("")) +
    xlab(element_text(""))

p2 <- plot(x[2:4], plot = FALSE) +
    labs(title = element_text("")) +
    xlab(element_text(""))

p01 <- bc_plot(xbc[2:4]) +
    xlim(290, 455) +
    xlab(element_text(""))
p02 <- bc_plot(xbc2[2:4]) +
    xlim(290, 455) +
    xlab(element_text("")) + 
    ylab(element_text(""))
p3 <- p01 + p02

p4 <- plot(res[[1]], plot = FALSE) +
    labs(title = element_text(""))


save(p1, p2, p3, p4, file = "boxcar.rda")



