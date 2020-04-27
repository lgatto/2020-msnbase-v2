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

x <- bc_groups(x)
xbc <- filterBoxCar(x)
xbc2 <- bc_zero_out_box(xbc, offset = 0)
res  <- combineSpectra(xbc2,
                       fcol = "bc_groups",
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




## 3D MS map
x <- readMSData(f, mode = "onDisk") %>%    
    bc_groups()
## use MS level to draw the full and BoxCar spectra in different
## colours
fData(x)$msLevel <- ifelse(is.na(fData(x)$bc_groups), 1, 2)

map <- MSmap(x, 1:12, 200, 1000, .005)

lattice::trellis.par.set("axis.line",
                         list(col = NA,lty = 1,lwd = 1))
pmap <- plot3D(map)


save(p1, p2, p3, p4, pmap, file = "boxcar.rda")
