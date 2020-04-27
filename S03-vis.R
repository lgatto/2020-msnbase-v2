f <- dir(system.file("extdata", package = "MSnbaseBoxCar"),
         pattern = "boxcar.mzML",
         full.names = TRUE)
basename(f)

x <- readMSData(f, mode = "onDisk") %>%    
    bc_groups()

fData(x)$msLevel <- ifelse(is.na(fData(x)$bc_groups), 1, 2)

map <- MSmap(x, 1:8, 200, 1000, .005)

vis0 <- plot3D(map)





