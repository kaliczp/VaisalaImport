VaisalaImport <- function(filename, column = 25){
    ## Determination of size in bytes
    size <- file.info(filename)$size
    stopifnot(size > 0) ## check case of empty file
    ### Process header
    newdata <- file(filename, "rb")
    rawHead <- readBin(newdata, "character", n=400)
    HeadNames <- rawHead[seq(7, by = 6, length=66)]
    HeadTypes <- rawHead[seq(10, by = 6, length=66)]
    Head.df <- data.frame(HeadNames, HeadTypes)
    rawData <- readBin(newdata, "raw", n = size - 400)
    close(newdata)
    ## Determine beginning of the values
    tempStart <- which(rawData == "00")
    tempStartOK <- rawData[tempStart + 1] == "01"
    Start <- tempStart[tempStartOK]
    ## if(!all(paste0(as.character(rawData[Start]),as.character(rawData[Start+1])) == "0001"))
    ## Data record identifier
    Record.Number <- as.numeric(rawData[Start+2])
    ## Where are precipitation codes?
    nr.precipit <- which(Record.Number == column)
### Create precipitation vector
    ## Empty vector
    prec.vec <- numeric(144)
    ## Fill prec.vec
    for(prec.point in 1:length(nr.precipit)){
        prec.start <- Start[nr.precipit[prec.point]] + 3
        prec.end <- Start[nr.precipit[prec.point]+1] -1
        prec.raw <- rawData[prec.start:prec.end]
        prec.vec[prec.point] <- readBin(prec.raw, "double", size = 4,  endian="big")
    }
    prec.vec
}
