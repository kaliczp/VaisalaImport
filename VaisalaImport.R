VaisalaImport <- function(filename){
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
    nr.precipit <- which(Record.Number == 25)
### Create precipitation matrix
    ## Empty matrix
    prec.mat <- matrix(raw(), nrow = 144, ncol = 4)
    ## Fill prec.mat row-by-row
    for(prec.point in 1:length(nr.precipit)){
        prec.start <- Start[nr.precipit[prec.point]] + 3
        prec.end <- Start[nr.precipit[prec.point]+1] -1
        prec.mat[prec.point,] <- rawData[prec.start:prec.end]
    }
    prec.mat
}
