VaisalaImport <- function(filename, column = 25){
    ## Determination of size in bytes
    size <- file.info(filename)$size
    stopifnot(size > 0) ## check case of empty file
### Read whole file
    newdata <- file(filename, "rb")
    rawData <- readBin(newdata, "raw", n = size)
    close(newdata)
    tempStart <- which(rawData == "00")
### Process header
    ## End of header
    tempHeadEnd <- which(rawData == as.raw("0xfe"))
    tempHeadEndOK <- rawData[tempHeadEnd + 1] == as.raw("0xff")
    HeadEnd <- tempHeadEnd[tempHeadEndOK]
    if(length(HeadEnd) > 1){
        tempHeadEndOK <- rawData[HeadEnd + 2] == as.raw("0xff")
        HeadEnd <- HeadEnd[tempHeadEndOK]
        if(length(HeadEnd) > 1){
            warning("HeadEnd longer than one! First is used.")
            HeadEnd <- HeadEnd[1]
        }
    }
    ## Header strings
    tempStartOK <- rawData[tempStart + 1] == "00" & rawData[tempStart + 2] == "00"
    StartHead <- tempStart[tempStartOK]
    if(rawToChar(rawData[(StartHead[1]+3):(StartHead[2]-1)]) != "B")
        warning("First character is not B!")
    ## Substract var names locations
    StartVarName <- StartHead[-1]
    StartVarName <- c(StartVarName[StartVarName < HeadEnd], HeadEnd)
    ## Create pointer to substarction
    HeadPointer <- seq(1, length(StartVarName) - 1, by = 2)
    HeadTextLength <- length(HeadPointer)
    ## Create empty variables
    HeadNames <- character(HeadTextLength)
    HeadTypes <- character(HeadTextLength)
    ## Varname substraction loop
    for(tti in 1:HeadTextLength) {
        HeadIndex <- HeadPointer[tti]
        HeadNames[tti] <- rawToChar(rawData[(StartVarName[HeadIndex] + 4):(StartVarName[HeadIndex + 1] - 1)])
        HeadTypes[tti] <- rawToChar(rawData[(StartVarName[HeadIndex + 1] + 4):(StartVarName[HeadIndex + 2] - 2)])
    }
    Head.df <- data.frame(HeadNames, HeadTypes)
### Process data
    ## Determine beginning of the values
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
