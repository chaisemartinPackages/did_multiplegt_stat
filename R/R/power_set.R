power_set <- function(set, sep = ":") {
    pwdf <- data.frame(subset = "", len = 0, id = 1:(2^length(set) -1))
    for (j in 1:nrow(pwdf)) {
        temp <- set[rev(set_fill(as.bin(j), length(set))) * 1:length(set)]
        pwdf$subset[j] <- paste(temp, collapse = sep)
        pwdf$len[j] <- length(temp)
    }
    return(pwdf[order(pwdf$len, pwdf$subset), ]$subset)    
}

as.bin <- function(x) {
    bin <- rev(as.integer(intToBits(x)))
    return(bin[match(1, bin, length(bin)):length(bin)])
}

set_fill <- function(set, n) {
    if (length(set) < n) {
        set <- c(rep(0,n-length(set)), set)
    }
    return(set)
}