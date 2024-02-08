rm(list = ls())

library(devtools)
library(usethis)
library(haven)

setwd("C:/Users/39380/C DE CHAISEMARTIN Dropbox/RAs De Chaisemartin/RAs Really Credible DID-TWFE/GitHub repo - local/did_continuous")
load_all("DIDcontinuous")
repo <- "chaisemartinPackages/ApplicationData/main" 
file <- "data_gazoline.dta"
url <- paste("https://raw.githubusercontent.com", repo, file, sep = "/")
gazoline <-  haven::read_dta(url)

# AOSS - RA
summary(did_continuous(
    df = gazoline,
    Y = "lngca",
    ID = "id",
    T = "year",
    D = "tau",
    order = 2,
    estimator = "aoss",
    estimation_method = "ra",
    placebo = TRUE,
    noextrapolation = TRUE
))

# WAOSS - RA
summary(did_continuous(
    df = gazoline,
    Y = "lngca",
    ID = "id",
    T = "year",
    D = "tau",
    order = 2,
    estimator = "waoss",
    estimation_method = "ra",
    placebo = TRUE,
    noextrapolation = TRUE
))

# WAOSS - PS
summary(did_continuous(
    df = gazoline,
    Y = "lngca",
    ID = "id",
    T = "year",
    D = "tau",
    order = 2,
    estimator = "waoss",
    estimation_method = "ps",
    placebo = TRUE,
    noextrapolation = TRUE
))

# WAOSS - DR
summary(did_continuous(
    df = gazoline,
    Y = "lngca",
    ID = "id",
    T = "year",
    D = "tau",
    order = 2,
    estimator = "waoss",
    estimation_method = "dr",
    placebo = TRUE,
    noextrapolation = TRUE
))

# IWAOSS - RA
summary(did_continuous(
    df = gazoline,
    Y = "lngca",
    ID = "id",
    T = "year",
    D = "lngpinc",
    Z = "tau",
    order = 2,
    estimator = "iwaoss",
    estimation_method = "ra",
    placebo = TRUE,
    noextrapolation = TRUE
))