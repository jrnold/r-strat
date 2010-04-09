# .Rprofile to set up this environment.
homeProfile <- file.path(Sys.getenv("HOME"), ".Rprofile")
if (file.access(homeProfile) >= 0)
    source(homeProfile)

## Load libraries
library(tools)
## library(svUnit)

PkgRoot <- file_path_as_absolute("..")
PkgUnitTests <- file.path(PkgRoot, "inst", "unitTests")
PkgData <- file.path(PkgRoot, "data")
PkgR <- file.path(PkgRoot, "R")

## Load all file in data/
myPkgData <- function() {
    lapply(dir("../data", pattern="\\.rda(ta)?$",
               ignore.case=TRUE, full.names=TRUE), "load", env=.GlobalEnv)
}

## Source all files in R/
myPkgSource <- function() {
    for (f in (dir("../R", pattern="\\.[rR]$", full.names=TRUE))) {
        source(f)
    }
}

## Run All Unit Tests
myTests <- function() {
    myTestSuite <- defineTestSuite("rino Unit Tests", pkgUnitTests)
    runTestSuite(myTestSuite)
}

.First <- function() {
    require(stats)
    require(stats4)
    require(graphics)
    require(svUnit)

    ## myPkgData()
    myPkgData()
    myPkgSource()

###     ## Not working right now: Trying to create shortcut functions to call unit tests
###     unittestdir <- file_path_as_absolute("../inst/unitTests")
###     fileList <- dir(unitTestDir, "^runit.+\\.[rR]$")
###     fileList <- substr(fileList, 1, nchar(fileList) - 2)
###     lapply(fileList, function(f)
###           assign(f, function() runTestFile(file.path(unitTestDir, f)),
###                  pos=sys.frame))

}

.Last <- function() {}
