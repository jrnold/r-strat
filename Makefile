## Makefile for building and testing the package
TOP=.
PKG=${shell cd ${TOP}; pwd}
R=R
## Sets the library paths to include my personal libraries
RLIB=${shell Rscript -e 'cat(paste(.libPaths(), collapse=":"), "\\n")'}

all: foo

check: 	
	export R_LIBS="${RLIB}";\
	R CMD check ${PKG} 

test: 	
	export R_LIBS="${RLIB}";\
	R CMD check --no-clean --no-codoc --no-latex ${PKG} 

