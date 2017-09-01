# Package: UniDOE
### Author: Haoyu Li and Aijun Zhang 
### date: 22/08/2017
### Depends: 
32-bit R(>= 3.4.1)
### Imports: 
Rcpp (>= 0.12.12)
### Linking To: 
Rcpp
### Introduction:
UniDOE is a R package, which implements efficient algorithms to search for design of matrix. Users can either download and install from binary source package or install from github directly using devtools, details of which are illustrated below. This package is distributed in the hope that it will be useful, but without any warranty.
### How to install:
At first, Make sure you are using 32-bit R(>=3.4.1).

Then download and install Rcpp(>=0.12.12) package if you haven't installed or updated it to >=0.12.12 version.

#### First way
git clone this repostory.

In R command-line: 

##### install.packages(file.choose(),repos=NULL)

Choose UniDOE_0.1.1.zip to install Or install it from GUI.

#### Second Way
Make sure you installed devtools.

In R:

##### library(devtools)

##### TOKEN <- c("7eec33dedd0291418c5e4879b5e323ccc48386b4")

##### install_github(repo="HAOYU-LI/UniDOE_src", auth_token=TOKEN)

