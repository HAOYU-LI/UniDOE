# Package: UniDOE
### Author: Aijun Zhang and Haoyu Li
### date: 22/08/2017
### Depends: 
32-bit R(>= 3.4.1), Rtools(>=34)
### Imports: 
Rcpp (>= 0.12.12)
### Linking To: 
Rcpp
### Introduction:
UniDOE is a R package, which implements an efficient stochastic evolutionary(SE) algorithm to search for [design of experiment](https://en.wikipedia.org/wiki/Design_of_experiments). Computational procedures are mainly achieved by c++ so that the calculation speed is greatly boosted. Users can either download and install from binary source package or install from github directly using devtools, of which details are illustrated below. This package is distributed in the hope that it will be useful, but without any warranty.
### How to install:
At first, Make sure you are using 32-bit R(>=3.4.1). Typing 'version' in R command line can retrieve related information, e.g.:
```
version
```
Output should show corresponding R version and architecture of your current platform:
```
platform       i386-w64-mingw32            
arch           i386                        
os             mingw32                     
system         i386, mingw32               
status                                     
major          3                           
minor          4.1                         
year           2017                        
month          06                          
day            30                          
svn rev        72865                       
language       R                           
version.string R version 3.4.1 (2017-06-30)
nickname       Single Candle 
```

#### First way

Download and install Rcpp(>=0.12.12) package if you haven't installed or updated it to >=0.12.12 version.

In R command-line: 
```
# It's easy to install Rcpp from CRAN
install.packages("Rcpp")
```
Git clone this repostory to your local machine. After that, you can install UniDOE from local files:

```
install.packages(file.choose(),repos=NULL)
```

Choose UniDOE_0.1.1.zip to install Or install it from GUI.

#### Second Way
It's more convenient to install UniDOE from github. At first, make sure you installed devtools.

```
install.packages("devtools")
```

Then install UniDOE from github:

```
library(devtools)
install_github(repo="HAOYU-LI/UniDOE")
```
### Useful links:
* [Experimental design](https://en.wikipedia.org/wiki/Design_of_experiments) - Intro to design of experiments
* [License](https://github.com/HAOYU-LI/UniDOE/blob/master/LICENSE) - License for this project
* [Maintainer](http://www.statsoft.org/)

