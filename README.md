# Investment Analysis
An R and Rmarkdown package analyzing top stocks and crypto returns and prices.

## Installation
The following code will install the package on your machine.

##### NOTE: This may take a long time given some of the computation times.
```
if(!'remotes' %in% installed.packages()){
  install.packages('remotes', dependencies = TRUE)
}
if(!'rlang' %in% installed.packages()){
  remotes::install_github('r-lib/rlang', dependencies = TRUE)
}
remotes::install_github("Jwychor/Crypto-Analysis")
```
