# Crypto Analysis
An R Shiny and Plotly package analyzing top stocks and crypto returns and prices.

## Installation
The following code will install the package on your machine.

```
if(!'remotes' %in% installed.packages()){
  install.packages('remotes', dependencies = TRUE)
}
if(!'rlang' %in% installed.packages()){
  remotes::install_github('r-lib/rlang', dependencies = TRUE)
}
remotes::install_github("Jwychor/Crypto-Analysis")
```
# Examples
![image](https://github.com/Jwychor/Crypto-Analysis/blob/master/Images/Dashboard1.JPG)

A live demo of the dashboard hosted on shinyapps.io can be found [here.](https://jwychor.shinyapps.io/InvestmentAnalysis/)
