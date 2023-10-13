# README

For replication:
- install `renv` package by `install.packages("renv")`
- install all required packages using `renv::restore()`
- run the script `run.R` 

All steps for data preparation are declared in `_targets.R` script. 
Models are estimated, tables and charts are produced in `.Rmd` files. 

## Data sources  

- Czechia, 2023 presidential election, data: [Czech presidential panel 2023]  
- Czechia, 1996 parliamentary election, data: [Pre-election study](https://doi.org/10.4232/1.3631) & [Post-election study](https://doi.org/10.4232/1.3633)  
- Germany, 2017 parlimentary election, data: [GLES Panel](https://doi.org/10.4232/1.14114)  
- Romania, 2009 presidential election, data: [Romanian election studies 2009](https://doi.org/10.7910/DVN/0L8N8C)  
- Romania, 2012 parlimentary election, data: [Romanian election studies 2012](https://doi.org/10.7910/DVN/3F7D4J)  
- Poland, 2019 EP election, data: [RECONNECT 2019 European Parliament Election Panel Survey](https://doi.org/10.11587/MOV0EZ)  
- Hungary, 2019 EP election, data: [RECONNECT 2019 European Parliament Election Panel Survey](https://doi.org/10.11587/MOV0EZ)  
