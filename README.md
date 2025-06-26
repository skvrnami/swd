# README

Replication materials for: Kostelka, F., Linek, L., Rovný, J. and M. Škvrňák. Electoral Participation and Satisfaction with Democracy in Central and Eastern Europe. _Political Science Research and Methods._ doi:[10.1017/psrm.2025.10028](https://www.cambridge.org/core/journals/political-science-research-and-methods/article/electoral-participation-and-satisfaction-with-democracy-in-central-and-eastern-europe/F6E33BBD14ED0286F909FA8272FFA0F8)

For replication:  

- open `swd.Rproj` file in RStudio/your preferred IDE  
- install `renv` package by `install.packages("renv")`  
- install all required packages using `renv::restore()`, the dependencies are declared in `renv.lock`  
- run the script `run.R` (which runs all the steps for data preparation declared in `_targets.R` script)  
- run `swd_final.Rmd` for estimating models and producing tables and charts   

Session info:  
```
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS 15.3.1

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Prague
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
[1] targets_1.7.1 dplyr_1.1.4  

loaded via a namespace (and not attached):
 [1] utf8_1.2.4             generics_0.1.3         tidyr_1.3.1           
 [4] renv_0.16.0            modelsummary_2.1.1     stringi_1.8.4         
 [7] digest_0.6.36          magrittr_2.0.3         evaluate_0.24.0       
[10] grid_4.4.0             fastmap_1.2.0          jsonlite_1.8.8        
[13] processx_3.8.4         nnet_7.3-19            backports_1.5.0       
[16] secretbase_1.0.0       Formula_1.2-5          ps_1.7.7              
[19] gridExtra_2.3          purrr_1.0.2            fansi_1.0.6           
[22] scales_1.3.0           codetools_0.2-20       cli_3.6.3             
[25] rlang_1.1.4            visNetwork_2.1.2       munsell_0.5.1         
[28] Hmisc_5.1-3            withr_3.0.0            base64enc_0.1-3       
[31] yaml_2.3.9             tools_4.4.0            checkmate_2.3.1       
[34] htmlTable_2.4.2        base64url_1.4          colorspace_2.1-0      
[37] ggplot2_3.5.1          marginaleffects_0.21.0 forcats_1.0.0         
[40] broom_1.0.6            vctrs_0.6.5            R6_2.5.1              
[43] rpart_4.1.23           lifecycle_1.0.4        stringr_1.5.1         
[46] htmlwidgets_1.6.4      MASS_7.3-60.2          foreign_0.8-86        
[49] cluster_2.1.6          callr_3.7.6            pkgconfig_2.0.3       
[52] pillar_1.9.0           gtable_0.3.5           Rcpp_1.0.13           
[55] data.table_1.15.4      glue_1.7.0             xfun_0.46             
[58] tibble_3.2.1           tidyselect_1.2.1       rstudioapi_0.16.0     
[61] knitr_1.48             igraph_2.0.3           htmltools_0.5.8.1     
[64] tables_0.9.28          rmarkdown_2.27         pscl_1.5.9            
[67] compiler_4.4.0
```

## Data sources  

- Czechia, 2023 presidential election, data: [Czech presidential panel 2023](https://doi.org/10.14473/CSDA/FH4JRK)  
- Czechia, 1996 parliamentary election, data: [Pre-election study](https://doi.org/10.4232/1.3631) & [Post-election study](https://doi.org/10.4232/1.3633)  
- Germany, 2017 parliamentary election, data: [GLES Panel](https://doi.org/10.4232/1.14114)  
- Romania, 2009 presidential election, data: [Romanian election studies 2009](https://doi.org/10.7910/DVN/0L8N8C)  
- Romania, 2012 parlimentary election, data: [Romanian election studies 2012](https://doi.org/10.7910/DVN/3F7D4J)  
- Poland, 2019 EP election, data: [RECONNECT 2019 European Parliament Election Panel Survey](https://doi.org/10.11587/MOV0EZ)  
- Hungary, 2019 EP election, data: [RECONNECT 2019 European Parliament Election Panel Survey](https://doi.org/10.11587/MOV0EZ)  

- [ESS](https://www.europeansocialsurvey.org/) for one chart on difference in SWD between Western and Eastern Europe