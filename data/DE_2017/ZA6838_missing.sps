* Encoding: UTF-8.
****************************************************************************************************
********** Defining Missing Values in GLES Datasets
****************************************************************************************************

*    Since 2013 all GLES studies use a consistent missing value schema which can be
*    downloaded from our homepage (www.gesis.org/gles).
*
*    The following syntax selects all numeric variables (apart from date variables) in the dataset
*    and defines their negative values as missing values.
*    If you have any questions or problems do not hestitate to contact us (gles@gesis.org).

****************************************************************************************************

* Set working directory (where dataset is saved).
CD 'INSERT/FILEPATH'. /* e.g. 'C:/Users/user/data'.

* Open dataset.
GET FILE 'ZAXXXX_vX-X-X.sav'.  /* e.g. 'ZA6838_v1-0-0_w10_sA.sav'.

* Select all numeric variables in dataset.
SPSSINC SELECT VARIABLES MACRONAME="!n" 
    /PROPERTIES TYPE=NUMERIC LEVEL=NOMINAL ORDINAL SCALE UNKNOWN ROLE=ANY
    /OPTIONS ORDER=ALPHA REVERSE=NO  PRINT=YES IFNONE=ERROR SEPARATOR=" ".

* Define missing values for all numeric variables.
MISSING VALUES !n (lo thru -1).


* Save dataset.
SAVE OUTFILE =  'ZAXXXX_vX-X-X.sav'.