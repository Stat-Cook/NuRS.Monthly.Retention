# NuRS.Monthly.Retention

This repository contains the code and utility functions used for the analysis of data as related to the NuRS (Nurse Retention and Safety) research project.  To install the current master branch from github use:

``` {r}
devtools::install_github("Stat-Cook/NuRS.Monthly.Retention")
```


## Set Up

To utilize this analysis there are a few requirements:

1. The code is intended to be evaluated on the NuRS VM - at present calls to the SQL database server are only enabled from the NuRS VM.
2. When initialized the `config.yml` file is blank for 'Server' and 'Databse' - please fill these in manually.
3. [QA step] If looking to utilize all functions of the package run `devtools::load_all()` from the repository root or supplying a path to the `NuRS.Monthly.Retention` package.
  