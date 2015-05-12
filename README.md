FAOSTAT Trade Matrix Explorer
=====================================

A simple shiny application for visualising FAOSTAT trade matrices. Work in progress.

Application is a standalone and is shipped with required data files.

Running the app locally
-------------------------------------

### Setting up the environment

You need to have up-to-date version of [R](http://www.r-project.org/) installed in any operating system. [RStudio IDE](http://www.rstudio.com/products/rstudio/download/) makes things easier, but is not required.

Then you need few dependencies tha can be installed running the script below in R: 

    PACKAGES <- c("dplyr","tidyr","ggplot2","scales","grid","maptools",
              "sp","rgeos","plyr","geosphere","extrafont","stringr",
              "rgdal","grid","gridExtra","shiny","shinyBS")
    #  Install packages
    inst <- match(PACKAGES, .packages(all=TRUE))
    need <- which(is.na(inst))
    if (length(need) > 0) install.packages(PACKAGES[need])
    # Load packages
    lapply(PACKAGES, require, character.only=T)

### Downloading the app

If you have git installed you can do
    
    git clone git@github.com:UNFAOstatistics/trade_matrix.git

And then change (working)directory to `trade_matrix` folder and run `runApp()`

If you don't have git, you can just [download the zipfile](https://github.com/UNFAOstatistics/trade_matrix/archive/master.zip), unzip and change (working)directory where the files are and run `runApp()`.


License: GPL (>= 2) + file LICENSE