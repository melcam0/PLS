pacchetti<-c('shiny',
             'shinydashboard',
             'shinythemes',
             'shinyjs',
             'shinyWidgets',
             'ggplot2',
             'readxl',
             'openxlsx',
             'BiocManager',
             'lattice',
             'latticeExtra',
             'stringr',
             'chemometrics',
             'DT')

install.packages(pkgs = pacchetti)

BiocManager::install("pcaMethods")






