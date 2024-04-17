pacchetti<-c('shiny',
             'shinydashboard',
             'shinythemes',
             'shinyjs',
             'shinyWidgets',
             'ggplot2',
             'readxl',
             'openxlsx',
             'BiocManager',
             # 'lattice',
             # 'latticeExtra',
             # 'stringr',
             # 'chemometrics',
             'pls',
             'gplots',
             'DT',
             'stringr')

install.packages(pkgs = pacchetti)

BiocManager::install("pcaMethods")






