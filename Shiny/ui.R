header<-dashboardHeader(title = "PCA",
                        tags$li(actionLink("openModal", label = "", 
                                           icon = icon("address-card")),
                                class = "dropdown"))

sidebar<- dashboardSidebar(
    sidebarMenu(
                menuItem(text = "Dati",icon = shiny::icon("file-text"),
                  menuItem("Esempi",tabName = "esempi"),
                  menuItem("Importa dati",tabName = "importa"),
                  menuItem("Variabili",tabName = "variabili"),
                  menuItem("Oggetti",tabName = "oggetti"),
                  menuItem("Vedi dati",tabName = "vedi"),
                  actionButton("reset","Reset",style='padding:4px; font-size:80%')
                  ),
                menuItem(text = "PCA",icon = shiny::icon("expand-arrows-alt"),
                  menuItem("Model computation",tabName = "modello"),
                  menuItem("Missing data recostruction",tabName = "dati_mancanti"),
                  menuItem("Randomization Test",tabName = "rnd_test"),
                  hr(),
                  menuItem("Plots",tabName = "grafici",
                           menuItem('Score plot',tabName = 'scores'),
                           menuItem('Loading plot',tabName = 'loadings'),
                           menuItem('Biplot',tabName = 'biplot'),
                           menuItem('Correlation plot',tabName = 'correlation'),
                           menuItem('Var. of each variable explained',tabName = 'var_variable')),
                  menuItem('Diagnostics',
                           menuItem(HTML('T <sup>2</sup> and/vs Q'),tabName = 't2andq'),
                           # menuItem('T^2 vs. Q (Influence Plot)',tabName='t2vsq'),
                           menuItem(HTML('T <sup>2</sup> Contribution Plot'),tabName = 't2contr'),
                           menuItem('Q Contribution Plot',tabName = 'qcontr')),
                  menuItem('External data set',
                           menuItem('Projection on the training set',tabName='ext_prj'),
                           menuItem(HTML('T <sup>2</sup> vs. Q (Influence Plot)'),tabName = 'ext_t2andq'),
                           menuItem(HTML('T <sup>2</sup> Contribution Plot'),tabName = 'ext_t2contr'),
                           menuItem('Q Contribution Plot',tabName = 'ext_qcontr'))
                ),
                hr(),
                menuItem(text = "File",icon = icon("briefcase", lib = "font-awesome"),
                         # menuSubItem("Dispensa",tabName = "dispensa"),
                         # menuSubItem("Diapositive",tabName = "diapositive"),
                         tags$header(
                           em(  
                             a(href="https://dispensepca.netlify.app/",  "   Dispense" ,target="_blank",style="white-space: pre-wrap")
                           )
                         ),
                         br(),
                         actionButton("quit", "Quit",onclick = "setTimeout(function(){window.close();},200);",
                                      style='padding:4px; font-size:80%'),
                         HTML('<p><center><font color="cyan"><br> Version 3.0 </font></center>')
                )
                        ))


# Dati --------------------------------------------------------------------

body<-dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),  
  fluidRow(useShinyjs(),
    tabItems(
      tabItem(tabName = "esempi",
              fluidPage(titlePanel("Seleziona dati esempio"),
              column(12,
                     uiOutput('lista_esempi')),
              column(2,
                     radioButtons("esempi_hd", "Display",
                                  choices = c(Head = "head",
                                              All = "all"),
                                  selected = "head")),
              column(8,
                     div(style = 'overflow-x: scroll;',tableOutput("dati_esempio"))))),
      
      tabItem(tabName = "importa",
              fluidPage(titlePanel("Importa dati"),
                tabsetPanel(type = "tabs",
                            tabPanel("Excel",
                                     column(12,
                                       fileInput("file_xlsx", "Scegli file Excel",
                                                   multiple = FALSE,
                                                   accept = c(".xlx",".xlsx"))),
                                     column(12,
                                            checkboxInput("header", "Header", TRUE)),
                                     column(2,
                                           numericInput("foglio_n", label = "Foglio n.",value = 1,min = 1,max = 100)
                                            ),
                                     column(2,
                                            radioButtons("disp_xlx", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")),
                                     
                                     column(8,
                                            div(style = 'overflow-x: scroll;',tableOutput("contents_xlsx")))),
                            tabPanel("CSV", 
                                     column(12,
                                            fileInput("file_csv", "Scegli file CSV",
                                                      multiple = FALSE,accept = c(".csv")
                                            )),
                                     column(12,
                                            checkboxInput("header", "Header", TRUE)),
                                     column(2,
                                            radioButtons("sep", "Separator",
                                                         choices = c(Comma = ",",
                                                                     Semicolon = ";",
                                                                     Tab = "\t"),
                                                         selected = ";"),
                                            radioButtons("quote", "Quote",
                                                         choices = c(None = "",
                                                                     "Double Quote" = '"',
                                                                     "Single Quote" = "'"),
                                                         selected = '"')),
                                     column(2,
                                            radioButtons("disp_csv", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")),
                                     
                                     column(8,
                                            div(style = 'overflow-x: scroll;',tableOutput("contents_csv")))),
                            tabPanel("Incolla",
                                     column(12,
                                            br(),
                                            actionButton("file_incolla", label = "Incolla"),
                                            br(),
                                            br(),
                                            br(),
                                            br()),
                                     column(2,
                                            radioButtons("disp_incolla", "Display",
                                                         choices = c(Head = "head",
                                                                     All = "all"),
                                                         selected = "head")),
                                     column(8,
                                            div(style = 'overflow-x: scroll;',tableOutput("contents_incolla"))))))),
      
      tabItem(tabName = "variabili",
              fluidPage(titlePanel("Variabili"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Variabile nomi righe",
                                             column(12,
                                                uiOutput("var_nomi"),
                                                textOutput("avviso_nomi")),
                                             column(12,"Nomi righe",
                                                verbatimTextOutput("nomi_righe"))),
                                    tabPanel("Variabili supplementari",
                                             column(4,
                                                 uiOutput("var_quali"),
                                                 uiOutput('var_quanti_sup')),
                                             column(8,"Variabili quantitative per la pca",
                                                verbatimTextOutput("var_quanti")))))),
      
      tabItem(tabName = "oggetti",
              fluidPage(titlePanel("Oggetti"),
                        column(4,
                               uiOutput("righe_tolte")),
                        column(8,"Righe cancellate",
                               verbatimTextOutput("righe_restanti"),
                               actionButton("desel_righe","Deseleziona tutte le righe")))), 

      tabItem(tabName = "vedi",
              fluidPage(titlePanel("Dati"),
                        div(style = 'overflow-x: scroll;',DT::dataTableOutput("dati")))),
      
# PCA -------------------------------------------------------------------    

# PCA - modello -----------------------------------------------------------

      tabItem(tabName = "modello",
              fluidPage(titlePanel("Model computation"),
                        column(12,
                               radioButtons("pca_type", " ",
                                            choices = c(PCA = "pca", Varimax= "varmax"), selected = "pca", inline=TRUE)),
                        column(4,
                               checkboxInput("pca_center", label = "Center", value = TRUE)),
                        column(4,
                               checkboxInput("pca_scale", label = "Scale", value = TRUE)),
                        column(4,
                               uiOutput('n_comp'),
                               uiOutput('n_comp_varmax')),
                        column(12,
                               actionButton("bpcamodel", label = "Execute")),
                        column(12,hr()),
                        column(8,
                              
                               plotOutput('scree_plot',height = "600px")),
                        column(4,
                               uiOutput('pca_var_type')),
                        column(12,
                               br()),
                        column(12,
                               verbatimTextOutput('model_out'),
                               downloadButton("pca_expvar_dwl")),
                        )),


# PCA - missing data reconstruction ---------------------------------------

tabItem(tabName = "dati_mancanti",
        fluidPage(titlePanel("Missing data reconstruction"),
                  column(4,
                         br(),br(),
                         checkboxInput("pca_missdata_center", label = "Center", value = TRUE)),
                  column(4,
                         br(),br(),
                         checkboxInput("pca_missdata_scale", label = "Scale", value = TRUE)),
                  column(4,
                         # uiOutput('pca_missdata_varsup'),
                         uiOutput('pca_missdata_n_comp_max')),
                  column(12,
                         actionButton("bpcamodel_miss", label = "Execute")),
                  column(12,
                         br()),
                  column(8,
                         plotOutput('pca_miss_pl')),
                  column(4,
                         uiOutput('pca_missdata_n_comp'),
                         uiOutput('bpcamodel_miss_rec')),
                  column(12,
                         br(),
                         verbatimTextOutput('pca_missdata_cmpl'),
                         downloadButton("pca_missdata_dwl"))
                  
                  
        )),

# PCA - randomization test ---------------------------------------

tabItem(tabName = "rnd_test",
        fluidPage(titlePanel("Randomization Test"),
                  column(9,
                         br(),
                         actionButton("brnd_test", label = "Execute")),
                  column(2,
                         selectInput("rnd_test_N", label = "Number of randomization", 
                                     choices = c(1:10000), 
                                     selected = 100)),
                  column(12,
                         br()),
                  column(8,
                         plotOutput('rnd_test_pl',height = "600px"))
        )),
# PCA - plots -------------------------------------------------------------
# PCA - scores plot -------------------------------------------------------
tabItem(tabName = "scores",
        fluidPage(titlePanel("Score Plots"),
                  column(8,radioButtons("pca_radio_score_type", "Plots",
                                        choices = c(Bidimensional = "2d", Tridimensional = "3d"), 
                                        selected = "2d", inline=TRUE)),
                  column(2),
                  column(12),
                  column(8,
                         plotOutput('scores_pl',height = "600px"),
                         br()),
                  column(4,
                         uiOutput('pca_score_compx'),
                         uiOutput('pca_score_compy'),
                         uiOutput('pca_score_compz'),
                         uiOutput('pca_score_label'),
                         uiOutput('pca_score_col'),
                         uiOutput('pca_score_chull'),
                         uiOutput('pca_score_col_fatt'),
                         uiOutput('pca_score_chull_fatt'),
                         uiOutput('pca_score_rnames'),
                         uiOutput('pca_score_ell'),
                         uiOutput('pca_score_line'),
                         uiOutput('scores_pl3d_lv_z'),
                         uiOutput('scores_pl3d_lv_x')),
                  column(12,
                         # verbatimTextOutput('pca_score_out'),
                         downloadButton("pca_score_dwl")),

                  
        )),
# PCA - loading plot ------------------------------------------------------

tabItem(tabName = "loadings",
        fluidPage(titlePanel("Loading Plots"),
                  column(8,radioButtons("pca_radio_load_type", "Plots",choices = c(Scatter = "sca", Line= "line", Bar= "bar"), 
                                        selected = "sca", inline=TRUE)),
                  column(2),
                  column(12),
                  column(8,
                         plotOutput('loading_pl',height = "600px"),
                         br(),
                         downloadButton("pca_loading_dwl")),
                  column(4,
                         uiOutput('pca_load_compx'),
                         uiOutput('pca_load_compy'),
                         uiOutput('pca_load_rnames'),
                         uiOutput('pca_load_arrows'),
                         uiOutput('pca_load_linecomp'),
                         uiOutput('pca_load_compN'),
                         uiOutput('pca_load_cnames'))
                  
        )),

# PCA - biplot ------------------------------------------------------

tabItem(tabName = "biplot",
        fluidPage(titlePanel("Biplot"),
                  column(12),
                  column(8,
                         plotOutput('biplot',height = "600px")),
                  column(4,
                         uiOutput('pca_biplot_compx'),
                         uiOutput('pca_biplot_compy'),
                         uiOutput('pca_biplot_rnames'),
                         uiOutput('pca_biplot_cnames'),
                         uiOutput('pca_biplot_arrows'))
        )),

# PCA - correlation plot ------------------------------------------------------

tabItem(tabName = "correlation",
        fluidPage(titlePanel("Correlation plot"),
                  column(12),
                  column(8,
                         plotOutput('corr_pl',height = "600px"),
                         br(),
                         downloadButton("pca_corr_dwl")),
                  column(4,
                         uiOutput('pca_corr_compx'),
                         uiOutput('pca_corr_compy'),
                         uiOutput('pca_corr_varsup'),
                         uiOutput('pca_corr_rnames'),
                         uiOutput('pca_corr_arrows'))
        )),

# PCA - explained variance variable ------------------------------------------------------

tabItem(tabName = "var_variable",
        fluidPage(titlePanel("Variance of each variable"),
                  column(12),
                  column(8,
                         # verbatimTextOutput('var_var_pl'),
                         plotOutput('var_var_pl',height = "600px"),
                         br(),
                         downloadButton("pca_var_var_dwl")),
                  column(4,
                         uiOutput('pca_var_var_compN'))
        )),


# PCA - diagnostic --------------------------------------------------------
# PCA - T^2 and/vs Q ---------------------------------------------------------

tabItem(tabName = "t2andq",
        fluidPage(titlePanel(uiOutput("pca_dia_t2andq_title")),
                  column(8,
                         radioButtons("pca_dia_t2andq_type", "Plots",
                                      choices = c("T^2 and Q" = "t2andq", "T^2 vs Q" = "t2vsq"),
                                      selected = "t2andq", inline=TRUE)),
                  column(4,
                         uiOutput('pca_dia_t2andq_joint')),
                  column(12
                         # ,actionButton("bpca_dia_t2andq", label = "Execute")
                         ),
                  column(8,
                         # verbatimTextOutput('var_var_pl'),
                         plotOutput('pca_dia_t2andq_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_t2andq_dwl")),
                  column(4,
                         uiOutput('pca_dia_t2andq_compN'),
                         uiOutput("pca_dia_t2andq_rn"))
        )),






# PCA - T^2 contribution ---------------------------------------------------------

tabItem(tabName = "t2contr",
        fluidPage(titlePanel(HTML('T <sup>2</sup> Contribution Plot')),

                  column(12),
                  column(8,
                         plotOutput('pca_dia_t2contr_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_t2contr_dwl")
                         ),
                  column(4,
                         uiOutput('pca_dia_t2contr_nr'),
                         uiOutput("pca_dia_t2contr_compN"),
                         checkboxInput("pca_dia_t2contr_norm", label = "Normalized", value = TRUE)
                         )
        )),

# PCA - Q contribution ---------------------------------------------------------

tabItem(tabName = "qcontr",
        fluidPage(titlePanel('Q Contribution Plot'),
                  
                  column(12),
                  column(8,
                         plotOutput('pca_dia_qcontr_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_qcontr_dwl")
                  ),
                  column(4,
                         uiOutput('pca_dia_qcontr_nr'),
                         uiOutput("pca_dia_qcontr_compN"),
                         checkboxInput("pca_dia_qcontr_norm", label = "Normalized", value = TRUE)
                  )
        )),



# PCA - ext data ----------------------------------------------------------


# PCA - ext data projection -----------------------------------------------

tabItem(tabName = "ext_prj",
        fluidPage(titlePanel("Projection on the training set"),
                  column(6,
                         radioButtons("pca_ext_data_load", "Load the training set",
                                      choices = c(Paste = "paste",
                                                  Excel = "excel"),
                                      selected = "paste",inline=TRUE)),
                  column(6,
                         uiOutput('pca_ext_data_paste_sp'),
                         uiOutput('pca_ext_data_paste'),
                         uiOutput('pca_ext_data_paste_sp1'),
                         uiOutput('pca_ext_data_excel')),
                  column(12,
                         actionButton("bpcaext", label = "Execute")),
                  column(12,
                         br()),
                  column(8,
                         plotOutput('pca_ext_data_pl',height = "600px"),
                         br(),
                         downloadButton("pca_ext_data_dwl")),
                  column(4,
                         uiOutput('pca_ext_data_varnames'),
                         uiOutput('pca_ext_data_varsup'),
                         uiOutput('pca_ext_data_rwnames'),
                         uiOutput('pca_ext_data_compx'),
                         uiOutput('pca_ext_data_compy'),
                         uiOutput('pca_ext_data_rnames'),
                         uiOutput('pca_ext_data_rnames_tr'),
                         uiOutput('pca_ext_data_ell'))
        )),

# PCA ext - diagnostic --------------------------------------------------------
# PCA ext - T^2 vs Q ---------------------------------------------------------

tabItem(tabName = "ext_t2andq",
        fluidPage(titlePanel(HTML("T <sup>2</sup> vs Q (influence plot)")),
                  column(8),
                  column(4,
                         checkboxInput("pca_dia_ext_t2andq_joint", label = "Joint diagnostics", value = FALSE)),
                  column(12),
                  column(8,
                         plotOutput('pca_dia_ext_t2andq_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_ext_t2andq_dwl")),
                  column(4,
                         uiOutput('pca_ext2q_compN'),
                         uiOutput('pca_ext_data_t2q_lab'),
                         checkboxInput("pca_dia_ext_t2andq_rnumbers", label = "Row Numbers", value = FALSE))
        )),


# PCA ext- T^2 contribution ---------------------------------------------------------

tabItem(tabName = "ext_t2contr",
        fluidPage(titlePanel(HTML('T <sup>2</sup> Contribution Plot')),
                  
                  column(12),
                  column(8,
                         plotOutput('pca_dia_ext_t2contr_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_ext_t2contr_dwl")
                  ),
                  column(4,
                         uiOutput('pca_dia_ext_t2contr_nr'),
                         uiOutput("pca_dia_ext_t2contr_compN"),
                         checkboxInput("pca_dia_ext_t2contr_norm", label = "Normalized", value = TRUE)
                  )
        )),

# PCA ext- Q contribution ---------------------------------------------------------

tabItem(tabName = "ext_qcontr",
        fluidPage(titlePanel(HTML('Q Contribution Plot')),
                  
                  column(12),
                  column(8,
                         plotOutput('pca_dia_ext_qcontr_pl',height = "600px"),
                         br(),
                         downloadButton("pca_dia_ext_qcontr_dwl")
                  ),
                  column(4,
                         uiOutput('pca_dia_ext_qcontr_nr'),
                         uiOutput("pca_dia_ext_qcontr_compN"),
                         checkboxInput("pca_dia_ext_qcontr_norm", label = "Normalized", value = TRUE)
                  )
        ))




      
   
     
)))





ui <- dashboardPage(skin = 'purple',header, sidebar, body,
                    tags$head(HTML("<title>PCA</title>"),tags$link(rel = "stylesheet", type = "text/css", href = "tema.css"),
                              tags$link(rel = "shortcut icon", href = "pca.ico")))














