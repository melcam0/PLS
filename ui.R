header<-dashboardHeader(title = "PLS",
                        tags$li(actionLink("openModal", label = "", 
                                           icon = icon("address-card")),
                                class = "dropdown"))

sidebar<- dashboardSidebar(
    sidebarMenu(
                menuItem(text = "Data",icon = shiny::icon("file-alt"),
                  menuItem("Examples",tabName = "esempi"),
                  menuItem("Load data",tabName = "importa"),
                  menuItem("Variables",tabName = "variabili"),
                  menuItem("Objects",tabName = "oggetti"),
                  menuItem("Show data",tabName = "vedi"),
                  menuItem("Row profile",tabName = "profile"),
                  actionButton("reset","Delete",style='padding:4px; font-size:80%')
                  ),
                menuItem(text = "PCR",icon = shiny::icon("expand-arrows-alt"),
                         menuItem("Model computation",tabName = "pcr_CV"),
                         menuItem("Permutation test",tabName = "pcr_permutation")),
                
                menuItem(text = "PLS1",icon = shiny::icon("expand-arrows-alt"),
                         menuItem("Model computation",tabName = "pls_CV"),
                         menuItem("Permutation test",tabName = "pls_permutation")),
                hr(),
                menuItem("Experimentals vs Calculated",tabName = "exp_calc"),
                menuItem("Residuals",tabName = "res"),
                menuItem("Scores",tabName = "scores"),
                menuItem("Loadings",tabName = "load"),
                menuItem("Biplot",tabName = "biplot"),
                menuItem("Coefficients",tabName = "coeff"),
                menuItem("Prediction",tabName = "pred"),
                hr(),
                menuItem(text = "File",icon = icon("briefcase", lib = "font-awesome"),
                         tags$header(
                           # em(  
                           #   a(href="https://dispensepca.netlify.app/",  "   Dispense" ,target="_blank",style="white-space: pre-wrap")
                           # )
                         ),
                         br(),
                         actionButton("quit", "Quit",onclick = "setTimeout(function(){window.close();},200);",
                                      style='padding:4px; font-size:80%'),
                         HTML('<p><center><font color="cyan"><br> Version 1.4 </font></center>')
                )
                        ))


# Dati --------------------------------------------------------------------

body<-dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),  
  fluidRow(useShinyjs(),
    tabItems(
      tabItem(tabName = "esempi",
              fluidPage(titlePanel("Select example data"),
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
              fluidPage(titlePanel("Load data"),
                tabsetPanel(type = "tabs",
                            tabPanel("Excel",
                                     column(12,
                                       fileInput("file_xlsx", "Select file Excel",
                                                   multiple = FALSE,
                                                   accept = c(".xlx",".xlsx"))),
                                     column(12,
                                            checkboxInput("header", "Header", TRUE)),
                                     column(2,
                                           numericInput("foglio_n", label = "Sheet n.",value = 1,min = 1,max = 100)
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
                                            fileInput("file_csv", "Select file CSV",
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
                            tabPanel("Paste",
                                     column(12,
                                            br(),
                                            actionButton("file_incolla", label = "Paste"),
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
              fluidPage(titlePanel("Variables"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Row names variable",
                                             column(12,
                                                uiOutput("var_nomi"),
                                                textOutput("avviso_nomi")),
                                             column(12,"Nomi righe",
                                                verbatimTextOutput("nomi_righe"))),
                                    tabPanel("Supplementary variables",
                                             column(4,
                                                 uiOutput("var_quali"),
                                                 uiOutput('var_quanti_sup')),
                                             column(8,"Quantitative variables for pls",
                                                verbatimTextOutput("var_quanti"))),
                                    tabPanel("Response variable",
                                             column(12,
                                                    uiOutput("var_y")))
                                    ))),

      tabItem(tabName = "oggetti",
              fluidPage(titlePanel("Objects"),
                        column(4,
                               uiOutput("righe_tolte")),
                        column(8,"Rows deleted",
                               verbatimTextOutput("righe_restanti"),
                               actionButton("desel_righe","Deselect all rows")))), 

      tabItem(tabName = "vedi",
              fluidPage(titlePanel("Dati"),
                        div(style = 'overflow-x: scroll;',DT::dataTableOutput("dati")))),

      tabItem(tabName = "profile",
              fluidPage(titlePanel("Row profile"),
                        column(10,
                               hr(),
                               plotOutput('profile_plot',height = "600px")
                               ),
                        column(2,
                               hr(),
                               checkboxInput("profile_chk", label = "Header on the x axis?", value = TRUE),
                               hr(),
                               uiOutput('profile_col'),
                               pickerInput("profile_transf", label = "Transformations",
                                           choices = list("Autoscaling (SNV)", "Binning", "First Derivative",
                                                          "Second Derivative", "Savitzky-Golay"),
                                           options =  list(
                                             "max-options" = 1,
                                             "max-options-text" = "No more!"
                                           ),
                                           multiple = TRUE),
                               uiOutput('profile_bin_1'),
                               uiOutput('profile_bin_2'),
                               uiOutput('profile_bin_3'),
                               uiOutput('profile_sg_1'),
                               uiOutput('profile_sg_2'),
                               uiOutput('profile_sg_3'),
                               uiOutput('profile_sg_4'),
                               uiOutput('profile_sg_5'),
                               actionButton("profile_reset","Reset",style='padding:4px; font-size:80%')
                               ),
                        column(12,
                               h5('Subsequent transformations'),
                               verbatimTextOutput('profile_success_trasf'),
                               downloadButton("ds_tr_download"))
                        )), 
      
# PCR/PLS -------------------------------------------------------------------    

# PCR - costruzione modello -----------------------------------------------------------

tabItem(tabName = "pcr_CV",
        fluidPage(titlePanel("Model computation"),
                  column(12,
                         radioButtons("pcr_cv_choise", label ="",inline=TRUE,
                                      choices = list("single CV" = 1, "repeated CV" = 2),
                                      selected = 1)),
                  column(4,
                         checkboxInput("pcr_scale", label = "Scale (centering by default)", value = FALSE)),
                  column(4,
                         uiOutput('pcr_n_comp')),
                  column(4,
                         uiOutput('pcr_n_cv')),
                  column(12,
                         actionButton("bpcrmodel", label = "Execute")),
                  column(12,
                         hr()),
                  column(10,
                         plotOutput('pcr_cv_plot',height = "600px"),
                         br(),
                         verbatimTextOutput('model_pcr_out_1'),
                         verbatimTextOutput('model_pcr_out_2')),
                  column(2,
                         uiOutput('pcr_n_rnd'),
                         br(),
                         uiOutput('pcr_n_comp_df'),
                         verbatimTextOutput('pcrmodel_out_df')
                         )
        )
),

# PCR - permutation -------------------------------------------------------

tabItem(tabName = "pcr_permutation",
        fluidPage(titlePanel("Permutation test"),
                  column(12,
                         numericInput("pcr_n_prm", label = "Number of permutations", value = 1000),
                         actionButton("bpcr_perm", label = "Execute")),
                  column(12,
                         br()),
                  column(10,
                         # verbatimTextOutput('a'),
                         plotOutput('pcr_perm_plot',height = "600px")),
                  column(2,
                         verbatimTextOutput('pcr_perm_txt'))

        )
),

# PLS - costruzione modello -----------------------------------------------------------

      tabItem(tabName = "pls_CV",
              fluidPage(titlePanel("Model computation"),
                        column(12,
                               radioButtons("pls_cv_choise", label ="",inline=TRUE,
                                            choices = list("single CV" = 1, "repeated CV" = 2), 
                                            selected = 1)),
                        column(4,
                               checkboxInput("pls_scale", label = "Scale (centering by default)", value = FALSE)),
                        column(4,
                               uiOutput('pls_n_comp')),
                        column(4,
                               uiOutput('pls_n_cv')),
                        column(12,
                               actionButton("bplsmodel", label = "Execute")),
                        column(12,
                               hr()),
                        column(10,
                               plotOutput('pls_cv_plot',height = "600px"),
                               br(),
                               verbatimTextOutput('model_out_1'),
                               verbatimTextOutput('model_out_2')),
                        column(2,
                               uiOutput('pls_n_rnd'),
                               br(),
                               uiOutput('pls_n_comp_df'),
                               verbatimTextOutput('plsmodel_out_df'))
                        )
              ),

# PLS - permutation -------------------------------------------------------

tabItem(tabName = "pls_permutation",
        fluidPage(titlePanel("Permutation test"),
                  column(12,
                         numericInput("pls_n_prm", label = "Number of permutations", value = 1000),
                         actionButton("bpls_perm", label = "Execute")),
                 column(12,
                        br()),
                  column(10,
                         # verbatimTextOutput('a'),
                         plotOutput('pls_perm_plot',height = "600px")),
                  column(2,
                         verbatimTextOutput('pls_perm_txt'))
                  
        )
),

# PLS - Exp vs calc -------------------------------------------------------------

tabItem(tabName = "exp_calc",
        fluidPage(titlePanel("Experimentals vs Calculated"),
                  column(10,
                         hr(),
                         plotOutput('pls_expvsfitted',height = "600px"),
                         br(),
                         downloadButton("pls_fitting_dwl")),
                  column(2,
                         hr(),
                         uiOutput('pls_expvsfitted_label'),
                         uiOutput('pls_expvsfitted_col'),
                         uiOutput('pls_expvsfitted_rnames'))
        )
),

# PLS - residui -----------------------------------------------------------

tabItem(tabName = "res",
        fluidPage(titlePanel("Residuals"),
                  column(10,
                         hr(),
                         plotOutput('pls_res_plot',height = "600px"),
                         br(),
                         downloadButton("pls_res_dwl")),
                  column(2,
                         hr(),
                         uiOutput('pls_res_label'),
                         uiOutput('pls_res_col'),
                         uiOutput('pls_res_rnames'))
        )),

# PLS - scores plot -------------------------------------------------------
tabItem(tabName = "scores",
        fluidPage(titlePanel("Score Plots"),
                  column(12,
                         radioButtons("pls_res_scale", label ="",inline=TRUE,
                                      choices = list("Same scale" = 1, "Different scale" = 2), 
                                      selected = 1)),
                  # column(2),
                  column(8,
                         hr(),
                         plotOutput('pls_scores_plot',height = "600px",width = "600px"),
                         br(),
                         downloadButton("pls_score_dwl")),
                  # column(2),
                  column(4,
                         hr(),
                         uiOutput('pls_score_compx'),
                         uiOutput('pls_score_compy'),
                         uiOutput('pls_score_label'),
                         uiOutput('pls_score_col'),
                         uiOutput('pls_score_rnames'))

        )),

# PLS - loading plot ------------------------------------------------------

tabItem(tabName = "load",
        fluidPage(titlePanel("Loading Plots"),
                  column(8,radioButtons("pls_radio_load_type", "Plots",choices = c(Scatter = "sca", Line= "line"), 
                                        selected = "sca", inline=TRUE)),
                  column(2),
                  column(12),
                  # column(2),
                  column(8,
                         plotOutput('loading_pl',height = "600px",width = "600px"),
                         br(),
                         downloadButton("pls_loading_dwl")),
                  # column(2),
                  column(4,
                         uiOutput('pls_load_compx'),
                         uiOutput('pls_load_compy'),
                         uiOutput('pls_load_rnames'),
                         uiOutput('pls_load_arrows'),
                         uiOutput('pls_load_chk'),
                         uiOutput('pls_load_linecomp'),
                         uiOutput('pls_load_compN'),
                         uiOutput('pls_load_cnames'))
        )),

# PLS - biplot ------------------------------------------------------

tabItem(tabName = "biplot",
        fluidPage(titlePanel("Biplot"),
                  # column(2),
                  column(8,
                         hr(),
                         plotOutput('biplot',height = "600px",width = "600px")),
                  # column(2),
                  column(4,
                         hr(),
                         uiOutput('pls_biplot_compx'),
                         uiOutput('pls_biplot_compy'),
                         uiOutput('pls_biplot_arrows'))
        )),

# PLS - coefficients ------------------------------------------------------

tabItem(tabName = "coeff",
        fluidPage(titlePanel("Coefficients"),
                  column(8,
                         hr(),
                         plotOutput('coeff_pl',height = "600px",width = "600px"),
                         br(),
                         downloadButton("pls_coefficients_dwl")),
                  column(4,
                         hr(),
                         checkboxInput("pls_coeff_chk", label = "Header on the x axis?", value = TRUE),
                         hr(),
                         uiOutput('pls_coeff_comp'))
        )),


# PLS - prediction --------------------------------------------------------

tabItem(tabName = "pred",
        fluidPage(titlePanel("Prediction"),
                  hr(),
                  column(4,
                         uiOutput('lista_esempi_ext')),
                  column(2,
                         radioButtons("pls_pred_data_load", "or",
                                      choices = c(Paste = "paste",
                                                  Excel = "excel"),
                                      selected = "paste",inline=TRUE)),
                  column(1),
                  column(2,
                         uiOutput('pls_pred_data_paste_sp'),
                         uiOutput('pls_pred_data_paste'),
                         uiOutput('pls_pred_data_paste_sp1'),
                         uiOutput('pls_pred_data_excel')),
                  
                  column(12),
                  column(3,
                         verbatimTextOutput('pls_load_ds')),
                  column(1),
                  column(2,
                         checkboxInput("pls_pred_y_chk", "Data set with response variable", FALSE)),
                  column(1),
                  column(5,
                         uiOutput('pls_pred_data_var_y')),
                  column(12,
                         br(),
                         br()),
                  column(12,
                         actionButton("bplsext", label = "Execute")),
                  column(12,
                         br()),
                  column(10,
                         verbatimTextOutput('pls_pred_print')),
                  column(10,
                         plotOutput('pls_pred_data_pl',height = "600px"),
                         br(),
                         downloadButton("pls_pred_data_dwl")),
                  column(2,
                         uiOutput('pls_ext_data_rnames')),
        ))

   
     
)))





ui <- dashboardPage(skin = 'purple',header, sidebar, body,
                    tags$head(HTML("<title>PLS</title>"),tags$link(rel = "stylesheet", type = "text/css", href = "tema.css"),
                              tags$link(rel = "shortcut icon", href = "pca.ico")))














