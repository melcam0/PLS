rm(list = ls())

colorpanel <- function (n, low, mid, high) {
  if (missing(mid) || missing(high)) {
    low <- col2rgb(low)
    if (missing(high)) 
      high <- col2rgb(mid)
    else high <- col2rgb(high)
    red <- seq(low[1, 1], high[1, 1], length = n)/255
    green <- seq(low[3, 1], high[3, 1], length = n)/255
    blue <- seq(low[2, 1], high[2, 1], length = n)/255
  }
  else {
    isodd <- odd(n)
    if (isodd) {
      n <- n + 1
    }
    low <- col2rgb(low)
    mid <- col2rgb(mid)
    high <- col2rgb(high)
    lower <- floor(n/2)
    upper <- n - lower
    red <- c(seq(low[1, 1], mid[1, 1], length = lower), seq(mid[1, 
                                                                1], high[1, 1], length = upper))/255
    green <- c(seq(low[3, 1], mid[3, 1], length = lower), 
               seq(mid[3, 1], high[3, 1], length = upper))/255
    blue <- c(seq(low[2, 1], mid[2, 1], length = lower), 
              seq(mid[2, 1], high[2, 1], length = upper))/255
    if (isodd) {
      red <- red[-(lower + 1)]
      green <- green[-(lower + 1)]
      blue <- blue[-(lower + 1)]
    }
  }
  rgb(red, blue, green)
}

dovc<-function(d){
  require(gplots)
  require(grDevices)
  require(graphics)
  if(is.character(d)){
    C<-factor(d)
    nl<-levels(C)
    n<-nlevels(C)
    if(n<=19){
      vcb<-c(	"black",
              "red",
              "green",
              "blue",
              "brown",
              "cyan",
              "orange",
              "pink",
              "yellow",
              "turquoise",
              "purple",
              "navy",
              "magenta",
              "khaki",
              "grey",
              "gold",
              "coral",
              "beige",
              "azure")
      Col<-vcb[1:n]
    }else{
      Col<-rainbow(n)
    }
    C<-as.character(C)
    for(i in 1:n){
      C[C==as.character(nl[i])]<-Col[i]
    }
  }else{
    d<-as.numeric(d) 
    C<-colorpanel(256,low="blue",high="red")[findInterval(d,seq(min(d),max(d),length.out=256))]
  }
  return(list(C))
}

convCppV<-function(u,v){
  m=length(u);n=length(v)
  p=m-n+1
  w<-rep(0,p)
  for(k in 1:p){
    w[k]<-u[k]*v[1]
    for(j in 2:n){
      w[k]<-w[k]+u[k+j-1]*v[j]
    }
  }
  return(w)
}

convCppM<-function(M,v){
  n=nrow(M);m<-ncol(M);f<-length(v)
  p=m-f+1
  X<-matrix(rep(0,n*p),n,p)
  for(i in 1:n){
    u<-as.numeric(M[i,])
    X[i,]<-convCppV(u,v)
  }
  return(X)
}

savitzkyGolay <- function(X, m, p, w, delta.wav) {
  if (is.data.frame(X)) 
    X <- as.matrix(X)
  if (w%%2 != 1){
    showNotification("needs an odd filter length w",type='error')
  }else{
    if (p >= w){
      showNotification("filter length w should be greater than polynomial order p",type='error')
    }else{
      if (p < m){
        showNotification("polynomial order p should be geater or equal to differentiation order m",type='error')
      }else{
        gap <- (w - 1)/2
        basis <- outer(-gap:gap, 0:p, "^")
        A <- solve(crossprod(basis, basis), tol = 0) %*% t(basis)
        if (is.matrix(X)) {
          if (w >= ncol(X)){
            showNotification("filter length w should be lower than ncol(X)",type='error')
          }else{
            output <- factorial(m) * convCppM(X, A[m + 1, ])
            g <- (w - 1)/2
            colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
            rownames(output) <- rownames(X)
          } 
        }
        if (is.vector(X)) {
          if (w >= length(X)){
            showNotification("filter length w should be lower than length(X)",type='error')
          }else{
            output <- factorial(m) * convCppV(X, A[m + 1, ])
            g <- (w - 1)/2
            names(output) <- names(X)[(g + 1):(length(X) - g)]
          }
        }
        # scaling
        if (!missing(delta.wav)) 
          output <- output/delta.wav^m
        return(output)
      }
    }
  }
} 


server <- function (input , output, session ){
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Authors:",size = 's',easyClose = TRUE,footer = NULL,
                  
                  tags$img(src = base64enc::dataURI(file = "GC.jpg", mime = "image/jpg")),
                  
                  
                  HTML((paste(" "," ","Giorgio Marrubini","email: giorgio.marrubini@unipv.it"," ",
                              'Camillo Melzi','email: camillomelzi@gmail.com',sep="<br/>"))))
    )
  })
  
  observeEvent(input$quit,{
    stopApp()
  })

  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
 
  observeEvent(input$reset,{
    dati$DS<-NULL
    dati$DS_nr=NULL
    dati$DS_righe=NULL
    dati$DS_tr=NULL
    dati$nr=NULL
    dati$var=NULL
    dati$var_nr=NULL
    dati$var_qt=NULL
    dati$var_qt_tr=NULL
    dati$var_qt_sup=NULL
    dati$var_ql=NULL
    dati$righe=NULL
    dati$righe_rest=NULL
    dati$righe_tolte=NULL
    dati$var_gr=NULL
    
    dati_ext$DS=NULL
    dati_ext$DS_nr=NULL
    dati_ext$nr=NULL
    
    trsf$testo=NULL
    plsdf$testo=NULL
    plsdf$testo_r_cv=NULL
    
    PLS$res=NULL
    PLS$resf=NULL
    PLS$ncompo=NULL
    PLS$res_df=NULL
    PLS$resf_df=NULL
    PLS$ncompo_df=NULL
    PLS$typ=NULL
    PLS$dataset=NULL
    PLS$dataset<-NULL
    PLS$nY<-NULL
    PLS$validation<-NULL
    PLS$nseg<-NULL
    PLS$segtype<-NULL
    PLS$scale<-NULL
    PLS$model<-NULL
    PLS$ncomp<-NULL
    PLS$ncomp_df<-NULL
    PLS$rmsep<-NULL
    PLS$rcv<-NULL
    PLS$rmsep_df<-NULL
    PLS$rcv_df<-NULL
    PLS$res<-NULL
    PLS$resf<-NULL
    PLS$R_sq <- NULL
    PLS$D <- NULL
    PLS$D_perm <- NULL

    graf$xlim=NULL
    graf$xylim=NULL
    graf$xvar_gr=NULL
    graf$xgr=NULL

    reset("lista_esempi")
    reset("file_xlsx")
    reset("file_csv")
  })
  

# reactiveValues ----------------------------------------------------------
  
  dati<-reactiveValues(DS=NULL,DS_nr=NULL,DS_righe=NULL,DS_tr=NULL,nr=NULL,var=NULL,var_nr=NULL,
                       var_qt=NULL,var_qt_tr=NULL,var_qt_sup=NULL,var_ql=NULL,righe=NULL,righe_rest=NULL,righe_tolte=NULL,
                       var_gr=NULL)
  
  trsf <- reactiveValues(testo=NULL)
  plsdf <- reactiveValues(testo=NULL,testo_r_cv=NULL)
  
  dati_ext<-reactiveValues(DS=NULL,DS_nr=NULL,nr=NULL)
  
  PLS <- reactiveValues(res=NULL,resf=NULL,ncompo=NULL,res_df=NULL,resf_df=NULL,ncompo_df=NULL,typ=NULL,dataset=NULL,nY=NULL,validation=NULL,nseg=NULL,segtype=NULL,scale=NULL,model=NULL,
                        ncomp=NULL,rmsep=NULL,rcv=NULL,ncomp_df=NULL,rmsep_df=NULL,rcv_df=NULL,
                        R_sq=NULL,D=NULL,D_perm=NULL)

  
  
  
  
  # PCA_miss<-reactiveValues(res=NULL,DS_rec=NULL)
  PLS_ext <- reactiveValues(prm=NULL,prm.tr=NULL,rmsep=NULL,res=NULL,res.tr=NULL)

  graf<-reactiveValues(xlim=NULL,ylim=NULL,var_gr=NULL,gr=NULL)


# carica dati -------------------------------------------------------------

  output$lista_esempi<-renderUI({
    fnames<-list.files(path = 'Dati')
    fext<-tools::file_ext(fnames)
    fnames<-fnames[fext %in% c("xlsx")]
    fnames<-tools::file_path_sans_ext(fnames)
    selectInput('lista_esempi',"",choices = c('',fnames),selected = 1)
  })

  observeEvent(input$lista_esempi,{
    if(input$lista_esempi!=""){
      tryCatch({
      require(readxl)
      path<-paste("Dati/",input$lista_esempi,".xlsx",sep="")
      df=read_excel(path = path,sheet = 1,col_names = TRUE)
      suppressWarnings(colnames(df)[!is.na(as.numeric(colnames(df)))]
                       <-as.numeric(colnames(df)[!is.na(as.numeric(colnames(df)))]))
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$DS_tr <- as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)
      dati$var_qt_tr<-colnames(df)},
      error = function(e) {
        stop(safeError(e))
      }
    )
    } else {
      dati$DS<-NULL
      dati$DS_nr=NULL
      dati$DS_righe=NULL
      dati$DS_tr=NULL
      dati$var=NULL
      dati$var_nr=NULL
      dati$var_qt=NULL
      dati$var_qt_tr=NULL
      dati$var_ql=NULL
      dati$righe=NULL
      dati$righe_rest=NULL
    }
  })
  
 output$dati_esempio <- renderTable({
    req(input$lista_esempi)
    if(input$esempi_hd == "head") {
      return(head(dati$DS))
    }
    else {
      return(dati$DS)
    }
  })
  
  observeEvent(input$file_xlsx,{
    tryCatch({
      require(readxl)
      df=read_excel(path = input$file_xlsx$datapath,sheet = input$foglio_n,col_names = input$header)
      suppressWarnings(colnames(df)[!is.na(as.numeric(colnames(df)))]
                       <-as.numeric(colnames(df)[!is.na(as.numeric(colnames(df)))]))
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$DS_tr <- as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var_nr<-colnames(df)
      dati$var<-colnames(df)
      dati$var_qt<-colnames(df)},
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  output$contents_xlsx <- renderTable({
    req(input$file_xlsx)
    if(input$disp_xlx == "head") {
      return(head(dati$DS))
    }
    else {
      return(dati$DS)
    }
  })
    
  observeEvent(input$file_csv,{
    tryCatch({
      df <- read.csv(input$file_csv$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$DS_tr <- as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)
      dati$var_qt_tr<-colnames(df)},
      error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  output$contents_csv <- renderTable({
    req(input$file_csv)
    if(input$disp_csv == "head") {
      return(head(dati$DS))
    }
    else {
      return(dati$DS)
    }
  })
  
  observeEvent(input$file_incolla,{
      df <- tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE,check.names = FALSE),
                   error = function(e) "Selezionare un dataset!")
      df <- type.convert(df)
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$DS_tr <- as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)
      dati$var_qt_tr<-colnames(df)
    })
  
  output$contents_incolla <- renderTable({
    validate(need(input$file_incolla>0,""))
    req(input$file_incolla)
    req(dati$DS)
    # if(!dati$DS=="Selezionare un dataset!"){
    if(nrow(dati$DS)>0){
      if(input$disp_incolla == "head") {
        return(head(dati$DS))
      }
      else {
        return(dati$DS)
      }
    }
 
  })
  
# dati caricati -----------------------------------------------------------
  
  output$dati<-DT::renderDataTable(rownames=TRUE,extensions = 'ColReorder',
                                   options = list(
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '100px', targets = "_all")),
                                     colReorder = TRUE),
                                   class = 'cell-border stripe',
                                   # filter = 'bottom',
                                   {
    validate(need(nrow(dati$DS)!=0,""))
    #if(length(dati$nr)==0){
      dati$DS
   # } else {
    #  dati$DS_nr[!dati$righe%in%dati$righe_tolte,]}
      })

# variabili qualitative ---------------------------------------------------
  
  output$var_quali<-renderUI({
    selectizeInput(inputId = "var_ql"," ",
                   choices = dati$var,
                   multiple = TRUE,
                   options = list(
                     placeholder = 'Select supplementary variables',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  observeEvent(input$var_ql,ignoreNULL = FALSE,{
    dati$var_ql<-input$var_ql
    dati$var_qt<-dati$var[!dati$var%in%input$var_ql]
  })
  
  output$var_quanti <- renderPrint({
    validate(need(nrow(dati$DS)!=0,""))
    if(!length(dati$var_qt)==0){
      dati$var_qt
    }else{
      "No supplementary variables"
    }
  })
  

# variabile nomi righe ---------------------------------------------------

  output$var_nomi<-renderUI({
    selectizeInput(inputId = "var_nr"," ",
                       choices = dati$var_nr,
                   options = list(
                     placeholder = 'Select column row names',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  observeEvent(input$var_nr,ignoreNULL = FALSE,{
    req(input$var_nr)
    if(length(input$var_nr)!=0){
      if(sum(duplicated(dati$DS[,input$var_nr]))==0){
        dati$col_nr<-input$var_n
        dati$DS<-as.data.frame(dati$DS_nr[,!dati$var_nr%in%input$var_nr])
        if(length(dati$var_nr)==2) names(dati$DS)<-dati$var_nr[input$var_nr!=dati$var_nr]
        dati$DS_righe<-as.data.frame(dati$DS_nr[,!dati$var_nr%in%input$var_nr])
        dati$nr<-dati$DS_nr[,dati$var_nr%in%input$var_nr]
        row.names(dati$DS)<-dati$nr
        dati$var<-colnames(dati$DS)
        dati$var_qt<-colnames(dati$DS)
        dati$righe<-dati$DS_nr[,dati$var_nr%in%input$var_nr]
      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Duplicate row names are not allowed!',
                       type = "error",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    } else {
      dati$DS<-dati$DS_nr
      dati$nr<-NULL
      dati$var<-colnames(dati$DS)
      dati$var_qt<-colnames(dati$DS)
      dati$righe<-row.names(dati$DS)
    }
  })
  
  output$nomi_righe<-renderPrint({
    validate(need(nrow(dati$DS)!=0,""))
    if(length(dati$nr)==0){
      "No column row names"
    } else {
      dati$nr
      }
    })
  
# variabile risposta ------------------------------------------------------

  output$var_y<-renderUI({
    selectizeInput(inputId = "var_y"," ",
                   choices = dati$var_qt,
                   options = list(
                     placeholder = 'Select response variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
# oggetti  ------------------------------------------------
  
  output$righe_tolte<-renderUI({
    checkboxGroupInput(inputId = "righe_tolte",label = "select rows to delete",
                       choices = dati$righe,selected =dati$righe_tolte)
  })
  
  observeEvent(input$righe_tolte,ignoreNULL = FALSE,{
    if(length(input$righe_tolte)!=0){
      dati$righe_tolte<-input$righe_tolte
      dati$righe_rest<-dati$righe[!dati$righe%in%input$righe_tolte] 
      dati$DS<-as.data.frame(dati$DS_righe[dati$righe%in%dati$righe_rest,])
      colnames(dati$DS)<-colnames(dati$DS_righe)
      row.names(dati$DS)<-dati$righe_rest
    } else {
      dati$DS<-dati$DS_righe
      dati$righe_tolte<-NULL
      dati$righe_rest<-dati$righe
    }
  })
  
  output$righe_restanti <- renderPrint({
    if(!length(dati$righe_tolte)==0){
      dati$righe_tolte
    }else{
     "No rows deleted"
    }
  })
  
  observeEvent(input$desel_righe,{
    dati$DS<-dati$DS_righe
    dati$righe_tolte<-NULL
    dati$righe_rest<-dati$righe
  })
  

# profile -----------------------------------------------------------------

  output$profile_col <- renderUI({
    req(!is.null(dati$var_ql))
    pickerInput("profile_col", label = "Color variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$profile_bin_1 <- renderUI({
    req(input$profile_transf=='Binning')
    req(!is.null(dati$var_qt))
    var <- dati$var_qt[!dati$var_qt%in%input$var_y]
    selectizeInput(inputId = "profile_bin_1"," ",
                   choices = var,
                   options = list(
                     placeholder = 'Select first variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$profile_bin_2 <- renderUI({
    req(input$profile_transf=='Binning')
    req(!is.null(dati$var_qt))
    var <- dati$var_qt[!dati$var_qt%in%input$var_y]
    selectizeInput(inputId = "profile_bin_2"," ",
                   choices = var,
                   options = list(
                     placeholder = 'Select last variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$profile_bin_3 <- renderUI({
    req(input$profile_transf=='Binning')
    req(!is.null(dati$DS))
    cl_start<-which(colnames(dati$DS)==input$profile_bin_1)
    cl_end<-which(colnames(dati$DS)==input$profile_bin_2)
    req(!identical(cl_start, integer(0)))
    req(!identical(cl_end, integer(0)))
    n<-cl_end-cl_start+1
    x <- 1:n
    x <- x[n%%x==0]
    x <- x[-length(x)]
    x <- x[-1]
    selectizeInput(inputId = "profile_bin_3"," ",
                   choices = x,
                   options = list(
                     placeholder = 'Bin width (divider of the number of selected columns)',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
    
  })
  
  prof_bin_3 <- reactiveValues(ampl=NULL)
  observeEvent(input$profile_bin_3,{
    prof_bin_3$ampl <- as.numeric(input$profile_bin_3)
  })

  output$profile_sg_1 <- renderUI({
    req(input$profile_transf=='Savitzky-Golay')
    req(!is.null(dati$var_qt))
    var <- dati$var_qt[!dati$var_qt%in%input$var_y]
    selectizeInput(inputId = "profile_sg_1"," ",
                   choices = var,
                   options = list(
                     placeholder = 'Select first variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$profile_sg_2 <- renderUI({
    req(input$profile_transf=='Savitzky-Golay')
    req(!is.null(dati$var_qt))
    var <- dati$var_qt[!dati$var_qt%in%input$var_y]
    selectizeInput(inputId = "profile_sg_2"," ",
                   choices = var,
                   options = list(
                     placeholder = 'Select last variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$profile_sg_3 <- renderUI({
    req(input$profile_transf=='Savitzky-Golay')
    cl_start<-which(colnames(dati$DS)==input$profile_sg_1)
    cl_end<-which(colnames(dati$DS)==input$profile_sg_2)
    req(!identical(cl_start, integer(0)))
    req(!identical(cl_end, integer(0)))
    x <- 1:100
    selectizeInput(inputId = "profile_sg_3"," ",
                   choices = x,
                   options = list(
                     placeholder = 'Derivative',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })

  output$profile_sg_4 <- renderUI({
    req(input$profile_transf=='Savitzky-Golay')
    cl_start<-which(colnames(dati$DS)==input$profile_sg_1)
    cl_end<-which(colnames(dati$DS)==input$profile_sg_2)
    req(!identical(cl_start, integer(0)))
    req(!identical(cl_end, integer(0)))
    req(input$profile_sg_3!="")
    d <- as.numeric(input$profile_sg_3)
    p <- 1:100
    p <- p[p>=d]
    selectizeInput(inputId = "profile_sg_4"," ",
                   choices = p,
                   options = list(
                     placeholder = 'Polynomial',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  output$profile_sg_5 <- renderUI({
    req(input$profile_transf=='Savitzky-Golay')
    cl_start<-which(colnames(dati$DS)==input$profile_sg_1)
    cl_end<-which(colnames(dati$DS)==input$profile_sg_2)
    req(!identical(cl_start, integer(0)))
    req(!identical(cl_end, integer(0)))
    req(input$profile_sg_3!="")
    req(!is.null(input$profile_sg_4))
    req(input$profile_sg_4!="")
    p <- as.numeric(input$profile_sg_4)
    n <- cl_end-cl_start
    w <- seq(1,n,2)
    w <- w[w>p] 
    selectizeInput(inputId = "profile_sg_5"," ",
                   choices = w,
                   options = list(
                     placeholder = 'Window size',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  prof_sg <- reactiveValues(der=NULL,pol=NULL,wind=NULL)
  observeEvent(input$profile_sg_3,{
    prof_sg$der <- as.numeric(input$profile_sg_3)
  })
  observeEvent(input$profile_sg_4,{
    prof_sg$pol <- as.numeric(input$profile_sg_4)
  })
  observeEvent(input$profile_sg_5,{
    prof_sg$wind <- as.numeric(input$profile_sg_5)
  })

  output$profile_plot <- renderPlot({
    req(!is.null(dati$DS))
      M_<-dati$DS[,dati$var_qt]
      if(!is.null(input$var_y))M_ <- M_[,colnames(M_)!=input$var_y]
      at <- 1:length((colnames(M_)))
      tk <- TRUE
      # if(length((colnames(M_))>15))tk <- FALSE
      # if(length((colnames(M_)))>9000000000)at=c(seq(1,length(colnames(M_)),(length(colnames(M_))-1)/10),length(colnames(M_)))
      if(length(colnames(M_))>15){
        n<-length((colnames(M_)))
        m<-floor((n-1)/11)
        at=seq(1,n,m)
        rm(n,m)
      }
      assex <- 1:length(colnames(M_))
      if(input$profile_chk){
        assex <- colnames(M_)
      }
      if(is.numeric(t(M_))){
        if(is.null(input$profile_col)){
          matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",xaxt='n')
          axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
        }else{
          req(!is.null(input$profile_col))
          grade<-dati$DS_tr[,input$profile_col]
          if(!is.null(grade)){
            tog<-typeof(grade)
            if(is.factor(grade))tog<-"factor"
            grade<-factor(grade)
            lev<-levels(grade)
            nl<-nlevels(grade)
            if(tog=="double")vcolor<-unlist(dovc(as.numeric(lev)))
            if(tog=="factor")vcolor<-unlist(dovc(as.character(lev)))
            if(tog=="character")vcolor<-unlist(dovc(as.character(lev)))
            if(tog=="integer")vcolor<-unlist(dovc(as.numeric(lev)))
            if(tog=="character" | tog=="factor"){
              matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",col=vcolor[grade],xaxt='n')
              axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
              legend("top", legend=lev,col=vcolor, cex=0.7,lty=1,ncol=min(length(lev),4),inset=c(0,-0.10),xpd=TRUE,bty = "n")
            }else{
              layout(mat=matrix(c(1,2),nrow=1),widths = c(8,0.9))
              par(mar = c(4, 3, 4, 0))
              matplot(y = t(M_),type = "l",lty=1,xlab="",ylab="",col=vcolor[grade],xaxt='n')
              axis(side=1, at=at,labels=assex[at])
              par(mar = c(2, 2, 4, 2))
              m<-as.numeric(lev)[order(as.numeric(lev),decreasing = FALSE)]
              tl <- input$input$profile_col
              image(y=m,z=t(m), col=vcolor,
                    axes=FALSE, main=tl, cex.main=.8,ylab='')
              axis(4,cex.axis=0.8,mgp=c(0,0.5,0))
            }
          }
        }
      }else{
        #allert: 'The matrix contains alphanumeric data: this operation is not allowed'
      }
  })

  observeEvent(input$profile_transf,{
    req(!is.null(dati$DS))
    if(is.null(dati$var_qt_tr))dati$var_qt_tr <- dati$var_qt
    if(input$profile_transf=='Autoscaling (SNV)'){
      M <- dati$DS[,dati$var_qt]
      M_s <- dati$DS[,!colnames(dati$DS)%in%dati$var_qt]
      if(!is.null(input$var_y)){
        M <- M[,colnames(M)!=input$var_y]
        M_s <- cbind.data.frame(M_s,dati$DS[,input$var_y])
        colnames(M_s)[length(colnames(M_s))] <- input$var_y
      }
      M<-t(M)
      M<-scale(M,center=TRUE,scale=TRUE)
      M<-t(M)
      dati$DS <- cbind.data.frame(M_s,M)
    }

    if(input$profile_transf=='Binning'){
      M <- dati$DS[,dati$var_qt]
      M_s <- dati$DS[,!colnames(dati$DS)%in%dati$var_qt]
      if(!is.null(input$var_y)){
        M <- M[,colnames(M)!=input$var_y]
        M_s <- cbind.data.frame(M_s,dati$DS[,input$var_y])
        colnames(M_s)[length(colnames(M_s))] <- input$var_y
      }



      observeEvent(input$profile_bin_1,{
        cl_start<-which(colnames(M)==input$profile_bin_1)
        req(!identical(cl_start, integer(0)))
        observeEvent(input$profile_bin_2,{
          cl_end<-which(colnames(M)==input$profile_bin_2)
          req(!identical(cl_end, integer(0)))
          observeEvent(input$profile_bin_3,{
            validate(need(!is.null(prof_bin_3$ampl),""))
            req(!is.na(prof_bin_3$ampl))
            req(!is.null(prof_bin_3$ampl))
            ampl <-prof_bin_3$ampl
            n<-cl_end-cl_start+1
            q_<-n/ampl
            X_<-as.data.frame(apply(M[,cl_start:(cl_start+ampl-1)],1,'mean'))
            for (i in 2:q_){
              Y_<-as.data.frame(apply(M[,(cl_start+ampl*(i-1)):(cl_start+ampl*i-1)],1,'mean'))
              X_<-cbind.data.frame(X_,Y_)
            }
            colnames(X_)<-paste0('X',1:q_)
            if(cl_start>1){
              M_1<-M[,1:(cl_start-1),drop=FALSE]
              X_<-cbind.data.frame(M_1,X_)}
            if(cl_end<ncol(M)){
              M_2<-M[,(cl_end+1):ncol(M),drop=FALSE]
              X_<-cbind.data.frame(X_,M_2)}
            dati$DS <- cbind.data.frame(M_s,X_)
            dati$var_qt <- colnames(X_)
          })
        })
      })
    }
    if(input$profile_transf=='First Derivative'){
      M <- dati$DS[,dati$var_qt]
      M_s <- dati$DS[,!colnames(dati$DS)%in%dati$var_qt]
      if(!is.null(input$var_y)){
        M <- M[,colnames(M)!=input$var_y]
        M_s <- cbind.data.frame(M_s,dati$DS[,input$var_y])
        colnames(M_s)[length(colnames(M_s))] <- input$var_y
      }
      M<-t(M)
      M<-diff(M,lag=1,differences=1)
      M<-t(M)
      dati$DS <- cbind.data.frame(M_s,M)
      dati$var_qt <- dati$var_qt[dati$var_qt%in%colnames(dati$DS)]
    }
    if(input$profile_transf=='Second Derivative'){
      M <- dati$DS[,dati$var_qt]
      M_s <- dati$DS[,!colnames(dati$DS)%in%dati$var_qt]
      if(!is.null(input$var_y)){
        M <- M[,colnames(M)!=input$var_y]
        M_s <- cbind.data.frame(M_s,dati$DS[,input$var_y])
        colnames(M_s)[length(colnames(M_s))] <- input$var_y
      }
      M<-t(M)
      M<-diff(M,lag=1,differences=2)
      M<-t(M)
      dati$DS <- cbind.data.frame(M_s,M)
      dati$var_qt <- dati$var_qt[dati$var_qt%in%colnames(dati$DS)]
    }

    if(input$profile_transf=='Savitzky-Golay'){
      M <- dati$DS[,dati$var_qt]
      M_s <- dati$DS[,!colnames(dati$DS)%in%dati$var_qt]
      if(!is.null(input$var_y)){
        M <- M[,colnames(M)!=input$var_y]
        M_s <- cbind.data.frame(M_s,dati$DS[,input$var_y])
        colnames(M_s)[length(colnames(M_s))] <- input$var_y
      }
      
      observeEvent(input$profile_sg_1,{
        cl_start<-which(colnames(M)==input$profile_sg_1)
        req(!identical(cl_start, integer(0)))
        observeEvent(input$profile_sg_2,{
          cl_end<-which(colnames(M)==input$profile_sg_2)
          req(!identical(cl_end, integer(0)))
          M_sg<-M[,cl_start:cl_end,drop=FALSE]
          observeEvent(input$profile_sg_3,{
            req(!is.null(prof_sg$der))
            req(!is.na(prof_sg$der))
            m<-prof_sg$der
            observeEvent(input$profile_sg_4,{
              req(!is.null(prof_sg$pol))
              req(!is.na(prof_sg$pol))
              p<-prof_sg$pol
              
              observeEvent(input$profile_sg_5,{
                req(!is.null(prof_sg$wind))
                req(!is.na(prof_sg$wind))
                w<-prof_sg$wind
                
                Z<-savitzkyGolay(X = M_sg,m = m,p = p,w = w)
                
                
                if(cl_start==1&cl_end==ncol(M)) M<-Z
                if(cl_start==1&cl_end<ncol(M)){
                  nc<-as.numeric(cl_end)+1
                  M<-cbind.data.frame(Z,M[,nc:ncol(M),drop=FALSE])
                }
                if(cl_start>1&cl_end==ncol(M)){
                  nc<-as.numeric(cl_start)-1
                  M<-cbind.data.frame(M[,1:nc,drop=FALSE],Z)
                }
                if(cl_start>1&cl_end<ncol(M)){
                  nc<-as.numeric(cl_start)-1;nc_1<-as.numeric(cl_end)+1
                  M<-cbind.data.frame(M[,1:nc,drop=FALSE],Z,M[,nc_1:ncol(M),drop=FALSE])
                }
                
                dati$DS <- cbind.data.frame(M_s,M)
                dati$var_qt <- colnames(M)
                
              })
            })
          })
        })
      })
    }
    
    trsf$testo <- paste(trsf$testo,'-',input$profile_transf, '-')
    trsf <- input$profile_transf
  })
  
  output$profile_success_trasf <- renderPrint({
    cat(trsf$testo)
  })
  
  observeEvent(input$profile_reset,{
    dati$DS <- dati$DS_tr
    dati$var_qt <- dati$var_qt_tr
    trsf$testo=NULL
    reset('profile_transf')
    prof_bin_3$ampl <- NULL
    
    prof_sg$der <- NULL
    prof_sg$pol <- NULL
    prof_sg$wind <- NULL

  })
  
  output$ds_tr_download <- downloadHandler(
    filename = "data_tr.xlsx", 
    content = function(file) {
      write.xlsx(dati$DS, file,colNames=TRUE)
    })
 

# PCR/PLS ---------------------------------------------------------------------

  # PCR - costruzione modello -------------------------------------------------
  
  output$pcr_n_comp<-renderUI({
    req(dati$var_qt)
    n <- length(dati$var_qt)-length(input$var_y)
    sel <- min(10,n)
    selectInput("pcr_n_comp", label = "Max. number of components", 
                choices = c(2:n), 
                selected = sel)
  })
  
  output$pcr_n_cv<-renderUI({
    req(!is.null(dati$DS))
    selectInput("pcr_n_cv", label = "Number of segments for CV", 
                choices = c(2:nrow(dati$DS)), 
                selected = 5)
  })
  
  output$pcr_n_rnd<-renderUI({
    req(!is.null(dati$DS))
    req(input$pcr_cv_choise=="2")
    numericInput("pcr_n_rnd", label = "Number of randomizations",value = 100,min = 1,max = 10000)
  })
  
  observeEvent(input$bpcrmodel,{
    validate(need(nrow(dati$DS)!=0,""))
    if(is.null(input$var_y)){
      sendSweetAlert(session, title = "Input Error",
                     text = 'Select response variable!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      M_<-dati$DS[,dati$var_qt]
      if(!is.null(input$var_y))M_ <- M_[,colnames(M_)!=input$var_y]
      Y_ <- dati$DS[,input$var_y]
      if((typeof(M_)=='double')|(typeof(M_)=='list')){
        M_<-data.frame(cbind(Y_,data.frame(M_)))
        naM<-names(M_)
        nNA<-sum(is.na(M_))
        nY<-1
        if(nNA>0){
          mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
          showNotification(mess)
          md<-prep(M_,scale="uv",center=TRUE,simple=FALSE,rev=FALSE)
          res<-pca(md$data,method="nipals",nPcs=min(ncol(M_),10),scale="uv",center=TRUE)
          M_<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
          M_<-as.data.frame(M_)
        }
        # M._<-M_  # ????
        ncompo<-min(as.numeric(input$pcr_n_comp),ncol(M_)-1)
        model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
        
        
        res<-pcr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
                  validation='CV',segments=as.numeric(input$pcr_n_cv),scale=as.logical(input$pcr_scale))
        resf<-pcr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
                   scale=as.logical(input$pcr_scale))
        PLS$res <- res
        PLS$resf <- resf
        PLS$ncompo <- ncompo
        PLS$typ<-'PCR'
        PLS$dataset<-M_
        PLS$nY<-nY
        PLS$validation<-'CV'
        PLS$nseg<-as.numeric(input$n_cv)
        PLS$segtype<-'interleaved'
        PLS$scale<-as.logical(input$pcr_scale)
        PLS$model<-as.formula(model)
        if(input$pcr_cv_choise=="2"){
          N<-c(NULL)
          D<-data.frame(NULL)
          withProgress(message = 'Reapeated CV:',value = 0, {
            n <- as.numeric(input$pcr_n_rnd)
            for(i in 1:n){
              incProgress(detail = paste(i,"times"),amount = 1/n)
              a=as.numeric(Sys.time())
              set.seed(a)
              M_=M_[sample(nrow(M_)),]
              M_<-as.data.frame(M_)
              res<-pcr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
                        validation='CV',segments=as.numeric(input$pls_n_cv),scale=as.logical(input$pcr_scale))
              # resf<-pcr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
              # scale=as.logical(ans[[7]]))
              rmsep<-RMSEP(res,intercep=FALSE)
              N[i]<-which.min(rmsep$val[1,,])
              D<-rbind.data.frame(D,rmsep$val[1,,])
            }
          })
          colnames(D)<-paste('Comp',c(1:ncompo))
          D<-cbind.data.frame(N=N,D)
          # D_min<-apply(D[,-1],2,min)
          # D_max<-apply(D[,-1],2,max)
          R_sq<-1-apply(D[,-1]^2,2,mean)*length(Y_)/sum((Y_ - mean(Y_))^2)
          PLS$R_sq <- R_sq
          PLS$D <- D
        }
      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Matrix/Table Requested!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    }
  })
  
  output$model_pcr_out_1 <- renderPrint({
    validate(need(nrow(dati$DS)!=0,"Load a dataset!"))
    validate(need(!is.null(PLS$res),"Execute the model!"))
    req(input$pcr_cv_choise=="1")
    vm<-R2(PLS$res,estimate='CV',ncomp=1:input$pcr_n_comp,intercept=FALSE)$val[1,,]*100
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    cat(' ',"\n")
    cat('CV% Explained Variance',"\n")
    print(round(vm,2))
    cat(' ',"\n")
    cat('RMSECV',"\n")
    print(round(rmsep$val[1,,],4))
    cat(' ',"\n")
    cat(paste('Minimum RMSECV at component n.',which.min(rmsep$val[1,,])),"\n")
    
  })
  
  output$model_pcr_out_2 <- renderPrint({
    validate(need(nrow(dati$DS)!=0,"Load a dataset!"))
    validate(need(!is.null(PLS$res),"Execute the model!"))
    req(input$pcr_cv_choise=="2")
    req(!is.null(PLS$D))
    cat(' ',"\n")
    cat('CV% Explained Variance',"\n")
    print(round(PLS$R_sq*100,2),quote=FALSE)
    #   print(round(vm,2))
    cat(' ',"\n")
    cat('Global RMSECV',"\n")
    print(round(sqrt(apply(PLS$D^2,2,mean))[-1],4))
    #   print(round(rmsep$val[1,,],4))
    cat(' ',"\n")
    cat(paste('Minimum Global RMSECV at component n.',which.min(sqrt(apply(PLS$D^2,2,mean))[-1])))
    
    
  })
  
  output$pcr_n_comp_df<-renderUI({
    req(!is.null(PLS$res))
    req(PLS$typ=='PCR')
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    selectInput("pcr_n_comp_df", label = "Number of components",
                choices = c(2:length(dati$var_qt)),
                selected = which.min(rmsep$val[1,,]))
  })

  output$pcrmodel_out_df <- renderPrint({
    req(!is.null(PLS$res))
    req(PLS$typ=='PCR')
    # cat(paste('Model created with ',format(as.numeric(input$pcr_n_comp_df),digits=2),
    #           ' components',sep=''))
    cat(paste('Model', PLS$typ, 'created with '),'\n')
    cat(paste(format(as.numeric(input$pcr_n_comp_df),digits=2),' components',sep=''))
  })
  
  output$pcr_cv_plot<-renderPlot({
    req(!is.null(PLS$res))
    req(PLS$typ=='PCR')
    if(input$pcr_cv_choise=="1"){
      vm<-R2(PLS$res,estimate='CV',ncomp=1:as.numeric(PLS$ncompo),intercept=FALSE)$val[1,,]*100
      rmsep<-RMSEP(PLS$res,intercep=FALSE)
      op<-par(pty='s',mfrow=c(1,2))
      plot(rmsep$val[1,,],xlab='Number of Components',ylab='RMSECV',main='',xaxt='n');grid()
      lines(rmsep$val[1,,])
      axis(1,at=1:PLS$ncompo)
      vm<-R2(PLS$res,estimate='CV',ncomp=1:as.numeric(PLS$ncompo),intercept=FALSE)$val[1,,]*100
      plot(vm,xlab='Number of Components',ylab='CV % Explained Variance',ylim=c(min(0,min(vm)),100),xaxt='n');grid()#
      lines(vm)
      axis(1,at=1:PLS$ncompo)
      par(op)
    }
    if(input$pcr_cv_choise=="2"){
      req(!is.null(PLS$D))
      D_min<-apply(PLS$D[,-1],2,min)
      D_max<-apply(PLS$D[,-1],2,max)
      
      
      op<-par(pty='s',mfrow=c(2,2))
      plot(sqrt(apply(PLS$D^2,2,mean))[-1],xlab='Number of Components',ylab='Global RMSECV',main='',
           ylim = c(min(D_min),max(D_max)),xaxt='n');grid()
      lines(sqrt(apply(PLS$D^2,2,mean))[-1]) 
      lines(D_min,col='red',lty = 2)
      lines(D_max,col='red',lty = 2)
      axis(1,at=1:PLS$ncompo)
      
      plot(PLS$R_sq*100,xlab='Number of Components',ylab='CV % Explained Variance',
           ylim=c(min(0,min(PLS$R_sq*100)),100),xaxt='n');grid()
      lines(PLS$R_sq*100)
      axis(1,at=1:PLS$ncompo)
      
      N <- PLS$D[,1]
      N<-c(N,min(N):max(N))
      F<-as.factor(N)
      plot(F[1:as.numeric(input$pcr_n_rnd)],xlab='Number of Components',ylab='Frequency',xaxt='n')
      axis(1,at=1:PLS$ncompo)
      par(op)
      
    }
  })
  
  # PCR - permutation -------------------------------------------------------
  
  observeEvent(input$bpcr_perm,{
    validate(need(nrow(dati$DS)!=0,""))
    req(PLS$res)
    
    Y_<-PLS$dataset[,1,drop=FALSE]
    X_<-PLS$dataset[,-1,drop=FALSE]
    D<-data.frame(NULL)
    withProgress(message = 'computation progress bar:',value = 0, {
      n <- as.numeric(input$pcr_n_prm) #da definire
      for(k in 1:n){
        incProgress(detail = sprintf("%d%% done", round(k/n*100)),amount = 1/n)
        a=as.numeric(Sys.time())
        set.seed(a)
        Y_=Y_[sample(nrow(Y_)),,drop=FALSE]
        X_=X_[sample(nrow(X_)),,drop=FALSE]
        Data<-cbind.data.frame(Y_,X_)
        res<-pcr(formula = PLS$model, ncomp = as.numeric(input$pls_n_comp_df), data = Data,
                  scale = PLS$scale, validation = "CV", segment.type = "interleaved",segments = as.numeric(input$pls_n_cv))
        rmsep<-RMSEP(res,intercep=FALSE)
        D<-rbind.data.frame(D,rmsep$val[1,,as.numeric(input$pcr_n_comp_df)])
      }
    })
    colnames(D)<-'RMSECV'
    PLS$D_perm <- D
    
  })
  
  output$pcr_perm_plot <- renderPlot({
    req(PLS$D_perm)
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    RMSEP <- rmsep$val[1,,as.numeric(input$pcr_n_comp_df)]
    
    
    plot(main='Distribution density',xlab='RMSECV',density(PLS$D_perm[,1]),xlim=c(min(RMSEP,PLS$D_perm[,1]),max(RMSEP,PLS$D_perm[,1])))
    abline(v=RMSEP, col="blue")
  })
  
  output$pcr_perm_txt <- renderPrint({
    req(PLS$D_perm)
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    RMSEP <- rmsep$val[1,,as.numeric(input$pcr_n_comp_df)]
    if(median(PLS$D_perm[,1])>=RMSEP){
      cat(paste0(sum(PLS$D_perm[,1]<=RMSEP)/as.numeric(input$pcr_n_prm),'%'))
    }else{
      cat(paste0(sum(PLS$D_perm[,1]>=RMSEP)/as.numeric(input$pcr_n_prm),'%'))
    }
  })


# PLS - costruzione modello -------------------------------------------------

  output$pls_n_comp<-renderUI({
    req(dati$var_qt)
    n <- length(dati$var_qt)-length(input$var_y)
    sel <- min(10,n)
    selectInput("pls_n_comp", label = "Max. number of components", 
                choices = c(2:n), 
                selected = sel)
      })

  output$pls_n_cv<-renderUI({
    req(!is.null(dati$DS))
    selectInput("pls_n_cv", label = "Number of segments for CV", 
                choices = c(2:nrow(dati$DS)), 
                selected = 5)
  })
  
  output$pls_n_rnd<-renderUI({
    req(!is.null(dati$DS))
    req(input$pls_cv_choise=="2")
    numericInput("pls_n_rnd", label = "Number of randomizations",value = 100,min = 1,max = 10000)
  })
  
  observeEvent(input$bplsmodel,{
   validate(need(nrow(dati$DS)!=0,""))
    if(is.null(input$var_y)){
      sendSweetAlert(session, title = "Input Error",
                     text = 'Select response variable!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      M_<-dati$DS[,dati$var_qt]
      if(!is.null(input$var_y))M_ <- M_[,colnames(M_)!=input$var_y]
      Y_ <- dati$DS[,input$var_y]
      if((typeof(M_)=='double')|(typeof(M_)=='list')){
        M_<-data.frame(cbind(Y_,data.frame(M_)))
        naM<-names(M_)
        nNA<-sum(is.na(M_))
        nY<-1
        if(nNA>0){
          mess<-paste(as.character(nNA),'NA present.We try to rebuild them!')
          showNotification(mess)
          md<-prep(M_,scale="uv",center=TRUE,simple=FALSE,rev=FALSE)
          res<-pca(md$data,method="nipals",nPcs=min(ncol(M_),10),scale="uv",center=TRUE)
          M_<-prep(res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
          M_<-as.data.frame(M_)
        }
        # M._<-M_  # ????
        ncompo<-min(as.integer(input$pls_n_comp),ncol(M_)-1)
        model<-paste(naM[nY],'~',(paste(naM[-nY],collapse='+')),sep='')
        
        
        res<-plsr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
                  validation='CV',segments=as.numeric(input$pls_n_cv),scale=as.logical(input$pls_scale))
        resf<-plsr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
                   scale=as.logical(input$pls_scale))
        PLS$res <- res
        PLS$resf <- resf
        PLS$ncompo <- ncompo
        PLS$typ<-'PLS'
        PLS$dataset<-M_
        PLS$nY<-nY
        PLS$validation<-'CV'
        PLS$nseg<-as.numeric(input$n_cv)
        PLS$segtype<-'interleaved'
        PLS$scale<-as.logical(input$pls_scale)
        PLS$model<-as.formula(model)
        if(input$pls_cv_choise=="2"){
          N<-c(NULL)
          D<-data.frame(NULL)
          withProgress(message = 'Reapeated CV:',value = 0, {
            n <- as.numeric(input$pls_n_rnd)
            for(i in 1:n){
              incProgress(detail = paste(i,"times"),amount = 1/n)
              a=as.numeric(Sys.time())
              set.seed(a)
              M_=M_[sample(nrow(M_)),]
              M_<-as.data.frame(M_)
              res<-plsr(as.formula(model),ncomp=ncompo,data=M_,segment.type="interleaved",
                        validation='CV',segments=as.numeric(input$pls_n_cv),scale=as.logical(input$pls_scale))
              # resf<-plsr(as.formula(model),ncomp=ncompo,data=M_,validation='none',
              # scale=as.logical(ans[[7]]))
              rmsep<-RMSEP(res,intercep=FALSE)
              N[i]<-which.min(rmsep$val[1,,])
              D<-rbind.data.frame(D,rmsep$val[1,,])
            }
          })
          colnames(D)<-paste('Comp',c(1:ncompo))
          D<-cbind.data.frame(N=N,D)
          # D_min<-apply(D[,-1],2,min)
          # D_max<-apply(D[,-1],2,max)
          R_sq<-1-apply(D[,-1]^2,2,mean)*length(Y_)/sum((Y_ - mean(Y_))^2)
          PLS$R_sq <- R_sq
          PLS$D <- D
        }
        }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Matrix/Table Requested!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    }
    })
  
  output$model_out_1 <- renderPrint({
    validate(need(nrow(dati$DS)!=0,"Load a dataset!"))
    validate(need(!is.null(PLS$res),"Execute the model!"))
    req(input$pls_cv_choise=="1")
    vm<-R2(PLS$res,estimate='CV',ncomp=1:PLS$ncompo,intercept=FALSE)$val[1,,]*100
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    cat(' ',"\n")
    cat('CV% Explained Variance',"\n")
    print(round(vm,2))
    cat(' ',"\n")
    cat('RMSECV',"\n")
    print(round(rmsep$val[1,,],4))
    cat(' ',"\n")
    cat(paste('Minimum RMSECV at component n.',which.min(rmsep$val[1,,])),"\n")
    
  })
  
  output$model_out_2 <- renderPrint({
    validate(need(nrow(dati$DS)!=0,"Load a dataset!"))
    validate(need(!is.null(PLS$res),"Execute the model!"))
    req(input$pls_cv_choise=="2")
    req(!is.null(PLS$D))
    cat(' ',"\n")
    cat('CV% Explained Variance',"\n")
    print(round(PLS$R_sq*100,2),quote=FALSE)
    #   print(round(vm,2))
    cat(' ',"\n")
    cat('Global RMSECV',"\n")
    print(round(sqrt(apply(PLS$D^2,2,mean))[-1],4))
    #   print(round(rmsep$val[1,,],4))
    cat(' ',"\n")
    cat(paste('Minimum Global RMSECV at component n.',which.min(sqrt(apply(PLS$D^2,2,mean))[-1])))
    
    
  })
  
  output$pls_n_comp_df<-renderUI({
    req(!is.null(PLS$res))
    req(PLS$typ=='PLS')
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    selectInput("pls_n_comp_df", label = "Number of components",
                choices = c(2:length(dati$var_qt)),
                selected = which.min(rmsep$val[1,,]))
  })
  
  output$plsmodel_out_df <- renderPrint({
    req(!is.null(PLS$res))
    req(PLS$typ=='PLS')
    # cat(paste('Model created with ',format(as.numeric(PLS$ncompo_df),digits=2),
    #           ' components',sep=''))
    cat(paste('Model', PLS$typ, 'created with '),'\n')
    cat(paste(format(as.numeric(input$pls_n_comp_df),digits=2),' components',sep=''))
  })
  
  output$pls_cv_plot<-renderPlot({
    req(!is.null(PLS$res))
    req(PLS$typ=='PLS')
    if(input$pls_cv_choise=="1"){
      vm<-R2(PLS$res,estimate='CV',ncomp=1:as.numeric(PLS$ncompo),intercept=FALSE)$val[1,,]*100
      rmsep<-RMSEP(PLS$res,intercep=FALSE)
      op<-par(pty='s',mfrow=c(1,2))
      plot(rmsep$val[1,,],xlab='Number of Components',ylab='RMSECV',main='',xaxt='n');grid()
      lines(rmsep$val[1,,])
      axis(1,at=1:PLS$ncompo)
      vm<-R2(PLS$res,estimate='CV',ncomp=1:as.integer(PLS$ncompo),intercept=FALSE)$val[1,,]*100
      plot(vm,xlab='Number of Components',ylab='CV % Explained Variance',ylim=c(min(0,min(vm)),100),xaxt='n');grid()#
      lines(vm)
      axis(1,at=1:PLS$ncompo)
      par(op)
    }
    if(input$pls_cv_choise=="2"){
      req(!is.null(PLS$D))
      D_min<-apply(PLS$D[,-1],2,min)
      D_max<-apply(PLS$D[,-1],2,max)
      
      op<-par(pty='s',mfrow=c(2,2))
      plot(sqrt(apply(PLS$D^2,2,mean))[-1],xlab='Number of Components',ylab='Global RMSECV',main='',
           ylim = c(min(D_min),max(D_max)),xaxt='n');grid()
      lines(sqrt(apply(PLS$D^2,2,mean))[-1]) 
      lines(D_min,col='red',lty = 2)
      lines(D_max,col='red',lty = 2)
      axis(1,at=1:PLS$ncompo)
      
      plot(PLS$R_sq*100,xlab='Number of Components',ylab='CV % Explained Variance',
           ylim=c(min(0,min(PLS$R_sq*100)),100),xaxt='n');grid()
      lines(PLS$R_sq*100)
      axis(1,at=1:PLS$ncompo)
      
      N <- PLS$D[,1]
      N<-c(N,min(N):max(N))
      F<-as.factor(N)
      plot(F[1:as.integer(input$pls_n_rnd)],xlab='Number of Components',ylab='Frequency')
      # axis(1,at=1:PLS$ncompo)
      par(op)
      
    }
  })

# PLS - permutation -------------------------------------------------------

  observeEvent(input$bpls_perm,{
    validate(need(nrow(dati$DS)!=0,""))
    req(PLS$res)
    
    Y_<-PLS$dataset[,1,drop=FALSE]
    X_<-PLS$dataset[,-1,drop=FALSE]
    D<-data.frame(NULL)
    withProgress(message = 'computation progress bar:',value = 0, {
      n <- as.numeric(input$pls_n_prm) #da definire
      for(k in 1:n){
        incProgress(detail = sprintf("%d%% done", round(k/n*100)),amount = 1/n)
        a=as.numeric(Sys.time())
        set.seed(a)
        Y_=Y_[sample(nrow(Y_)),,drop=FALSE]
        X_=X_[sample(nrow(X_)),,drop=FALSE]
        Data<-cbind.data.frame(Y_,X_)
        res<-plsr(formula = PLS$model, ncomp = as.numeric(input$pls_n_comp_df), data = Data,
                  scale = PLS$scale, validation = "CV", segment.type = "interleaved",segments = as.numeric(input$pls_n_cv))
        rmsep<-RMSEP(res,intercep=FALSE)
        D<-rbind.data.frame(D,rmsep$val[1,,as.numeric(input$pls_n_comp_df)])
      }
    })
    colnames(D)<-'RMSECV'
    PLS$D_perm <- D
    
  })
  
  output$pls_perm_plot <- renderPlot({
    req(PLS$D_perm)
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    RMSEP <- rmsep$val[1,,as.numeric(input$pls_n_comp_df)]
    
    
    plot(main='Distribution density',xlab='RMSECV',density(PLS$D_perm[,1]),xlim=c(min(RMSEP,PLS$D_perm[,1]),max(RMSEP,PLS$D_perm[,1])))
    abline(v=RMSEP, col="blue")
  })
  
  output$pls_perm_txt <- renderPrint({
    req(PLS$D_perm)
    rmsep<-RMSEP(PLS$res,intercep=FALSE)
    RMSEP <- rmsep$val[1,,as.numeric(input$pls_n_comp_df)]
    if(median(PLS$D_perm[,1])>=RMSEP){
      cat(paste0(sum(PLS$D_perm[,1]<=RMSEP)/as.numeric(input$pls_n_prm),'%'))
    }else{
      cat(paste0(sum(PLS$D_perm[,1]>=RMSEP)/as.numeric(input$pls_n_prm),'%'))
    }
  })

# PLS - Exp vs calc -------------------------------------------------------------

  output$pls_expvsfitted_label <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_expvsfitted_label", label = "Label variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_expvsfitted_col <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_expvsfitted_col", label = "Color variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_expvsfitted_rnames <- renderUI({
    req(!is.null(PLS$res))
    checkboxInput("pls_expvsfitted_rnames", label = "Row names", value = FALSE)
  })
  
  output$pls_expvsfitted <- renderPlot({
    # req(!is.null(PLS$res))
    validate(need(!is.null(PLS$res),"Compute the model!"))
    req(!is.null(dati$DS))
    req(!is.null(input$pls_expvsfitted_rnames))
    require(gplots)
    g<-NULL;tex<-NULL;vcolor<-NULL
    if(!is.null(input$pls_expvsfitted_col)){
      g<-dati$DS[,input$pls_expvsfitted_col]
      g<-factor(g)
      vcolor<-colorpanel(nlevels(g),low="red",high="green") }
    if(!is.null(input$pls_expvsfitted_label)){
      tex<-dati$DS[,input$pls_expvsfitted_label]}
    if(as.logical(input$pls_expvsfitted_rnames))tex<-row.names(dati$DS)
    if(is.null(input$var_y)){
      sendSweetAlert(session, title = "Input Error",
                     text = 'Select response variable!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      ms<-dati$DS[,input$var_y]
      op<-par(pty='s',mfrow=c(1,2))
      if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
      if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)

      ft<-PLS$resf$fitted.values[,,n_comp_df]
      yl<-c(min(ft,ms),max(ft,ms))

      plot(ms,ft,xlab='Experimental Value',ylab='Fitted Value',xlim=yl,ylim=yl,
           main=paste('Model with',n_comp_df,'Comp.'),type='n')
      lines(par('usr')[1:2],par('usr')[3:4]);grid()
      if((is.null(g))&(is.null(tex)))points(ms,ft,col='black')
      if((!is.null(g))&(is.null(tex)))points(ms,ft,col=vcolor[as.numeric(g)])
      if((is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),cex=0.7)
      if((!is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=0.7)
      
      ft<-PLS$res$validation$pred[,,n_comp_df]
      yl<-c(min(ft,ms),max(ft,ms))
      plot(ms,ft,xlab='Experimental Value',ylab='CV Value',xlim=yl,ylim=yl,
           main=paste('Model with',n_comp_df,'Comp.'),type='n')
      lines(par('usr')[1:2],par('usr')[3:4]);grid()
      if((is.null(g))&(is.null(tex)))points(ms,ft,col='black')
      if((!is.null(g))&(is.null(tex)))points(ms,ft,col=vcolor[as.numeric(g)])
      if((is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),cex=0.7)
      if((!is.null(g))&(!is.null(tex)))text(ms,ft,as.character(tex),col=vcolor[as.numeric(g)],cex=0.7)
      par(op)
    }
  })
  
  output$pls_fitting_dwl <- downloadHandler(
    filename = "fitted.xlsx", 
    content = function(file) {
      if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
      if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
      ft<-PLS$resf$fitted.values[,,n_comp_df]
      ft_cv<-PLS$res$validation$pred[,,n_comp_df]
      df<-cbind.data.frame('Fitted Value'=ft,'CV Value'=ft_cv)
      write.xlsx(df, file,colNames=TRUE)
    })
  

# PLS - residui -----------------------------------------------------------

  output$pls_res_label <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_res_label", label = "Label variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_res_col <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_res_col", label = "Color variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_res_rnames <- renderUI({
    req(!is.null(PLS$res))
    checkboxInput("pls_res_rnames", label = "Row names", value = FALSE)
  })
  
  output$pls_res_plot <- renderPlot({
    # req(!is.null(PLS$res))
    validate(need(!is.null(PLS$res),"Compute the model!"))
    req(!is.null(dati$DS))
    req(!is.null(input$pls_res_rnames))
    
    require(gplots)
    g<-NULL;tex<-NULL;vcolor<-NULL
    if(!is.null(input$pls_res_col)){
      g<-dati$DS[,input$pls_res_col]
      g<-factor(g)
      vcolor<-colorpanel(nlevels(g),low="red",high="green") }
    if(!is.null(input$pls_res_label)){
      tex<-dati$DS[,input$pls_res_label]}
    if(as.logical(input$pls_res_rnames))tex<-row.names(dati$DS)
    if(is.null(input$var_y)){
      sendSweetAlert(session, title = "Input Error",
                     text = 'Select response variable!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      ms<-dati$DS[,input$var_y]
      op<-par(pty='s',mfrow=c(1,2))
      
      if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
      if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)

      rs<-PLS$resf$fitted.values[,,n_comp_df]-ms
      plot(1:length(rs),rs,type='n',xlab='Object Number',ylim=c(min(0,rs),max(0,rs)),
           ylab=paste('Residuals in Fitting with ',n_comp_df,' Comp.'));grid();
      abline(h=0,col="red")
      if((is.null(g))&(is.null(tex)))points(1:length(rs),rs,col='black')
      if((!is.null(g))&(is.null(tex)))points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
      if((is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),cex=0.8,pch=16)
      if((!is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),
                                            col=vcolor[as.numeric(g)],cex=0.8)
      rs<-PLS$res$validation$pred[,,n_comp_df]-ms
      plot(1:length(rs),rs,xlab='Object Number',ylab=paste('Residuals in CV with ',n_comp_df,
                                                           ' Comp.'),type='n',ylim=c(min(0,rs),max(0,rs)));grid()
      abline(h=0,col="red")
      if((is.null(g))&(is.null(tex)))points(1:length(rs),rs,col='black')
      if((!is.null(g))&(is.null(tex)))points(1:length(rs),rs,col=vcolor[as.numeric(g)],pch=16)
      if((is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),cex=0.8,pch=16)
      if((!is.null(g))&(!is.null(tex)))text(1:length(rs),rs,as.character(tex),
                                            col=vcolor[as.numeric(g)],cex=0.8)
      par(op)
    }
  })
  
  output$pls_res_dwl <- downloadHandler(
    filename = "residuals.xlsx", 
    content = function(file) {
      ms<-dati$DS[,input$var_y]
   
      if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
      if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
      
      rs<-PLS$resf$fitted.values[,,n_comp_df]-ms
      rs_cv<-PLS$res$validation$pred[,,n_comp_df]-ms
      df<-cbind.data.frame('Residuals'=rs,'Residuals in CV'=rs_cv)
      write.xlsx(df, file,colNames=TRUE)
    })
  

# PLS - scores plots -------------------------------------------------------

  output$pls_score_compx <- renderUI({
    req(!is.null(PLS$res))
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_score_compx", label = "Component on x-axis",
                choices = 1:n_comp_df,
                selected = 1)
  })
  
  output$pls_score_compy <- renderUI({
    req(!is.null(PLS$res))
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_score_compy", label = "Component on y-axis",
                choices = 1:n_comp_df,
                selected = 2)
  })
  
  output$pls_score_label <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_score_label", label = "Label variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_score_col <- renderUI({
    req(!is.null(PLS$res))
    req(!is.null(dati$var_ql))
    pickerInput("pls_score_col", label = "Color variable",
                choices = dati$var_ql,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pls_score_rnames <- renderUI({
    req(!is.null(PLS$res))
    checkboxInput("pls_score_rnames", label = "Row names", value = FALSE)
  })
  
  output$pls_scores_plot <- renderPlot({
    # req(!is.null(PLS$res))
    validate(need(!is.null(PLS$res),"Compute the model!"))
    req(!is.null(dati$DS))
    req(!is.null(input$pls_score_rnames))
    n1<-as.numeric(input$pls_score_compx)
    n2<-as.numeric(input$pls_score_compy)
    Ms<-PLS$res$scores
    if(input$pls_res_scale==1){
      yl<-c(min(Ms[,n1],Ms[,n2]),max(Ms[,n1],Ms[,n2]))
      xl<-yl
    }else{ 
      yl<-c(min(Ms[,n2]),max(Ms[,n2]))
      xl<-c(min(Ms[,n1]),max(Ms[,n1]))
    }
    tex<-NULL;grade<-NULL
    if(!is.null(input$pls_score_label))tex<-dati$DS[,input$pls_score_label]
    if(input$pls_score_rnames)tex<-rownames(dati$DS)
    if(!is.null(input$pls_score_col)){
      grade<-dati$DS[,input$pls_score_col]
      grade<-factor(grade)
      lev<-levels(grade)
      nl<-nlevels(grade)
      vcolor<-unlist(dovc(as.character(lev)))
    }
    V <- explvar(PLS$res) 
    if(is.null(tex) & is.null(grade)){
      plot(Ms[,n1],Ms[,n2],
           xlab=paste('Component ',n1,' (',as.character(round(V[n1],1)),'% of variance)',sep=''),
           ylab=paste('Component ',n2,' (',as.character(round(V[n2],1)),'% of variance)',sep=''),
           xlim=xl,ylim=yl,pty='o',col='black');grid()
    }
    if(!is.null(tex)& is.null(grade)){
      plot(Ms[,n1],Ms[,n2],type='n',
           xlab=paste('Component ',n1,' (',as.character(round(V[n1],1)),'% of variance)',sep=''),
           ylab=paste('Component ',n2,' (',as.character(round(V[n2],1)),'% of variance)',sep=''),
           xlim=xl,ylim=yl);grid()
      text(Ms[,n1],Ms[,n2],as.character(tex),col='black',cex=0.8)
    }
    if(is.null(tex)&!is.null(grade)){
      plot(Ms[,n1],Ms[,n2],type='n',
           xlab=paste('Component ',n1,' (',as.character(round(V[n1],1)),'% of variance)',sep=''),
           ylab=paste('Component ',n2,' (',as.character(round(V[n2],1)),'% of variance)',sep=''),
           xlim=xl,ylim=yl);grid()
      for(i in 1:nl){
        points(subset(Ms[,c(n1,n2)],grade==lev[i]),pch=19,col=vcolor[i])
      }
      rm(lev,nl,vcolor)
    }
    if(!is.null(tex)& !is.null(grade)){
      plot(Ms[,n1],Ms[,n2],type='n',
           xlab=paste('Component ',n1,' (',as.character(round(V[n1],1)),'% of variance)',sep=''),
           ylab=paste('Component ',n2,' (',as.character(round(V[n2],1)),'% of variance)',sep=''),
           xlim=xl,ylim=yl);grid()
      for(i in 1:nl){
        text(subset(Ms[,c(n1,n2)],grade==lev[i]),as.character(subset(tex,grade==lev[i])),
             col=vcolor[i],cex=0.8)
      }
    }
    text(0,0,'+',cex=1.2,col='red')
  })
  
  output$pls_score_dwl <- downloadHandler(
    filename = "scores.xlsx",
    content = function(file) {
      df <- PLS$res$scores[,]
      write.xlsx(df, file,colNames=TRUE)
    })

# PLS - loading plots -----------------------------------------------------

  output$pls_load_compx <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='sca')
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_load_compx", label = "Component on x-axis", 
                choices = 1:n_comp_df, 
                selected = 1)
  })
  
  output$pls_load_compy <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='sca')
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_load_compy", label = "Component on y-axis", 
                choices = 1:n_comp_df, 
                selected = 2)
  })
  
  output$pls_load_rnames <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='sca')
    checkboxInput("pls_load_rnames", label = "Variable names", value = FALSE)
  })
  
  output$pls_load_arrows <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='sca')
    checkboxInput("pls_load_arrows", label = "Arrows", value = FALSE)
  })
  
  
  output$pls_load_chk <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='line')
    checkboxInput("pls_load_chk", label = "Header on the x axis?", value = TRUE)
  })
  
  output$pls_load_linecomp <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='line')
    textInput("pls_load_linecomp", label = "Components to be plotted (e.g.,1,3,5)", value = "1,2")
  })
  
  output$pls_load_compN <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='bar')
    selectInput("pls_load_compN", label = "Component number", 
                choices = 1:PLS$res@nPcs, 
                selected = 1)
  })
  
  output$pls_load_cnames <- renderUI({
    req(!is.null(PLS$res))
    req(input$pls_radio_load_type=='bar')
    checkboxInput("pls_load_cnames", label = "Column names", value = FALSE)
  })
  
  output$loading_pl <- renderPlot({
    # req(!is.null(PLS$res))
    validate(need(!is.null(PLS$res),"Compute the model!"))
    # req(!is.null(input$pls_load_rnames))
    require(stringr)
    require(gplots)
    V <- explvar(PLS$res) 
    if(input$pls_radio_load_type=="sca"){
      req(!is.null(input$pls_load_rnames))
      n1<-as.integer(input$pls_load_compx)
      n2<-as.integer(input$pls_load_compy)
      T<-loadings(PLS$res)
      tex<-as.character(1:nrow(T))
      if(as.logical(input$pls_load_rnames))tex<-row.names(T)
      Tlim<-c(min(T[,c(n1,n2)]),max(T[,c(n1,n2)]))
      Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
      plot(T[,n1],T[,n2],
           xlab=paste('Component ',n1,' (',as.character(round(V[n1],1)),'% of variance)',sep=''),
           ylab=paste('Component ',n2,' (',as.character(round(V[n2],1)),'% of variance)',sep=''),
           main='X-loading Plot',type='n',xlim=Tlim,ylim=Tlim)
      text(T[,n1],T[,n2],tex,cex=0.6)
      text(0,0,'+',cex=1.2,col='red')
      grid()
      if(as.logical(input$pls_load_arrows))
        arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,n1],T[,n2],col='red')
    }
    if(input$pls_radio_load_type=="line"){
      req(input$pls_load_linecomp)
      # if(length(as.numeric(unlist(str_split(input$pls_load_linecomp,','))))==2){
        # req(length(as.numeric(unlist(str_split(input$pls_load_linecomp,','))))==2)
      T<-loadings(PLS$res)
      vi<-as.numeric(unlist(str_split(input$pls_load_linecomp,',')))
      M_<-dati$DS[,dati$var_qt]
      if(!is.null(input$var_y))M_ <- M_[,colnames(M_)!=input$var_y]
      at <- 1:length(rownames(T))
      tk <- TRUE
      # if(length(rownames(T))>15)tk <- FALSE
      if(length(( rownames(T)))>15){
        n<-length(rownames(T))
        m<-floor((n-1)/11)
        at=seq(1,n,m)
      }
      assex <- 1:length(rownames(T))
      x_lab <- 'Variable Number'
      if(input$pls_load_chk){
        assex <- colnames(M_)
        x_lab <- ''
      }
      if(sum(is.na(vi))==0){
        ylim <- c(min(T[,vi]),max(T[,vi]))
        # ylim <- c(min(T),max(T))
        plot(T[,1],ylab='x-loading value',
             xlab=x_lab,type='n',xaxt='n',
             # ylim=c(min(T),max(T))
             ylim=ylim)
        axis(side=1, at=at,labels=assex[at],cex.axis=0.8,tick=tk)
        grid()
        leg <- c(NULL);k=0
        for(i in vi){
          lines(T[,i],col=i)
          k=k+1
          leg[k] <- paste('Component ',i,' (',as.character(round(V[i],1)),'% of variance)',sep='')
        }
        legend("bottomleft",legend=leg,col=vi,lty=1)
      }
    }
  })
  
  output$pls_loading_dwl <- downloadHandler(
    filename = "loadings.xlsx",
    content = function(file) {
      df <- loadings(PLS$res)[,]
      write.xlsx(df, file,colNames=TRUE)
    })

# PLS - biplot ------------------------------------------------------------
  output$pls_biplot_compx <- renderUI({
    req(!is.null(PLS$res))
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_biplot_compx", label = "Component on x-axis", 
                choices = 1:n_comp_df, 
                selected = 1)
  })
  
  output$pls_biplot_compy <- renderUI({
    req(!is.null(PLS$res))
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    selectInput("pls_biplot_compy", label = "Component on y-axis", 
                choices = 1:n_comp_df, 
                selected = 2)
  })
  
  output$pls_biplot_arrows <- renderUI({
    checkboxInput("pls_biplot_arrows", label = "Arrows", value = FALSE)
  })
  
  output$biplot <- renderPlot({
    # req(!is.null(PLS$res))
    validate(need(!is.null(PLS$res),"Compute the model!"))
    req(input$pls_biplot_compx)
    req(input$pls_biplot_compy)
    req(!is.null(input$pls_biplot_arrows))
    
    biplot(PLS$res,comps=c(as.numeric(input$pls_biplot_compx),as.numeric(input$pls_biplot_compy)),var.axes=as.logical(input$pls_biplot_arrows))
    grid()
  })


# PLS - coefficients ------------------------------------------------------

  output$pls_coeff_comp <- renderUI({
    req(!is.null(PLS$res))
    # req(input$pls_radio_load_type=='line')
    textInput("pls_coeff_comp", label = "Number of latent (e.g.,1,3,5)", value = "1,2")
  })

  output$coeff_pl <- renderPlot({
    validate(need(!is.null(PLS$res),"Compute the model!"))
    # req(!is.null(PLS$res))
    req(input$pls_coeff_comp)
    if (is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]
    if (!is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]/PLS$res$scale
    vi<-as.numeric(unlist(str_split(input$pls_coeff_comp,',')))
    #print(Cm)
    M_<-dati$DS[,dati$var_qt]
    if(!is.null(input$var_y))M_ <- M_[,colnames(M_)!=input$var_y]
    
    at <- 1:length(colnames(M_))
    tk <- TRUE
    if(length(colnames(M_))>15){
      n<-length((colnames(PLS$dataset)[-PLS$nY]))
      m<-floor((n-1)/11)
      at=seq(1,n,m)
    }
    assex <- 1:length(colnames(PLS$dataset)[-PLS$nY])
    x_lab <- 'Variable Number'
    if(input$  pls_coeff_chk){
      assex <- colnames(M_)
      x_lab <- ''
    }
    nCm<-length(Cm[,1])
    if(sum(is.na(vi))==0){
      plot(1:nCm,Cm[,1],xlab=x_lab,ylab='Regression Coefficients',type='n',xaxt='n',
           ylim=c(min(Cm[,vi]),max(Cm[,vi])))
      axis(side=1, at=at,labels=assex[at])
      grid()
      for(i in vi)lines(Cm[,i],col=which(vi==i))
      legend("bottomleft",legend=as.character(vi),col=1:length(vi),lty=1)
      abline(0,0,lty=2)
    }
  })

  output$pls_coefficients_dwl <- downloadHandler(
    filename = "coefficients.xlsx",
    content = function(file) {
      if (is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]
      if (!is.null(PLS$res$scale))Cm<-PLS$res$coefficients[,1,]/PLS$res$scale
      write.xlsx(Cm, file,colNames=TRUE)
    })

# PLS - prediction --------------------------------------------------------

  output$lista_esempi_ext<-renderUI({
    fnames<-list.files(path = 'Dati')
    fext<-tools::file_ext(fnames)
    fnames<-fnames[fext %in% c("xlsx")]
    fnames<-tools::file_path_sans_ext(fnames)
    selectInput('lista_esempi_ext',"Load data set to be predicted from the examples",choices = c('',fnames),selected = 1,
                width="74%")
  })

  observeEvent(input$lista_esempi_ext,{
    if(input$lista_esempi_ext!=""){
      path<-paste("Dati/",input$lista_esempi_ext,".xlsx",sep="")
      df <- tryCatch(
        read_excel(path = path,sheet = 1,col_names = TRUE)
        )
      suppressWarnings(colnames(df)[!is.na(as.numeric(colnames(df)))]
                       <-as.numeric(colnames(df)[!is.na(as.numeric(colnames(df)))]))
      dati_ext$DS<-as.data.frame(df)
      dati_ext$DS_nr <- as.data.frame(df)
    }
  })

  output$pls_pred_data_paste_sp <- renderUI({
    req(input$pls_pred_data_load=='paste')
    br()
  })
  
  output$pls_pred_data_paste_sp1 <- renderUI({
    req(input$pls_pred_data_load=='paste')
    hr()
  })
  
  output$pls_pred_data_paste <- renderUI({
    req(input$pls_pred_data_load=='paste')
    actionButton("pls_pred_data_paste", label = "Paste")
  })
  
  output$pls_pred_data_excel <- renderUI({
    req(input$pls_pred_data_load=='excel')
    fileInput("pls_pred_data_excel", " ",
              multiple = FALSE,
              accept = c(".xlx",".xlsx"))
  })
  
  observeEvent(input$pls_pred_data_excel,{
    df <- tryCatch(
      read_excel(path = input$pls_pred_data_excel$datapath,sheet = 1,col_names = TRUE))
    suppressWarnings(colnames(df)[!is.na(as.numeric(colnames(df)))]
                     <-as.numeric(colnames(df)[!is.na(as.numeric(colnames(df)))]))
    dati_ext$DS<-as.data.frame(df)
    dati_ext$DS_nr <- as.data.frame(df)
  })
  
  observeEvent(input$pls_pred_data_paste,{
    df <- tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE,check.names = FALSE),
                   error = function(e) "Selezionare un dataset!")
    df <- type.convert(df)
    dati_ext$DS<-as.data.frame(df)
    dati_ext$DS_nr <- as.data.frame(df)
  })

  output$pls_load_ds <- renderPrint({
    validate(need(!is.null(dati_ext$DS),"No data set!"))
    cat('Dimension loaded data set:',"\n")
    cat(dim(dati_ext$DS))
  })

  output$pls_pred_data_var_y<-renderUI({
    req(input$pls_pred_y_chk==TRUE)
    selectizeInput(inputId = "pls_pred_data_var_y"," ",
                   choices = colnames(dati_ext$DS),
                   options = list(
                     placeholder = 'Select response variable',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
  })
  
  pls_pred_data <- reactiveValues(var_y=NULL)
  observeEvent(input$pls_pred_data_var_y,{
    req(input$pls_pred_y_chk)
    pls_pred_data$var_y <- input$pls_pred_data_var_y
  })
    
  observeEvent(input$bplsext,{
    if(is.null(PLS$res)){
      sendSweetAlert(session, title = "Input Error",
                     text = 'Compute the model!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      if(is.null(dati_ext$DS)){
        sendSweetAlert(session, title = "Input Error",
                       text = 'Load data set to be predicted!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }else{
        if(pls_pred_data$var_y==""){
          sendSweetAlert(session, title = "Input Error",
                         text = 'Select response variable',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }else{
          M_<-dati_ext$DS
          M_<-data.frame(M_)
          if(input$pls_pred_y_chk){
            Y_ <- dati_ext$DS[,input$pls_pred_data_var_y]
          }else{
            Y_<-rep(0,nrow(M_))
          }
        
          if(PLS$typ=='PCR') ncomp <- as.numeric(input$pcr_n_comp_df)
          if(PLS$typ=='PLS') ncomp <- as.numeric(input$pls_n_comp_df)
          
          # ncomp <- as.numeric(input$pls_n_comp_df)
          prm<-drop(predict(PLS$res,newdata=M_,ncomp=1:ncomp,scale=PLS$scale))
          PLS_ext$prm <- prm
          prm.tr<-drop(predict(PLS$res,newdata=NULL,ncomp=1:ncomp,scale=PLS$scale))
          PLS_ext$prm.tr <- prm.tr
          if(input$pls_pred_y_chk){
            rmsep<-RMSEP(PLS$res,ncomp=ncomp,scale=PLS$scale,
                         intercept=FALSE)$val[1,,]
            PLS_ext$rmsep <- rmsep
          }
          if(nrow(M_)==1){
            prm<-matrix(unlist(prm),1,ncomp)
            PLS_ext$prm <- prm
          }
          res<-prm[,ncomp]-Y_
          PLS_ext$res <- res
          res.tr<-prm.tr[,ncomp]-PLS$dataset[,1]
          PLS_ext$res.tr <- res.tr
        }
      }
      }
  })
  
  output$pls_pred_print <- renderPrint({
    req(!is.null(PLS_ext$res))
    if(input$pls_pred_y_chk){
      cat('Prediction Statistics',"\n")
      cat('',"\n")
      cat(paste('RMSEP:',format(PLS_ext$rmsep,digits=4)),"\n")
      cat(paste('BIAS :',format(mean(PLS_ext$res),digits=4)),"\n")
      cat('',"\n")
    }
    cat('Predicted Values',"\n")
    if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
    print(PLS_ext$prm[,n_comp_df])
  })
  
  
  output$pls_ext_data_rnames <- renderUI({
    req(!is.null(PLS_ext$res))
    req(input$pls_pred_y_chk)
    checkboxInput("pls_ext_data_rnames", label = "Row names", value = FALSE)
  })
  
  output$pls_pred_data_pl <- renderPlot({
    req(!is.null(PLS_ext$res))
    req(input$pls_pred_data_var_y)
    req(input$pls_pred_y_chk==TRUE)
    req(!is.null(input$pls_ext_data_rnames))
    M_<-dati_ext$DS
    M_<-data.frame(M_)
    Y_ <- dati_ext$DS[,input$pls_pred_data_var_y]
    if(PLS$typ=='PCR') ncomp <- as.numeric(input$pcr_n_comp_df)
    if(PLS$typ=='PLS') ncomp <- as.numeric(input$pls_n_comp_df)
    # ncomp <- as.numeric(input$pls_n_comp_df)
    prm <- PLS_ext$prm
    prm.tr <- PLS_ext$prm.tr
    res <- PLS_ext$res
    res.tr <- PLS_ext$res.tr
    op<-par(pty='s',mfrow=c(1,2))
    if(!input$pls_ext_data_rnames){
      plot(Y_,prm[,ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
           xlim=c(min(c(Y_,prm[,ncomp],prm.tr[,ncomp])),
                  max(c(Y_,prm[,ncomp],prm.tr[,ncomp]))),
           ylim=c(min(c(Y_,prm[,ncomp],prm.tr[,ncomp])),
                  max(c(Y_,prm[,ncomp],prm.tr[,ncomp])))
           )
      lines(par('usr')[1:2],par('usr')[3:4],col='red')
      grid()
      plot(1:nrow(M_),res,xlab='Object Number',ylab='Residuals',
           ylim=c(min(min(res),min(res.tr)),max(c(res,res.tr))))
      abline(h=0,col="red")
      grid()
    }else{
      plot(Y_,prm[,ncomp],xlab='Experimental Value',ylab='Predicted Value',asp=1,
           xlim=c(min(c(Y_,prm[,ncomp],prm.tr[,ncomp])),
                  max(c(Y_,prm[,ncomp],prm.tr[,ncomp]))),
           ylim=c(min(c(Y_,prm[,ncomp],prm.tr[,ncomp])),
                  max(c(Y_,prm[,ncomp],prm.tr[,ncomp]))),type='n')
      text(Y_,prm[,ncomp],as.character(row.names(M_)))
      lines(par('usr')[1:2],par('usr')[3:4],col='red')
      grid()
      plot(1:nrow(M_),res,xlab='Object Number',ylab='Residuals',type='n',
           ylim=c(min(c(res,res.tr)),max(c(res,res.tr))))
      text(1:nrow(M_),res,as.character(row.names(M_)))
      abline(h=0,col="red")
      grid()
    }
    par(op)
  })

  output$pls_coeff_dwl <- downloadHandler(
    filename = "coeff.xlsx",
    content = function(file) {
      if(PLS$typ=='PCR')n_comp_df <- as.numeric(input$pcr_n_comp_df)
      if(PLS$typ=='PLS')n_comp_df <- as.numeric(input$pls_n_comp_df)
      df <- PLS_ext$prm[,n_comp_df]
      write.xlsx(df, file,colNames=TRUE)
    })
  

}
  



























