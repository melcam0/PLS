rm(list = ls())

colorpanel <- function (n, low, mid, high) 
{
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


server <- function (input , output, session ){
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Autori:",size = 's',easyClose = TRUE,footer = NULL,
                  
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
    dati$nr=NULL
    dati$var=NULL
    dati$var_nr=NULL
    dati$var_qt=NULL
    var_qt_sup=NULL
    dati$var_ql=NULL
    dati$righe=NULL
    dati$righe_rest=NULL
    dati$righe_tolte=NULL
    dati$var_gr=NULL
    
    dati_ext$DS=NULL
    dati_ext$DS_nr=NULL
    dati_ext$nr=NULL

    PCA$res=NULL
    PCA$dataset=NULL
    PCA$center=NULL
    PCA$scale=NULL
    PCA$centered=NULL
    PCA$scaled=NULL
    PCA$sgt=NULL
    PCA$type=NULL
    PCA$T2=NULL
    PCA$Q=NULL
    
    PCA$d_T2=NULL  
    PCA$d_Q=NULL
    PCA$c_T2=NULL
    PCA$c_Q=NULL
    
    PCA$rnd=NULL
    
    PCA_miss$res=NULL
    PCA_miss$DS_rec=NULL
    
    PCA_ext$scores=NULL
    PCA_ext$T2=NULL
    PCA_ext$Q=NULL
    
    graf$xlim=NULL
    graf$xylim=NULL
    graf$xvar_gr=NULL
    graf$xgr=NULL

    reset("lista_esempi")
    reset("file_xlsx")
    reset("file_csv")
  })
  

# reactiveValues ----------------------------------------------------------
  
  dati<-reactiveValues(DS=NULL,DS_nr=NULL,DS_righe=NULL,nr=NULL,var=NULL,var_nr=NULL,
                       var_qt=NULL,var_qt_sup=NULL,var_ql=NULL,righe=NULL,righe_rest=NULL,righe_tolte=NULL,
                       var_gr=NULL)
  
  dati_ext<-reactiveValues(DS=NULL,DS_nr=NULL,nr=NULL)
  
  
  PCA <- reactiveValues(res=NULL,dataset=NULL,center=NULL,scale=NULL,centered=NULL,scaled=NULL,sgt=NULL,type=NULL,
                        T2=NULL,Q=NULL,d_T2=NULL,d_Q=NULL,c_T2=NULL,c_Q=NULL,rnd=NULL)
  PCA_miss<-reactiveValues(res=NULL,DS_rec=NULL)
  PCA_ext <- reactiveValues(scores=NULL,T2=NULL,Q=NULL)

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
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)},
      error = function(e) {
        stop(safeError(e))
      }
    )
    } else {
      dati$DS<-NULL
      dati$DS_nr=NULL
      dati$DS_righe=NULL
      dati$var=NULL
      dati$var_nr=NULL
      dati$var_qt=NULL
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
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
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
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)},
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
      df <- tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE),
                   error = function(e) "Selezionare un dataset!")
      df <- type.convert(df)
      dati$DS<-as.data.frame(df)
      dati$DS_nr<-as.data.frame(df)
      dati$DS_righe<-as.data.frame(df)
      dati$righe<-row.names(df)
      dati$righe_rest<-row.names(df)
      dati$var<-colnames(df)
      dati$var_nr<-colnames(df)
      dati$var_qt<-colnames(df)
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
    checkboxGroupInput(inputId = "var_ql",label = "seleziona le variabili supplementari",
                       choices = dati$var,selected =dati$var_ql)
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
      "Non ci sono variabili quantitative"
    }
  })
  

# variabile nomi righe ---------------------------------------------------

  output$var_nomi<-renderUI({
    selectizeInput(inputId = "var_nr"," ",
                       choices = dati$var_nr,
                   options = list(
                     placeholder = 'Selezione eventuale colonna nomi righe',
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
      "Non c'è colonna nomi righe "
    } else {
      dati$nr
      }
    })
  

# oggetti  ------------------------------------------------
  
  output$righe_tolte<-renderUI({
    checkboxGroupInput(inputId = "righe_tolte",label = "seleziona le righe da cancellare",
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
     "Non ci sono righe cancellate"
    }
  })
  
  observeEvent(input$desel_righe,{
    dati$DS<-dati$DS_righe
    dati$righe_tolte<-NULL
    dati$righe_rest<-dati$righe
  })
  
  

# PCA ---------------------------------------------------------------------

# PCA - modello -----------------------------------------------------------

  output$n_comp<-renderUI({
    req(dati$var_qt)
    selectInput("n_comp", label = "Max. number of pricipal components", 
                choices = c(2:length(dati$var_qt)), 
                selected = length(dati$var_qt))
      })
  
  output$n_comp_varmax<-renderUI({
    req(dati$var_qt)
    req(input$pca_type=='varmax')
    selectInput("n_comp_varmax", label = "N° comp. varimax", 
                choices = c(2:input$n_comp), 
                selected = 1)
  })
 
  
  observeEvent(input$bpcamodel,{
    validate(need(nrow(dati$DS)!=0,""))
    if(sum(apply(dati$DS[,dati$var_qt],2,'is.numeric'))!=ncol(dati$DS[,dati$var_qt])){
        sendSweetAlert(session, title = "Input Error",
                       text = 'Le variabili qualitative devono essere selezionate come supplementari!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      require(pcaMethods)
      ans <- list()
      ans[[2]] <- 'all';ans[[3]] <- 'all'
      ans[[4]] <- input$n_comp
      ans[[5]] <- input$pca_center
      ans[[6]] <- input$pca_scale

      M_ <- as.data.frame(dati$DS[,dati$var_qt])

      if((typeof(M_)=='double')|(typeof(M_)=='list')){
        nNA<-sum(is.na(M_))
        if(nNA>0){
          mess<-paste(
            as.character(nNA),
            'missing data present - This is a WARNING message, not an error message')
          showNotification(mess)}

        ncom<-as.numeric(ans[[4]])

        validate(need(!(ncom>ncol(M_))|(ncom<1),'Wrong component number !'))

        sgt<-as.integer(ans[[4]])
        if(!ans[[6]])sgt<-sum(apply(M_,2,var))
        ccs<-'none';if(ans[[6]])ccs<-'uv'
        md<-prep(M_,scale=ccs,center=ans[[5]],simple=FALSE,rev=FALSE)
        res<-pca(md$data,method="nipals",nPcs=as.numeric(ans[[4]]),scale=ccs,center=ans[[5]])

        PCA$res<-res
        PCA$dataset<-prep(PCA$res@completeObs,scale=md$scale,center=md$center,reverse=TRUE)
        PCA$dataset<-as.data.frame(PCA$dataset)
        PCA$center<-ans[[5]]
        PCA$scale<-ans[[6]]
        PCA$centered<-md$center
        PCA$scaled<-md$scale
        PCA$sgt<-sgt
        PCA$type<-'pca'

        if(input$pca_type=='varmax'){
          ncomp <- as.numeric(input$n_comp_varmax)
          prl<-t(PCA$res@loadings[,1:ncomp])
          go<-1
          while(go==1){
            for(i in 1:(as.numeric(ncomp)-1)){
              for(j in (i+1):ncomp){
                lo<-prl[c(i,j),]
                rotb<-0
                sim<-sum(lo^4)
                simmax<-sim
                for (rot in seq(-90,90,0.1)){
                  rm<-c(cos(rot*pi/180),-sin(rot*pi/180),sin(rot*pi/180),cos(rot*pi/180))
                  rm<-matrix(rm,2,2)
                  lo2<-rm%*%lo
                  sim2<-sum(lo2^4)
                  if(sim2>simmax){
                    lob<-lo2
                    simmax<-sim2
                    rotb<-rot}}
                if(rotb!=0){
                  go<-1
                  prl[i,]<-lob[1,]
                  prl[j,]<-lob[2,]}else{go<-0}
              }
            }
          }

          prs<-PCA$res@completeObs%*%t(prl)
          vp<-apply(prs^2,2,sum)/sum(apply(PCA$res@completeObs^2,2,sum))
          ivp<-sort(vp,decreasing=TRUE,index.return=TRUE)$ix
          vp<-sort(vp,decreasing=TRUE,index.return=TRUE)$x
          PCA$res@loadings<-PCA$res@loadings[,1:ncomp]
          PCA$res@scores<-PCA$res@scores[,1:ncomp]
          name.pca<-colnames(PCA$res@loadings)
          PCA$res@loadings<-t(prl[ivp,])
          PCA$res@scores<-prs[,ivp]
          name.pca<-gsub("PC", "Factor ", name.pca)
          colnames(PCA$res@loadings)<-name.pca
          colnames(PCA$res@scores)<-name.pca
          names(vp)<-name.pca
          PCA$res@nPcs<-ncomp
          PCA$res@R2<-vp
          PCA$res@sDev<-sqrt(PCA$res@R2)
          PCA$res@R2cum<-cumsum(PCA$res@R2)
          PCA$type<-'varimax'
        }
      }
    }
    })

output$model_out <- renderPrint({
  validate(need(nrow(dati$DS)!=0,"Caricare un dataset!"))
  validate(need(!is.null(PCA$res),"Eseguire il modello!"))
  V<-data.frame(PCA$res@R2,PCA$res@R2cum)
  colnames(V)<-c('% Var.expl.','% Var. cum. expl.')
  rownames(V)<-attr(PCA$res@sDev,'names')
  round(t(V*100),2)
})

output$pca_expvar_dwl <- downloadHandler(
  filename = "expl_variance.xlsx", 
  content = function(file) {
    df<-rbind.data.frame(PCA$res@sDev^2,PCA$res@R2*100,PCA$res@R2cum*100)
    colnames(df)<-attr(PCA$res@sDev^2,'names')
    df <- cbind.data.frame(' '= c('expvar','percexpvar','cpercexpvar'),df)
    write.xlsx(df, file,colNames=TRUE)
  })

output$pca_var_type<-renderUI({
  req(!is.null(PCA$res))
  radioButtons("pca_var_type", "Plot",choices = c(Scree = "scree", Cumulative= "cum"), selected = "scree", inline=TRUE)
})

output$scree_plot<-renderPlot({
  
  req(!is.null(PCA$res))
  req(input$pca_var_type)

  require(ggplot2)
  df<-data.frame(x=1:PCA$res@nPcs, y=PCA$res@R2*100)
  if(input$pca_var_type=="scree")gg<-ggplot(df, aes(x, y))+ggtitle("% Explained Variance")
  if(input$pca_var_type=="cum")gg<-ggplot(df, aes(x, y=cumsum(y)))+ggtitle("Cumulative Explained Variance")
  gg<- gg+ geom_point(colour='red', size = 2)+geom_line(colour='blue')+theme_light()
  # gg<- gg+ scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  gg<- gg+ scale_x_continuous(breaks = 1:PCA$res@nPcs)
  gg<- gg+ xlab("Component Number")
  gg<- gg+ ylab("% Explained Variance")
  # girafe(code = print(gg))
  gg <- gg+theme(aspect.ratio=1,axis.text = element_text(size = 10),axis.title = element_text(size = 15))
  print(gg)
})


# PCA - scores plots -------------------------------------------------------

output$pca_score_compx <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_score_compx", label = "Component on x-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 1)
})

output$pca_score_compy <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_score_compy", label = "Component on y-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 2)
})

output$pca_score_compz <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_score_type=='3d')
  selectInput("pca_score_compz", label = "Component on z-axis", 
              choices = c(1:PCA$res@nPcs), 
              selected = 3)
})

output$pca_score_line <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_score_type=='2d')
  checkboxInput("pca_score_line", label = "Line", value = FALSE)
})

output$pca_score_label <- renderUI({
  req(!is.null(PCA$res))
  req(!is.null(dati$var_ql))
  req(input$pca_radio_score_type=='2d')
  pickerInput("pca_score_label", label = "Label variable",
              choices = dati$var_ql,
              options =  list(
                "max-options" = 1,
                "max-options-text" = "No more!"
              ),
              multiple = TRUE)
})

output$pca_score_col <- renderUI({
  req(!is.null(PCA$res))
  req(!is.null(dati$var_ql))
  pickerInput("pca_score_col", label = "Color variable",
              choices = dati$var_ql,
              options =  list(
                "max-options" = 1,
                "max-options-text" = "No more!"
              ),
              multiple = TRUE)
})

output$pca_score_chull <- renderUI({
  req(!is.null(PCA$res))
  req(!is.null(dati$var_ql))
  req(input$pca_radio_score_type=='2d')
  pickerInput("pca_score_chull", label = "Convex Hull variable",
              choices = dati$var_ql,
              options =  list(
                "max-options" = 1,
                "max-options-text" = "No more!"
              ),
              multiple = TRUE)
})

output$pca_score_col_fatt <- renderUI({
  req(!is.null(input$pca_score_col))
  req(is.numeric(dati$DS[,input$pca_score_col]))
  checkboxInput("pca_score_col_fatt", label = "Color variable is qualitative", value = FALSE)
})

output$pca_score_chull_fatt <- renderUI({
  req(!is.null(input$pca_score_chull))
  req(is.numeric(dati$DS[,input$pca_score_chull]))
  checkboxInput("pca_score_chull_fatt", label = "Convex Hull variable is qualitative", value = FALSE)
})

output$pca_score_ell <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_score_type=='2d')
  checkboxInput("pca_score_ell", label = "Ellipses", value = FALSE)
})

output$pca_score_rnames <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_score_type=='2d')
  checkboxInput("pca_score_rnames", label = "Row names", value = FALSE)
})

output$scores_pl <- renderPlot({
  req(!is.null(PCA$res))
  require(lattice)
  require(latticeExtra)
  if(input$pca_radio_score_type=='2d'){
    req(!is.null(input$pca_score_rnames))
    req(!is.null(input$pca_score_ell))
    req(!is.null(input$pca_score_line))
    require(lattice)
    
    lab<-input$pca_score_label
    if(is.null(input$pca_score_label))lab <- 'None'
    col<-input$pca_score_col
    if(is.null(input$pca_score_col))col <- 'None'
    c_hull<-input$pca_score_chull
    if(is.null(input$pca_score_chull))c_hull <- 'None'
    if(is.null(input$pca_score_col_fatt)) col.fatt <- FALSE
    if(!is.null(input$pca_score_col_fatt)) col.fatt <- input$pca_score_col_fatt
    if(is.null(input$pca_score_chull_fatt)) chull.fatt <- FALSE
    if(!is.null(input$pca_score_chull_fatt)) chull.fatt <- input$pca_score_chull_fatt
    
    ans <- list()
    ans[[1]] <- as.numeric(input$pca_score_compx)
    ans[[2]] <- as.numeric(input$pca_score_compy)
    ans[[3]] <- lab
    ans[[4]] <- col
    ans[[5]] <- input$pca_score_rnames
    ans[[6]] <- input$pca_score_ell
    ans[[7]] <- input$pca_score_line
    
    c1<-as.numeric(ans[[1]])
    c2<-as.numeric(ans[[2]])
    tex<-NULL;grade<-NULL
    if(as.logical(ans[[5]]))tex<-rownames(PCA$dataset)
    if(ans[[5]])tex<-rownames(PCA$dataset)
    if(as.character(ans[[3]])!='None')tex<-dati$DS[,lab]
    if(as.character(ans[[4]])!='None')grade<-dati$DS[,col]
    if(col.fatt)grade<-as.factor(dati$DS[,col])
    
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
    }
    S<-PCA$res@scores
    V<-PCA$res@R2
    r<-nrow(S)
    siz=.9-log10(r)/10 # defines the size of the characters in the plots, based on the number of samples
    
    c<-nrow(PCA$res@loadings)
    if(!PCA$scale)c <- sum(apply(PCA$dataset,2,'var'))
    
    DeltaS1lim=(max(S[,c1])-min(S[,c1]))
    DeltaS2lim=(max(S[,c2])-min(S[,c2]))
    
    if (DeltaS1lim>DeltaS2lim){
      Delta<-DeltaS1lim-DeltaS2lim
      S1lim<-c(min(S[,c1])-DeltaS1lim*0.05,max(S[,c1])+DeltaS1lim*0.05)
      S2lim<-c(min(S[,c2])-Delta/2-DeltaS1lim*0.05,max(S[,c2])+Delta/2+DeltaS1lim*0.05)
    }
    if (DeltaS2lim>DeltaS1lim){
      Delta<-DeltaS2lim-DeltaS1lim
      S1lim<-c(min(S[,c1])-Delta/2-DeltaS2lim*0.05,max(S[,c1])+Delta/2+DeltaS2lim*0.05)
      S2lim<-c(min(S[,c2])-DeltaS2lim*0.05,max(S[,c2])+DeltaS2lim*0.05)
    }
    
    if(PCA$type=='pca'){
      xl<-paste('Component ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'% of variance)',sep='')
      yl<-paste('Component ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'% of variance)',sep='')
    }else{
      xl<-paste('Factor ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'% of variance)',sep='')
      yl<-paste('Factor ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'% of variance)',sep='')}
    tl=paste('Score Plot (',as.character(round((V[c1]+V[c2])*100,1)),'% of total variance)',sep='')
    
    if(!is.null(grade)){
      if(tog=="double" | tog=="integer"){
        tl=paste('Score Plot (',as.character(round((V[c1]+V[c2])*100,1)),'% of total variance) \n color scale: ',col,sep='')
        # if(!variable$control==1)tl=paste('Score Plot (',as.character(round((V[c1]+V[c2])*100,1)),'% of total variance) \n color scale: ',
        #                                  colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))],sep='')
      }
    }
    
    panel.score<-function(x,y,...){
      panel.xyplot(x,y,...)
      panel.grid(h=-1, v=-1,lty = 3,col = "grey80")
      panel.text(x,y,...) 
      panel.text(0,0,'+',cex=1.2,col='red')
    }
    
    panel.score.ell<-function(x,y,e1,e2,r,c,...){
      panel.xyplot(x,y,...)
      panel.grid(h=-1, v=-1,lty = 3,col = "grey80")
      panel.text(x,y,...) 
      panel.text(0,0,'+',cex=1.2,col='red')
      
      rad1=sqrt((e1*((r-1)/r)*c)*qf(.95,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((e2*((r-1)/r)*c)*qf(.95,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='')
      
      rad1=sqrt((e1*((r-1)/r)*c)*qf(.99,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((e2*((r-1)/r)*c)*qf(.99,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      panel.xyplot(x, y, type = "l",col='red',xlab='',ylab='',lty=2)
      
      rad1=sqrt((e1*((r-1)/r)*c)*qf(.999,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((e2*((r-1)/r)*c)*qf(.999,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='',lty=3)
      
      rm(rad1,rad2,theta,x,y)
    }
    
    if(is.null(tex) & is.null(grade)){
      if(!as.logical(ans[[6]])){
        .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,
                    pty='o',xlab=xl,ylab=yl,main=tl,col='black',cex=siz,labels=NULL,
                    panel=panel.score)
        if(ans[[7]]).G_<-update(.G_,type='b')
        .G_<-update(.G_,asp=1)
        # print(.G_)
      } else {
        .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,
                    pty='o',xlab=xl,ylab=yl,main=tl,col='black',cex=siz,
                    labels=NULL,panel=panel.score.ell,
                    e1=V[c1],e2=V[c2],r=r,c=c,
                    sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                    par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
        if(ans[[7]]).G_<-update(.G_,type='b')
        .G_<-update(.G_,asp=1)
        # print(.G_)
      }
    }
    
    if(!is.null(tex)& is.null(grade)){
      if(!as.logical(ans[[6]])){
        .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,
                    type='n',
                    labels=tex,cex=siz,panel=panel.score)
        if(ans[[7]]).G_<-update(.G_,type='l')
        .G_<-update(.G_,asp=1)
        # print(.G_)
      } else{
        .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,
                    type='n',
                    labels=tex,cex=siz,panel=panel.score.ell,
                    e1=V[c1],e2=V[c2],r=r,c=c,
                    sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                    par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
        if(ans[[7]]).G_<-update(.G_,type='l')
        .G_<-update(.G_,asp=1)
        # print(.G_)
      }
    }
    
    if(is.null(tex)& !is.null(grade)){
      if(tog=="character" | tog=="factor"){
        if(!as.logical(ans[[6]])){
          .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,
                      col=vcolor[grade],pch=19,cex=siz,labels=NULL,
                      key=list(columns=min(nl,4),cex=0.8,text=list(lev),
                               points=list(pch=19,col=vcolor)),panel=panel.score)
          if(ans[[7]]).G_<-update(.G_,type='b')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        } else {
          .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,
                      col=vcolor[grade],pch=19,cex=siz,labels=NULL,
                      key=list(columns=min(nl,4),cex=0.8,text=list(lev),
                               points=list(pch=19,col=vcolor)),panel=panel.score.ell,
                      e1=V[c1],e2=V[c2],r=r,c=c,
                      sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                      par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
          if(ans[[7]]).G_<-update(.G_,type='b')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        }
      }
      
      if(tog=="double" | tog=="integer"){
        
        panel.levelplot.points<-function (x, y, z, subscripts = TRUE, at = pretty(z), shrink, 
                                          labels, label.style, contour, region, pch = 21, col.symbol = "#00000044", 
                                          ..., col.regions = regions$col, fill = NULL){
          regions <- trellis.par.get("regions")
          zcol <- level.colors(z, at, col.regions, colors = TRUE)
          x <- x[subscripts]
          y <- y[subscripts]
          zcol <- zcol[subscripts]
          panel.xyplot(x, y, fill = zcol, pch = pch, col.symbol = col.symbol, 
                       ...)
          panel.grid(h=-1, v=-1,lty = 3,col = "grey")
          panel.text(0,0,'+',cex=1.2,col='red')
        }
        
        
        panel.levelplot.points.ell<-function (x, y, z,e1,e2,cl,row, subscripts = TRUE, at = pretty(z), shrink, 
                                              labels, label.style, contour, region, pch = 21, col.symbol = "#00000044", 
                                              col.regions = regions$col, fill = NULL,...){
          regions <- trellis.par.get("regions")
          zcol <- level.colors(z, at, col.regions, colors = TRUE)
          x <- x[subscripts]
          y <- y[subscripts]
          zcol <- zcol[subscripts]
          panel.xyplot(x, y, fill = zcol, pch = pch, col.symbol = col.symbol, 
                       ...)
          panel.grid(h=-1, v=-1,lty = 3,col = "grey")
          panel.text(0,0,'+',cex=1.2,col='red')
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.95,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.95,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='')
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.99,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.99,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x, y, type = "l",col='red',xlab='',ylab='',lty=2)
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.999,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.999,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='',lty=3)
          
          rm(rad1,rad2,theta,x,y)
        }
        
        if(!as.logical(ans[[6]])){
          
          .G_<-levelplot(dati$DS[,col] ~S[,c1]*S[,c2],xlim=S1lim,ylim=S2lim,
                         xlab=xl,ylab=yl,main=tl,col=vcolor[grade],pch=19,cex=siz,labels=NULL,
                         panel = panel.levelplot.points, col.regions = colorpanel(256,low = "blue",high = "red"),
                         at=seq(min(as.numeric(lev)),max(as.numeric(lev)),length.out=256),
                         #colorkey=list(labels=list(at=seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                     (max(as.numeric(lev)-min(as.numeric(lev))))/4),
                         #                          labels=round(seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                                 (max(as.numeric(lev)-min(as.numeric(lev))))/4),2)))
          )
          if(ans[[7]]).G_<-update(.G_,type='b')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        } else {
          .G_<-levelplot(dati$DS[,col] ~S[,c1]*S[,c2],
                         e1=V[c1],e2=V[c2],row=r,cl=c,
                         xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,col=vcolor[grade],pch=19,cex=siz,
                         panel = panel.levelplot.points.ell, col.regions = colorpanel(256,low = "blue",high = "red"),
                         at=seq(min(as.numeric(lev)),max(as.numeric(lev)),length.out=256),
                         #colorkey=list(labels=list(at=seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                       (max(as.numeric(lev)-min(as.numeric(lev))))/4),
                         #                          labels=round(seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                                  (max(as.numeric(lev)-min(as.numeric(lev))))/4),2))),
                         sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                         par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
          if(ans[[7]]).G_<-update(.G_,type='b')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        }
        if (exists("panel.levelplot.points")) rm(panel.levelplot.points)
        if (exists("panel.levelplot.points.ell")) rm(panel.levelplot.points.ell)
      }
      rm(lev,nl,vcolor)
    }
    
    if(!is.null(tex)& !is.null(grade)){
      if(tog=="character" | tog=="factor"){
        if(!as.logical(ans[[6]])){
          .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,labels=tex,
                      main=tl,col=vcolor[grade],type='n',cex=siz,panel=panel.score,
                      key=list(columns=min(nl,4),cex=0.8,text=list(lev),points=list(pch=19,col=vcolor)))
          if(ans[[7]]).G_<-update(.G_,type='l')
          .G_<-update(.G_,asp=1);print(.G_)
        } else {
          .G_<-xyplot(S[,c2]~S[,c1],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,labels=tex,
                      main=tl,col=vcolor[grade],type='n',cex=siz,panel=panel.score.ell,
                      key=list(columns=min(nl,4),cex=0.8,text=list(lev),points=list(pch=19,col=vcolor)),
                      e1=V[c1],e2=V[c2],r=r,c=c,
                      sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                      par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
          if(ans[[7]]).G_<-update(.G_,type='l')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        }
      }
      
      if(tog=="double" | tog=="integer"){
        panel.levelplot.points<-function (x, y, z, subscripts = TRUE, at = pretty(z), shrink, 
                                          labels, label.style, contour, region, pch = 21, col.symbol = "#00000044", 
                                          ..., col.regions = regions$col, fill = NULL){
          regions <- trellis.par.get("regions")
          zcol <- level.colors(z, at, col.regions, colors = TRUE)
          x <- x[subscripts]
          y <- y[subscripts]
          zcol <- zcol[subscripts]
          panel.xyplot(x, y, fill = zcol, pch = pch, col.symbol = col.symbol, 
                       ...)
          panel.text(x,y,col=zcol,labels,...)
          panel.grid(h=-1, v=-1,lty = 3,col = "grey")
          panel.text(0,0,'+',cex=1.2,col='red')
        }
        
        panel.levelplot.points.ell<-function (x, y, z, e1,e2,row,cl, subscripts = TRUE, at = pretty(z), shrink, 
                                              labels, label.style, contour, region, pch = 21, col.symbol = "#00000044", 
                                              ..., col.regions = regions$col, fill = NULL){
          regions <- trellis.par.get("regions")
          zcol <- level.colors(z, at, col.regions, colors = TRUE)
          x <- x[subscripts]
          y <- y[subscripts]
          zcol <- zcol[subscripts]
          panel.xyplot(x, y, fill = zcol, pch = pch, col.symbol = col.symbol, 
                       ...)
          panel.text(x,y,col=zcol,labels,...)
          panel.grid(h=-1, v=-1,lty = 3,col = "grey")
          panel.text(0,0,'+',cex=1.2,col='red')
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.95,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.95,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='')
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.99,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.99,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x, y, type = "l",col='red',xlab='',ylab='',lty=2)
          
          rad1=sqrt((e1*((row-1)/row)*cl)*qf(.999,2,row-2)*2*(row^2-1)/(row*(row-2))); 
          rad2=sqrt((e2*((row-1)/row)*cl)*qf(.999,2,row-2)*2*(row^2-1)/(row*(row-2)));
          theta <- seq(0, 2 * pi, length=1000)
          x <- rad1 * cos(theta)
          y <- rad2 * sin(theta)
          panel.xyplot(x,y, type = "l",col='red',xlab='',ylab='',lty=3)
          
          rm(rad1,rad2,theta,x,y)
        }
        
        if(!as.logical(ans[[6]])){
          .G_<-levelplot(dati$DS[,col] ~S[,c1]*S[,c2],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,col=vcolor[grade],type='n',cex=siz,
                         panel = panel.levelplot.points, col.regions = colorpanel(256,low = "blue",high = "red"),labels=tex,
                         at=seq(min(as.numeric(lev)),max(as.numeric(lev)),length.out=256),
                         #colorkey=list(labels=list(at=seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                    (max(as.numeric(lev)-min(as.numeric(lev))))/4),
                         #                          labels=round(seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                                   (max(as.numeric(lev)-min(as.numeric(lev))))/4),2)))
          )
          if(ans[[7]]).G_<-update(.G_,type='l')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        } else {
          .G_<-levelplot(dati$DS[,col] ~S[,c1]*S[,c2],xlim=S1lim,ylim=S2lim,xlab=xl,ylab=yl,main=tl,col=vcolor[grade],type='n',cex=siz,
                         panel = panel.levelplot.points.ell, col.regions = colorpanel(256,low = "blue",high = "red"),
                         at=seq(min(as.numeric(lev)),max(as.numeric(lev)),length.out=256),
                         #colorkey=list(labels=list(at=seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                      (max(as.numeric(lev)-min(as.numeric(lev))))/4),
                         #                          labels=round(seq(min(as.numeric(lev)),max(as.numeric(lev)),
                         #                                                 (max(as.numeric(lev)-min(as.numeric(lev))))/4),2))),
                         e1=V[c1],e2=V[c2],row=r,cl=c,labels=tex,
                         sub="Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001",
                         par.settings = list(par.sub.text = list(cex = 0.6,col = "red")))
          if(ans[[7]]).G_<-update(.G_,type='l')
          .G_<-update(.G_,asp=1)
          # print(.G_)
        }
        if (exists("panel.levelplot.points")) rm(panel.levelplot.points)
        if (exists("panel.levelplot.points.ell")) rm(panel.levelplot.points.ell)
      }
      # rm(lev,nl,vcolor)
    }

    if(as.character(c_hull)=='None')print(.G_)
    if(as.character(c_hull)!='None'){
      # variable<-makevar(chull)   
      grade<-dati$DS[,c_hull]
      if(chull.fatt)grade<-as.factor(dati$DS[,c_hull])
      # grade[is.na(grade)]='-'
      # grade[grade=='']='-'
      if(!is.null(grade)){
        tog<-typeof(grade)
        if(is.factor(grade))tog<-"factor"
        grade<-factor(grade)
        lev<-levels(grade)
        # if(tog=="factor")vcolor<-unlist(dovc(as.character(lev)))
        # if(tog=="character")vcolor<-unlist(dovc(as.character(lev)))
        if(tog=="factor"|tog=="character"){
          vcolor<-unlist(dovc(as.character(lev)))
          vcolor[lev=='-']="#00000000"
          if(ans[4]!='None'& ans[4][[1]]==ans[5][[1]]){
          # if(ans[4]!='None'){
            # variable<-makevar(ans[[4]])   
            grade<-dati$DS[,col]
            tog <- typeof(grade)
            if(tog=='factor'|tog=='character'){
              grade<-factor(grade)
              vcolor<-unlist(dovc(as.character(levels(grade))))
              vcolor[!levels(grade)%in%lev]="#00000000"
              lev<-levels(grade)
            }
          }
          
          i=0
          A<-".G_"
          len <- length(lev)
          S_L <- vector(mode = "list", length = len)
          CH <- vector(mode = "list", length = len)
          for(l in lev){
            i=i+1
            S_L[[i]]<-S[dati$DS[,c_hull]==l,c(c1,c2)]
            if(!is.matrix(S_L[[i]]))
              next
            hpts <- chull(S_L[[i]])
            hpts<- c(hpts, hpts[1])
            CH[[i]] <- hpts
            A<-paste0(A,"+xyplot(S_L[[",i,"]][CH[[",i,"]],2]~S_L[[",i,"]][CH[[",i,"]],1],type='l',col=vcolor[",which(l==lev),"])")
          }
          print(eval(parse(text=A)))
        }else{
          print(.G_)
        }
      }}
  }
  
  if(input$pca_radio_score_type=='3d'){
    # require(lattice)
    req(input$pca_score_compx,input$pca_score_compy,input$pca_score_compz)
    req(input$scores_pl3d_lv_z,input$scores_pl3d_lv_x)
    
    c1<-as.numeric(input$pca_score_compx)
    c2<-as.numeric(input$pca_score_compy)
    c3<-as.numeric(input$pca_score_compz)
    
    col<-input$pca_score_col
    if(is.null(input$pca_score_col))col <- 'None'
    grade<-NULL
    if(col!='None')grade<-dati$DS[,col]
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
    }
    S<-PCA$res@scores
    V<-PCA$res@R2
    r<-nrow(S)
    siz=.9-log10(r)/10 # defines the size of the characters in the plots, based on the number of samples
    c<-nrow(PCA$res@loadings)
    
    DeltaS1lim=0.01*(max(S[,c1])-min(S[,c1]))
    S1lim<-c(min(S[,c1])-DeltaS1lim,max(S[,c1])+DeltaS1lim)
    DeltaS2lim=0.01*(max(S[,c2])-min(S[,c2]))
    S2lim<-c(min(S[,c2])-DeltaS2lim,max(S[,c2])+DeltaS2lim)
    DeltaS3lim=0.01*(max(S[,c3])-min(S[,c3]))
    S3lim<-c(min(S[,c3])-DeltaS3lim,max(S[,c3])+DeltaS3lim)
    # dev.new(title="PCA 3D score plot")
    
    m<-min(c(S1lim,S2lim,S3lim))
    M<-max(c(S1lim,S2lim,S3lim))
    
    if(PCA$type=='pca'){
      xl<-paste('Comp. ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'%)',sep='')
      yl<-paste('Comp. ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'%)',sep='')
      zl<-paste('Comp. ',as.character(c3),' (',as.character(round(V[c3]*100,1)),'%)',sep='')
    }else{
      xl<-paste('Fact. ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'%)',sep='')
      yl<-paste('Fact. ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'%)',sep='')
      zl<-paste('Fact. ',as.character(c3),' (',as.character(round(V[c3]*100,1)),'%)',sep='')}
    tl=paste('Score Plot (',as.character(round((V[c1]+V[c2]+V[c3])*100,1)),'% of total variance)',sep='')
    
    Data<-as.data.frame(S[,c(c1,c2,c3)])
    # colnames(Data)<-c("x","y","z")
    # colnames(Data)[1]<-'x'
    # colnames(Data)[2]<-'y'
    # colnames(Data)[3]<-'z'
    
    if(is.null(grade)){
      score.3D<-cloud(S[,c3]~S[,c1]*S[,c2],data = Data,screen = list(z=input$scores_pl3d_lv_z,x=-input$scores_pl3d_lv_x),
                      xlim=c(m,M),ylim=c(m,M),zlim=c(m,M),xlab=list(xl,cex=0.8),ylab=list(yl,cex=0.8),zlab=list(zl,cex=0.8),
                      main=tl,cex=siz,col="black",pch=19);print(score.3D)
    }
    if(!is.null(grade)){
      if(tog=="character" | tog=="factor"){
        score.3D<-cloud(S[,c3]~S[,c1]*S[,c2],data = Data,screen = list(z = input$scores_pl3d_lv_z, x =-input$scores_pl3d_lv_x),
                        xlim=c(m,M),ylim=c(m,M),zlim=c(m,M),xlab=list(xl,cex=0.8),ylab=list(yl,cex=0.8),zlab=list(zl,cex=0.8),
                        main=tl,cex=siz,col=vcolor[grade],pch=19,
                        key=list(columns=min(nl,4),cex=0.8,text=list(lev),points=list(pch=19,col=vcolor)));print(score.3D)
      }
      if(tog=="double" | tog=="integer"){
        
        tl=paste('Score Plot (',as.character(round((V[c1]+V[c2]+V[c3])*100,1)),'% of total variance) \n color scale: ',col,sep='')
        # if(!variable$control==1)tl=paste('Score Plot (',as.character(round((V[c1]+V[c2]+V[c3])*100,1)),'% of total variance) \n color scale: ',
        #                                  colnames(eval(parse(text=variable$name),envir=.GlobalEnv))[as.numeric(sub("]","",sub(".*\\[,", "", variable$surname)))],sep='')
        
        score.3D<-cloud(S[,c3]~S[,c1]*S[,c2],data = Data,screen = list(z = input$scores_pl3d_lv_z, x =-input$scores_pl3d_lv_x),col=vcolor[grade],
                        drape=TRUE,
                        at=seq(min(as.numeric(lev)),max(as.numeric(lev)),length.out=256),
                        #colorkey=list(labels=list(at=seq(min(as.numeric(lev)),max(as.numeric(lev)),
                        #                                 (max(as.numeric(lev)-min(as.numeric(lev))))/4),
                        #                          labels=round(seq(min(as.numeric(lev)),max(as.numeric(lev)),
                        #                                                  (max(as.numeric(lev)-min(as.numeric(lev))))/4),2))),
                        col.regions =colorpanel(256,low = "blue",high = "red"),
                        xlim=c(m,M),ylim=c(m,M),zlim=c(m,M),xlab=list(xl,cex=0.8),ylab=list(yl,cex=0.8),zlab=list(zl,cex=0.8),
                        main=tl,cex=siz,pch=19);print(score.3D)
      }
      
      # rm(lev,nl,vcolor,tog)
    }
  }
})


output$pca_score_dwl <- downloadHandler(
  filename = "scores.xlsx", 
  content = function(file) {
    df<-PCA$res@scores
    df<-cbind.data.frame(' '=rownames(PCA$dataset),df)
    write.xlsx(df, file,colNames=TRUE)
  })

output$scores_pl3d_lv_z<-renderUI({
  req(input$pca_radio_score_type=='3d')
  sliderInput('scores_pl3d_lv_z',label = 'Horizontal rotation',min = 0,max = 360,value = 30,step = 10)
})
output$scores_pl3d_lv_x<-renderUI({
  req(input$pca_radio_score_type=='3d')
  sliderInput('scores_pl3d_lv_x',label = 'Vertical rotation',min = 0,max = 90,value = 60,step = 10)
})

# PCA - loading plots -----------------------------------------------------

output$pca_load_compx <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='sca')
  selectInput("pca_load_compx", label = "Component on x-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 1)
})

output$pca_load_compy <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='sca')
  selectInput("pca_load_compy", label = "Component on y-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 2)
})

output$pca_load_rnames <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='sca')
  checkboxInput("pca_load_rnames", label = "Row names", value = FALSE)
})

output$pca_load_arrows <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='sca')
  checkboxInput("pca_load_arrows", label = "Arrows", value = FALSE)
})

output$pca_load_linecomp <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='line')
  textInput("pca_load_linecomp", label = "Components to be plotted (e.g.,1,3,5)", value = "1,2")
})

output$pca_load_compN <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='bar')
  selectInput("pca_load_compN", label = "Component number", 
              choices = 1:PCA$res@nPcs, 
              selected = 1)
})

output$pca_load_cnames <- renderUI({
  req(!is.null(PCA$res))
  req(input$pca_radio_load_type=='bar')
  checkboxInput("pca_load_cnames", label = "Column names", value = FALSE)
})

output$loading_pl <- renderPlot({
  req(!is.null(PCA$res))
  require(stringr)
  require(gplots)
  if(input$pca_radio_load_type=='sca'){
    req(!is.null(input$pca_load_rnames))
    
    ans1 <- list()
    ans1[[1]] <- as.numeric(input$pca_load_compx)
    ans1[[2]] <- as.numeric(input$pca_load_compy)
    ans1[[3]] <- 'None'
    ans1[[4]] <- input$pca_load_rnames
    ans1[[5]] <- input$pca_load_arrows
    
    op<-par(pty='s')
    n1<-as.integer(ans1[[1]])
    n2<-as.integer(ans1[[2]])
    T<-PCA$res@loadings
    V<-PCA$res@R2
    siz=.9-log10(nrow(T))/10 # defines the size of the characters in the plots, based on the number of variables
    tex<-as.character(1:nrow(T))
    # if(ans[[3]]!='None'){
    #   variable<-makevar(ans[[3]])
    #   tex<-variable$value
    # }
    if(as.logical(ans1[[4]]))tex<-row.names(T)
    Tlim<-c(min(T[,c(n1,n2)]),max(T[,c(n1,n2)]))
    Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
    
    if(PCA$type=='pca'){
      plot(T[,n1],T[,n2],xlab=paste('Component ',as.character(n1),' (',as.character(round(V[n1]*100,1)),'% of variance)',sep=''),ylab=paste('Component ',as.character(n2),' (',as.character(round(V[n2]*100,1)),'% of variance)',sep=''),
           main=paste('Loading Plot (',as.character(round((V[n1]+V[n2])*100,1)),'% of total variance)',sep=''),type='n',xlim=Tlim,ylim=Tlim)
    }else{
      plot(T[,n1],T[,n2],xlab=paste('Factor ',as.character(n1),' (',as.character(round(V[n1]*100,1)),'% of variance)',sep=''),ylab=paste('Factor ',as.character(n2),' (',as.character(round(V[n2]*100,1)),'% of variance)',sep=''),
           main=paste('Loading Plot (',as.character(round((V[n1]+V[n2])*100,1)),'% of total variance)',sep=''),type='n',xlim=Tlim,ylim=Tlim)}
    
    text(T[,n1],T[,n2],tex,cex=siz) 
    text(0,0,'+',cex=1.2,col='red')
    grid()
    if(as.logical(ans1[[5]]))
      arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,n1],T[,n2],0.1,col='red')
  }

  if(input$pca_radio_load_type=='line'){
    req(input$pca_load_linecomp)
    req(sum(is.na(as.numeric(unlist(str_split(input$pca_load_linecomp,',')))))==0)
    T<-PCA$res@loadings
    vi<-as.numeric(unlist(str_split(input$pca_load_linecomp,',')))
    plot(T[,1],ylab='Loading value',xlab='Variable number',type='n',ylim=c(min(T[,vi]),max(T[,vi])))
    grid()
    for(i in vi)lines(T[,i],col=i)
    legend("bottomleft",legend=as.character(vi),col=vi,lty=1)
    # rm(i,vi)
    abline(0,0,lty=2) 
  }

  if(input$pca_radio_load_type=='bar'){
    req(!is.null(input$pca_load_cnames))

    plotco<-function(T,c1=1,label=NULL){
      nr<-nrow(T)
      if(is.null(label))label<-as.character(1:nr)
      # dev.new(title="PCA loading plot bar")
      
      if(PCA$type=='pca'){
        barplot(T[,c1],main=paste('Loading on Component',as.character(c1),sep=' '),
                names.arg=as.character(label),cex.names=0.7,las=2,mgp=c(3, .4, 0))}
      else{
        barplot(T[,c1],main=paste('Loading on Factor',as.character(c1),sep=' '),
                names.arg=as.character(label),cex.names=0.7,las=2,mgp=c(3, .4, 0))}
      box(lty=1,col='red')
      grid()
      return()}

    ans <- list()
    ans[[1]] <- as.numeric(input$pca_load_compN)
    ans[[2]] <- input$pca_load_cnames
    
    lb<-1:PCA$res@nVar
    if(as.logical(ans[[2]]))lb<-names(as.data.frame(PCA$dataset))
    plotco(PCA$res@loadings,as.numeric(ans[[1]]),lb)
  }
})

output$pca_loading_dwl <- downloadHandler(
  filename = "loadings.xlsx", 
  content = function(file) {
    df <- PCA$res@loadings
    write.xlsx(df, file,colNames=TRUE)
  })


# PCA - biplot ------------------------------------------------------------

output$pca_biplot_compx <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_biplot_compx", label = "Component on x-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 1)
})

output$pca_biplot_compy <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_biplot_compy", label = "Component on y-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 2)
})

output$pca_biplot_rnames <- renderUI({
  checkboxInput("pca_biplot_rnames", label = "Row names", value = FALSE)
})

output$pca_biplot_cnames <- renderUI({
  checkboxInput("pca_biplot_cnames", label = "Column names", value = FALSE)
})

output$pca_biplot_arrows <- renderUI({
  checkboxInput("pca_biplot_arrows", label = "Arrows", value = FALSE)
})

output$biplot <- renderPlot({
  req(!is.null(PCA$res))
  req(!is.null(input$pca_biplot_rnames))
  req(!is.null(input$pca_biplot_cnames))
  req(!is.null(input$pca_biplot_arrows))

  ans <- list()
  ans[[1]] <- as.numeric(input$pca_biplot_compx)
  ans[[2]] <- as.numeric(input$pca_biplot_compy)
  ans[[3]] <- input$pca_biplot_rnames
  ans[[4]] <- input$pca_biplot_cnames
  ans[[5]] <- input$pca_biplot_arrows
  
  tex<-as.character(1:PCA$res@nObs)
  if(ans[[3]])tex<-rownames(PCA$dataset)
  
  c1<-as.numeric(ans[[1]])
  c2<-as.numeric(ans[[2]])
  S<-PCA$res@scores
  V<-PCA$res@R2
  Slim<-c(min(S[,c(c1,c2)]),max(S[,c(c1,c2)]))
  Slim<-c(sign(Slim[1])*max(abs(Slim)),sign(Slim[2])*max(abs(Slim)))
  
  if(PCA$type=='pca'){
    xl<-paste('Component ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'% of variance)',sep='')
    yl<-paste('Component ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'% of variance)',sep='')
  }else{
    xl<-paste('Factor ',as.character(c1),' (',as.character(round(V[c1]*100,1)),'% of variance)',sep='')
    yl<-paste('Factor ',as.character(c2),' (',as.character(round(V[c2]*100,1)),'% of variance)',sep='')}
  
  tl=paste('Biplot (',as.character(round((V[c1]+V[c2])*100,1)),'% of total variance)',sep='')
  op<-par(pty='s')
  if(is.null(tex)){
    plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,pty='o',xlab=xl,ylab=yl,col='black')
  }else{
    plot(S[,c(c1,c2)],xlim=Slim,ylim=Slim,xlab=xl,ylab=yl,type='n')
    text(S[,c(c1,c2)],as.character(tex),col='black',cex=0.7)}
  par(op)
  # draw loading arrows
  par(new=TRUE)
  T<-PCA$res@loadings
  tex<-1:nrow(T)
  if(as.logical(ans[[4]]))tex<-rownames(T)
  Tlim<-c(min(T[,c(c1,c2)]),max(T[,c(c1,c2)]))
  Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
  plot(T[,c(c1,c2)],axes=FALSE,type='n',xlim=Tlim,ylim=Tlim,pty='s',xlab=xl,ylab=yl)
  if(as.logical(ans[[5]]))arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,c1],T[,c2],col='red')
  text(T[,c1],T[,c2],as.character(tex),cex=0.7,col='red')
  axis(side=4)
  axis(side=3)
  par(new=FALSE)
  # draw centre and grid
  grid()
  text(0,0,'+',cex=1.2,col='red')
  title(main=tl,line=2.5)
})


# PCA - correlation plot --------------------------------------------------

output$pca_corr_compx <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_corr_compx", label = "Component on x-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 1)
})

output$pca_corr_compy <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_corr_compy", label = "Component on y-axis", 
              choices = 1:PCA$res@nPcs, 
              selected = 2)
})

output$pca_corr_rnames <- renderUI({
  req(!is.null(PCA$res))
  checkboxInput("pca_corr_rnames", label = "Row names", value = FALSE)
})

output$pca_corr_arrows <- renderUI({
  req(!is.null(PCA$res))
  checkboxInput("pca_corr_arrows", label = "Arrows", value = FALSE)
})

output$pca_corr_varsup <- renderUI({
  req(!is.null(PCA$res))
  req(!is.null(dati$var_ql))
  varsupqt <- c(NULL)
  for(vs in dati$var_ql){
    if(is.numeric(dati$DS[,vs]))varsupqt <- c(varsupqt,vs)
  }
  pickerInput("pca_corr_varsup", label = "Variable sup.",
              choices = varsupqt,
              multiple = TRUE)
})

output$corr_pl <- renderPlot({
  
  circle <- function(center = c(0, 0), npoints = 1000) {
    r = 1
    tt = seq(0, 2 * pi, length = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[1] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  corcir=circle()
  
  
  
  req(!is.null(PCA$res))
  # require(stringr)
  require(gplots)
  
  
  req(!is.null(input$pca_corr_rnames))
  
  ans1 <- list()
  ans1[[1]] <- as.numeric(input$pca_corr_compx)
  ans1[[2]] <- as.numeric(input$pca_corr_compy)
  ans1[[3]] <- 'None'
  ans1[[4]] <- input$pca_corr_rnames
  ans1[[5]] <- input$pca_corr_arrows
  
  op<-par(pty='s')
  n1<-as.integer(ans1[[1]])
  n2<-as.integer(ans1[[2]])
  # T<-PCA$res@loadings
  T <- sweep(PCA$res@loadings,2,PCA$res@sDev,"*")
  V<-PCA$res@R2
  siz=.9-log10(nrow(T))/10 # defines the size of the characters in the plots, based on the number of variables
  tex<-as.character(1:nrow(T))
  # if(ans[[3]]!='None'){
  #   variable<-makevar(ans[[3]])
  #   tex<-variable$value
  # }
  if(as.logical(ans1[[4]]))tex<-row.names(T)
  
  # Tlim<-c(min(T[,c(n1,n2)]),max(T[,c(n1,n2)]))
  # Tlim<-c(sign(Tlim[1])*max(abs(Tlim)),sign(Tlim[2])*max(abs(Tlim)))
  
  x.lim.inf <- -1.01;x.lim.sup <- 1.01
  y.lim.inf <- -1.01;y.lim.sup <- 1.01
  if(PCA$scale==FALSE|PCA$center==FALSE){
    x.lim.inf <- min(T[,n1]-0.01,-1.01);x.lim.sup <- max(T[,n1]+0.01,1.01)
    y.lim.inf <- min(T[,n2]-0.01,-1.01);y.lim.sup <- max(T[,n2]+0.01,1.01)
  }
  
  
  if(PCA$type=='pca'){
    plot(T[,n1],T[,n2],xlab=paste('Component ',as.character(n1),' (',as.character(round(V[n1]*100,1)),'% of variance)',sep=''),ylab=paste('Component ',as.character(n2),' (',as.character(round(V[n2]*100,1)),'% of variance)',sep=''),
         main=paste('Correlation Plot (',as.character(round((V[n1]+V[n2])*100,1)),'% of total variance)',sep=''),type='n',
         xlim=c(x.lim.inf,x.lim.sup),ylim=c(y.lim.inf,y.lim.sup),asp=1)
  }else{
    plot(T[,n1],T[,n2],xlab=paste('Factor ',as.character(n1),' (',as.character(round(V[n1]*100,1)),'% of variance)',sep=''),ylab=paste('Factor ',as.character(n2),' (',as.character(round(V[n2]*100,1)),'% of variance)',sep=''),
         main=paste('Correlation Plot (',as.character(round((V[n1]+V[n2])*100,1)),'% of total variance)',sep=''),type='n',
         xlim=c(x.lim.inf,x.lim.sup),ylim=c(y.lim.inf,y.lim.sup),asp=1)}
  
  text(T[,n1],T[,n2],tex,cex=siz) 
  text(0,0,'+',cex=1.2,col='red')
  points(x = corcir$x,y = corcir$y,cex=0.1,col='grey',type='l')
  
  grid()
  
  if(as.logical(ans1[[5]])){
    arrows(rep(0,dim(T)[1]),rep(0,dim(T)[2]),T[,n1],T[,n2],0.1,col='red')}
  
  # var sup
  
  varsup<-input$pca_corr_varsup
  if(!is.null(varsup)){
    for(vs in varsup){
      D<-NULL;S <- NULL
      if(is.numeric(dati$DS[,vs])){
        D<-dati$DS[,vs]
        S<-cor(D,PCA$res@scores,use="complete.obs")
        text(S[,n1],S[,n2],vs,cex=siz,col='blue')
        if(as.logical(ans1[[5]]))arrows(0,0,S[,n1],S[,n2],0.1,col='blue',lty = 2)
      }
      
    }
  }
})

output$pca_corr_dwl <- downloadHandler(
  filename = "correlations.xlsx", 
  content = function(file) {
    df <- sweep(PCA$res@loadings,2,PCA$res@sDev,"*")
    df<-as.data.frame(df)
    df<-cbind(' '= row.names(df),df)

    varsup<-input$pca_corr_varsup
    if(!is.null(varsup)){
      for(vs in varsup){
        # D<-NULL;S <- NULL
        if(is.numeric(dati$DS[,vs])){
          D<-dati$DS[,vs]
          S<-cor(D,PCA$res@scores,use="complete.obs")
          S<-as.data.frame(S)
          S<-cbind(' '=vs,S)
          df<-rbind.data.frame(df,S)
        }
      }
    }

    df
    write.xlsx(df, file,colNames=TRUE)
  })


# PCA - variance variable explained ----------------------------------------

output$pca_var_var_compN <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_var_var_compN", label = "Number of components",
              choices = 1:PCA$res@nPcs,
              selected = 1)
})

output$var_var_pl <- renderPlot({
  req(!is.null(PCA$res))
  req(input$pca_var_var_compN)
  require(chemometrics)
  if(PCA$res@scaled=='uv')scale<-TRUE else scale<-FALSE
  pcaVarexpl(PCA$dataset,a=as.numeric(input$pca_var_var_compN),scale=scale,center=PCA$res@centered,las=2,cex.names=0.7,mgp=c(3, .4, 0),
                          main='Variance of each Variable explained')
})

output$pca_var_var_dwl <- downloadHandler(
  filename = "variance_var.xlsx",
  content = function(file) {
    require(chemometrics)
    if(PCA$res@scaled=='uv')scale<-TRUE else scale<-FALSE
    varexp <- pcaVarexpl(PCA$dataset,a=as.numeric(input$pca_var_var_compN),scale=scale,center=PCA$res@centered,las=2,cex.names=0.7,mgp=c(3, .4, 0),
                 main='Variance of each Variable explained')
    df <- as.data.frame(varexp$ExplVar)
    colnames(df) <- 'var_expl'
    df<-cbind.data.frame(' '=rownames(df),df)
    write.xlsx(df, file,colNames=TRUE)
  })

# PCA - missing data reconstruction ---------------------------------------

output$pca_missdata_n_comp_max<-renderUI({
  req(dati$DS)
  selectInput("pca_missdata_n_comp_max", label = "Max. number of components for reconstruction", 
              choices = c(2:length(dati$var_qt)), 
              selected = length(dati$var_qt))
})

observeEvent(input$bpcamodel_miss,{
  if(sum(apply(dati$DS[,dati$var_qt],2,'is.numeric'))!=ncol(dati$DS[,dati$var_qt])){
    sendSweetAlert(session, title = "Input Error",
                   text = 'Le variabili qualitative devono essere selezionate come supplementari!',
                   type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  }else{
  require(pcaMethods)
  
  ans <- list()
  ans[[4]] <- input$pca_missdata_n_comp_max
  ans[[5]] <- input$pca_missdata_center
  ans[[6]] <- input$pca_missdata_scale
  
  M_<-as.data.frame(dati$DS[,dati$var_qt])
  M_0<-M_
  
  if((typeof(M_)=='double')|(typeof(M_)=='list')){
    # previous.name<-ans[[1]]
    M.na<-is.na(M_)
    if(sum(is.na(M_))!=0){
      sc<-"none"
      if(as.logical(ans[[6]]))sc<-"uv"
      pre<-as.logical(ans[[5]])
      npc<-min(as.numeric(ans[[4]]),dim(M_))
      res<-pca(M_,method="nipals",center=pre,scale=sc,nPcs=npc)
      PCA_miss$res <- res
    }else{
      sendSweetAlert(session, title = "Input Error",
                      text = 'No Missing Data!',
                      type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
     
      }
  }
  }
})
      
output$pca_miss_pl <- renderPlot({  
  req(!is.null(PCA_miss$res))
      # V_<-PCA_miss$res@R2*100
      require(ggplot2)
      df<-data.frame(x=1:PCA_miss$res@nPcs, y=PCA_miss$res@R2*100)
      gg<-ggplot(df, aes(x, y))+ggtitle("% Explained Variance")
      gg<- gg+ geom_point(colour='red', size = 2)+geom_line(colour='blue')+theme_light()
      # gg<- gg+ scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
      gg<- gg+ scale_x_continuous(breaks = 1:PCA_miss$res@nPcs)
      gg<- gg+ xlab("Component Number")
      gg<- gg+ ylab("% Explained Variance")
      print(gg)
})

output$pca_missdata_n_comp<-renderUI({
  req(!is.null(PCA_miss$res))
  selectInput("pca_missdata_n_comp", label = "Number of components for reconstruction",
              choices = c(2:PCA_miss$res@nPcs),
              selected = PCA_miss$res@nPcs)
})

output$bpcamodel_miss_rec <- renderUI({
  req(!is.null(PCA_miss$res))
  actionButton("bpcamodel_miss_rec", label = "Reconstruction")
})

observeEvent(input$bpcamodel_miss_rec,{
  req(!is.null(PCA_miss$res))
  req(input$pca_missdata_n_comp)
  
  M_<-as.data.frame(dati$DS[,dati$var_qt])
  M.na<-is.na(M_)

  npc<-input$pca_missdata_n_comp
  pre<-input$pca_missdata_center
  
  M.rec<-fitted(PCA_miss$res,nPcs=npc,pre=pre,post=TRUE)
  
  M.rec[!M.na]<-M_[!M.na]
  M.rec<-as.data.frame(M.rec)
  names(M.rec)<-names(M_)
  row.names(M.rec)<-row.names(M_)
  
  PCA_miss$DS_rec <- M.rec

})

output$pca_missdata_cmpl <- renderPrint({
  req(!is.null(PCA_miss$res))
  head(PCA_miss$DS_rec)
})

output$pca_missdata_dwl <- downloadHandler(
  filename = "completed.xlsx",
  content = function(file) {
    df <- as.data.frame(PCA_miss$DS_rec)
    df<-cbind.data.frame(' '=rownames(df),df)
    write.xlsx(df, file,colNames=TRUE)
  })


# PCA - randomization test ------------------------------------------------

observeEvent(input$brnd_test,{
  if(is.null(PCA$res)){
    sendSweetAlert(session, title = "Input Error",
                   text = 'Run PCA Model First!',
                   type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
  }else{
    require(pcaMethods)
    set.seed(as.numeric(Sys.time()))
    nr <- dim(PCA$dataset)[1];nc <- dim(PCA$dataset)[2]
    n <- as.numeric(input$rnd_test_N)
    var_rnd <- data.frame(matrix(rep(0,n*as.numeric(PCA$res@nPcs)),nrow = n))
    withProgress(message = 'Randomization:',value = 0, {
    for(i in 1:n){
      incProgress(detail = paste("NumRnd", i),amount = 1/n)
      M_ <- data.frame(matrix(rnorm(n = nr*nc),nrow = nr))
      sgt<-as.integer(PCA$res@nPcs)
      if(!PCA$scale)sgt<-sum(apply(M_,2,var))
      ccs<-'none';if(PCA$scale)ccs<-'uv'
      md<-prep(M_,scale=ccs,center=PCA$center,simple=FALSE,rev=FALSE)
      res<-pca(md$data,method="nipals",nPcs=as.numeric(PCA$res@nPcs),scale=ccs,center=PCA$center)
      var_rnd[i,] <- res@R2*100
    }
    })
  PCA$rnd <- var_rnd
  }
})

output$rnd_test_pl <- renderPlot({
  req(!is.null(PCA$rnd))
  n <- as.numeric(input$rnd_test_N)
  var_rnd_mean <- apply(PCA$rnd,2,FUN = 'mean')
  var_rnd_sd <- apply(PCA$rnd,2,FUN = 'sd')
  var_rnd_ic_sup <- var_rnd_mean+qt(p = 0.975,df = n-1)*var_rnd_sd*(1+1/sqrt(n)) # rispetto CAT ho messo *(1+1/sqrt(n))
  var_rnd_ic_inf <- var_rnd_mean-qt(p = 0.975,df = n-1)*var_rnd_sd*(1+1/sqrt(n))
  require(ggplot2)
  df<-data.frame(x=1:PCA$res@nPcs, y=PCA$res@R2*100)
  if(input$pca_var_type=="scree")gg<-ggplot(df, aes(x, y))+ggtitle("% Explained Variance")
  if(input$pca_var_type=="cum")gg<-ggplot(df, aes(x, y=cumsum(y)))+ggtitle("Cumulative Explained Variance")
  gg<- gg+ geom_point(colour='red', size = 2)+geom_line(colour='blue')+theme_light()
  gg<- gg+ scale_x_continuous(breaks = 1:PCA$res@nPcs)
  gg<- gg+ xlab("Component Number")
  gg<- gg+ ylab("% Explained Variance")
  # girafe(code = print(gg))
  gg <- gg+theme(aspect.ratio=1,axis.text = element_text(size = 10),axis.title = element_text(size = 15))
  df_m <- data.frame(x=1:PCA$res@nPcs,y=var_rnd_mean)
  gg <- gg+geom_line(data = df_m,mapping = aes(x=x,y=y),colour='darkgreen',linetype = "dashed")
  df_s <- data.frame(x=1:PCA$res@nPcs,y=var_rnd_ic_sup)
  gg <- gg+geom_line(data = df_s,mapping = aes(x=x,y=y),colour='darkgreen',linetype = "dotted")
  df_i <- data.frame(x=1:PCA$res@nPcs,y=var_rnd_ic_inf)
  gg <- gg+geom_line(data = df_i,mapping = aes(x=x,y=y),colour='darkgreen',linetype = "dotted")
  print(gg)
})

# PCA - diagnostic: T2 and Q -----------------------------------------------

output$pca_dia_t2andq_title<-renderUI({
  if(input$pca_dia_t2andq_type== "t2andq")titolo <- HTML("T <sup>2</sup> and Q")
  if(input$pca_dia_t2andq_type=='t2vsq')titolo <-HTML("T <sup>2</sup> vs Q (influence plot)")
  titolo
})

output$pca_dia_t2andq_compN <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_dia_t2andq_compN", label = "Number of components",
              choices = 1:PCA$res@nPcs,
              selected = 1)
})

observeEvent(input$pca_dia_t2andq_compN,{
  if(!is.null(PCA$res)){
    if(sum(PCA$res@missing)>0){
      sendSweetAlert(session, title = "Input Error",
                     text = paste('Program aborted because of presence of', sum(PCA$res@missing),
                                  'missing data'),
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      if(PCA$type=='pca'){
        ncp<-as.numeric(input$pca_dia_t2andq_compN)
        n<-PCA$res@nObs
        m<-PCA$res@nVar
        X<-as.matrix(PCA$res@completeObs)
        P<-as.matrix(PCA$res@loadings[,1:ncp])
        L<-as.vector((PCA$res@sDev[1:ncp])^2)
        MQ<-diag(rep(1,m))-(P%*%t(P))
        MT<-P%*% (diag(length(L))*(1/L))%*%t(P)
        Q<-diag(X%*%MQ%*%t(X))
        T<-diag(X%*%MT%*%t(X))

        PCA$T2 <- T
        PCA$Q <- Q

      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Function not allowed with Varimax!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)

      }
    }
  }else{
    sendSweetAlert(session, title = "Input Error",
                   text = 'Run PCA Model First!',
                   type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)}
})

output$pca_dia_t2andq_rn <- renderUI({
  req(input$pca_dia_t2andq_type== "t2vsq")
  radioButtons("pca_dia_t2andq_rn", " ",
               choices = c("Rows number" = "rnum", "Rows name" = "rname"))
})

output$pca_dia_t2andq_joint <- renderUI({
  req(input$pca_dia_t2andq_type== "t2vsq")
  checkboxInput("pca_dia_t2andq_joint", label = "Joint diagnostics", value = FALSE)
})

output$pca_dia_t2andq_pl <- renderPlot({
  req(!is.null(PCA$res))
  req(!is.null(PCA$Q))
  req(!is.null(PCA$T2))
  
  Q <- as.numeric(PCA$Q)
  T <- as.numeric(PCA$T2)
  ncp<-as.numeric(input$pca_dia_t2andq_compN)
  n<-PCA$res@nObs
  m<-PCA$res@nVar
  
  Qlim<-10^(mean(log10(Q))+qt(0.95,n-1)*sd(log10(Q)))
  Qlim2<-10^(mean(log10(Q))+qt(0.99,n-1)*sd(log10(Q)))
  Qlim3<-10^(mean(log10(Q))+qt(0.999,n-1)*sd(log10(Q)))
  Tlim<-(n-1)*ncp/(n-ncp)*qf(0.95,ncp,n-ncp)
  Tlim2<-(n-1)*ncp/(n-ncp)*qf(0.99,ncp,n-ncp)
  Tlim3<-(n-1)*ncp/(n-ncp)*qf(0.999,ncp,n-ncp)
  
  if(is.na(Tlim))Tlim<-0
  mT<-max(T,Tlim)
  if(is.na(Qlim))Qlim<-0
  mQ<-max(Q,Qlim)
  
  if(input$pca_dia_t2andq_type== "t2andq"){
    op<-par(mfrow=c(1,2))
    plot(Q,ylim=c(0,1.1*mQ),ylab="Q",xlab="Sample number",cex.lab=1.2)
    abline(h=Qlim,col='red')
    abline(h=Qlim2,lty=2,col='red')
    abline(h=Qlim3,lty=3,col='red')
    xtx<-(1:n)[Q>Qlim];ytx<-Q[Q>Qlim];tx<-as.character(xtx)
    if(length(xtx)!=0)text(xtx,ytx,label=tx,cex=0.5,pos=3)
    #title(main=paste("Lines: crit. val. at p=0.05, 0.01, 0.001 Number of components: ",ncp),cex.main=0.6)
    title(main=paste("Lines: crit. val. at p=0.05, 0.01, 0.001 -",ncp,"components"),cex.main=0.6)
    plot(T,ylim=c(0,mT*1.1),ylab="T^2",xlab="Sample number",cex.lab=1.2)
    abline(h=Tlim,col='red')
    abline(h=Tlim2,lty=2,col='red')
    abline(h=Tlim3,lty=3,col='red')
    xtx<-(1:n)[T>Tlim];ytx<-T[T>Tlim];tx<-as.character(xtx)
    if(length(xtx)!=0)text(xtx,ytx,label=tx,cex=0.5,pos=3)
    title(main=paste("Lines: crit. val. at p=0.05, 0.01, 0.001 -",ncp,"components"),cex.main=0.6);par(op)
  }
  if(input$pca_dia_t2andq_type== "t2vsq"){
    req(!is.null(input$pca_dia_t2andq_rn))
    
    plot(T,Q,cex=0.5,ylim=c(0,mQ*1.1),xlim=c(0,mT*1.1),ylab="Q Index",xlab="T^2 Hotelling Index",cex.lab=1.2)
    title(main=paste("Number of components:",ncp),sub='Lines show critical values (solid: p=0.05; dashed: p=0.01; dotted: p=0.001) - Outliers coded according to their line number',cex.sub=0.6)
    grid()
    abline(v=Tlim,col='red')
    abline(h=Qlim,col='red')
    abline(v=Tlim2,lty=2,col='red')
    abline(h=Qlim2,lty=2,col='red')
    abline(v=Tlim3,lty=3,col='red')
    abline(h=Qlim3,lty=3,col='red')
    if((Tlim!=0)|(Qlim!=0)){
      txt<-1:n
      if(input$pca_dia_t2andq_rn=='rname')txt<-row.names(PCA$dataset)
      QT<-data.frame(Q=Q,T=T,tx=txt) 
      QTs<-subset(QT,((T>Tlim)|(Q>Qlim)))
      if(nrow(QTs)!=0)text(QTs$T,QTs$Q,label=QTs$tx,cex=0.5,pos=3)
      rm(QT,QTs)}
    if(input$pca_dia_t2andq_joint==TRUE){
      Qlim<-10^(mean(log10(Q))+qt(0.974679,n-1)*sd(log10(Q)))
      Qlim2<-10^(mean(log10(Q))+qt(0.994987,n-1)*sd(log10(Q)))
      Qlim3<-10^(mean(log10(Q))+qt(0.9995,n-1)*sd(log10(Q)))
      Tlim<-(n-1)*ncp/(n-ncp)*qf(0.974679,ncp,n-ncp)
      Tlim2<-(n-1)*ncp/(n-ncp)*qf(0.994987,ncp,n-ncp)
      Tlim3<-(n-1)*ncp/(n-ncp)*qf(0.9995,ncp,n-ncp)
    }
    if(is.na(Tlim))Tlim<-0
    mT<-max(T,Tlim)
    if(is.na(Qlim))Qlim<-0
    mQ<-max(Q,Qlim)
    # dev.new(title="influence plot joint diagnostics")
    plot(T,Q,cex=0.5,ylim=c(0,mQ*1.1),xlim=c(0,mT*1.1),ylab="Q Index",xlab="T^2 Hotelling Index",cex.lab=1.2)
    if(input$pca_dia_t2andq_joint==TRUE){
      title(main=paste("Joint diagnostics - Number of components:",ncp),sub='Boxes define acceptancy regions (solid: p=0.05; dashed: p=0.01; dotted: p=0.001) - Outliers coded according to their line number',cex.sub=0.6)
    }else{
      title(main=paste("Number of components:",ncp),sub='Lines show critical values (solid: p=0.05; dashed: p=0.01; dotted: p=0.001) - Outliers coded according to their line number',cex.sub=0.6)
    }
    grid()
    abline(v=Tlim,col='red')
    abline(h=Qlim,col='red')
    abline(v=Tlim2,lty=2,col='red')
    abline(h=Qlim2,lty=2,col='red')
    abline(v=Tlim3,lty=3,col='red')
    abline(h=Qlim3,lty=3,col='red')
    if((Tlim!=0)|(Qlim!=0)){
      txt<-1:n
      if(input$pca_dia_t2andq_rn=='rname')txt<-row.names(PCA$dataset)
      QT<-data.frame(Q=Q,T=T,tx=txt) 
      QTs<-subset(QT,((T>Tlim)|(Q>Qlim)))
      if(nrow(QTs)!=0)text(QTs$T,QTs$Q,label=QTs$tx,cex=0.5,pos=3)
    }
  }
})

output$pca_dia_t2andq_dwl <- downloadHandler(
  filename = "t2q.xlsx",
  content = function(file) {
    df <- cbind.data.frame(t2=PCA$T2,q=PCA$Q)
    df <- as.data.frame(df)
    df<-cbind.data.frame(' '=rownames(df),df)
    write.xlsx(df, file,colNames=TRUE)
  })

# PCA - diagnostic: T2 contribution -----------------------------------------------

output$pca_dia_t2contr_compN <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_dia_t2contr_compN", label = "Number of components",
              choices = 1:PCA$res@nPcs,
              selected = 2)
})

output$pca_dia_t2contr_nr <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_dia_t2contr_nr", label = "Row number",
              choices = 1:PCA$res@nObs,
              selected = 1)
})

output$pca_dia_t2contr_pl <- renderPlot({
  req(!is.null(PCA$res))
  
  pcaconplot<-function(i,PCA,n,m,ncp,lbl,nm){
    X<-as.matrix(PCA$res@completeObs)
    name_r <- rownames(as.data.frame(X))[i]
    P<-PCA$res@loadings[,1:ncp]
    S<-PCA$res@scores[,1:ncp]
    Ls<-PCA$res@sDev[1:ncp]#not variance because the sum must be the Malanobis distance
    sgl<-sum(Ls)
    sgr<-PCA$sgt-sgl
    MT<-P%*%diag(1/Ls,ncp,ncp)%*%t(P)
    T<-X%*%MT
    Ti<-T[i,]
    
    minT<-min(Ti)
    maxT<-max(Ti)
    if(minT>0){minT<-0}
    if(maxT<0){maxT<-0}
    Tlim<-c(minT,maxT)
    
    if(nm){
      Ti<-Ti/apply(abs(T),2,quantile,probs=0.95)
      Tlim<-c(min(Ti,-1.1),max(Ti,1.1))
    }

    options(scipen=1)
    barplot2(Ti,main=paste('T^2 of object',name_r),ylim=Tlim,
             cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
    box(which="plot",lty="solid")
    if(nm)abline(h=1,col='red')
    if(nm)abline(h=-1,col='red')
    return(Ti)
  }

  library(gplots)

    if(PCA$type=='pca'){
      req(input$pca_dia_t2contr_nr)
      req(input$pca_dia_t2contr_compN)
      # req(input$pca_dia_t2contr_norm)
      
      vc<-as.numeric(input$pca_dia_t2contr_nr)
      ncp<-as.numeric(input$pca_dia_t2contr_compN)
      nm<-input$pca_dia_t2contr_norm
      nc<-PCA$res@nVar
      nr<-PCA$res@nObs
      lbl<-names(as.data.frame(PCA$dataset))
      if(ncp<=nc){
        PCA$d_T2 <- pcaconplot(vc,PCA,nr,nc,ncp,lbl,nm)
      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Number of component greater than number of variables!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    }else{
      sendSweetAlert(session, title = "Input Error",
                     text = 'Function not allowed with Varimax!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)}
})

output$pca_dia_t2contr_dwl <- downloadHandler(
  filename = "t2contr.xlsx",
  content = function(file) {
    df <- cbind.data.frame(PCA$d_T2)
    df <- as.data.frame(df)
    colnames(df) <- 'T2'
    df<-cbind.data.frame(' '=rownames(df),df)
    write.xlsx(df, file,colNames=TRUE)
  })

# PCA - diagnostic: Q contribution -----------------------------------------------

output$pca_dia_qcontr_compN <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_dia_qcontr_compN", label = "Number of components",
              choices = 1:PCA$res@nPcs,
              selected = 2)
})

output$pca_dia_qcontr_nr <- renderUI({
  req(!is.null(PCA$res))
  selectInput("pca_dia_qcontr_nr", label = "Row number",
              choices = 1:PCA$res@nObs,
              selected = 1)
})

output$pca_dia_qcontr_pl <- renderPlot({
  req(!is.null(PCA$res))
  
  pcaconplot<-function(i,PCA,n,m,ncp,lbl,nm){
    X<-as.matrix(PCA$res@completeObs)
    name_r <- rownames(as.data.frame(X))[i]
    P<-PCA$res@loadings[,1:ncp]
    S<-PCA$res@scores[,1:ncp]
    Ls<-PCA$res@sDev[1:ncp]#not variance because the sum must be the Malanobis distance
    sgl<-sum(Ls)
    sgr<-PCA$sgt-sgl
    MQ<-S%*%t(P)
    Q<-sign(X-MQ)*(X-MQ)^2
    Qi<-Q[i,]
    
    minQ<-min(Qi)
    maxQ<-max(Qi)
    if(minQ>0){minQ<-0}
    if(maxQ<0){maxQ<-0}
    Qlim<-c(minQ,maxQ)
    
    if(nm){
      Qi<-Qi/apply(abs(Q),2,quantile,probs=0.95)
      Qlim<-c(min(Qi,-1.1),max(Qi,1.1))
    }

    options(scipen=1)
    barplot2(Qi,main=paste('Q of object',name_r),ylim=Qlim,
             cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
    box(which="plot",lty="solid")
    if(nm)abline(h=1,col='red')
    if(nm)abline(h=-1,col='red')
    return(Qi)
  }
  
  
  library(gplots)

    if(sum(PCA$res@missing)>0){
      mess<-paste('Not possible to compute Q diagnostics with', sum(PCA$res@missing),'missing data')
      sendSweetAlert(session, title = "Input Error",
                     text = mess,
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      if(PCA$type=='pca'){
        req(input$pca_dia_qcontr_nr)
        req(input$pca_dia_qcontr_compN)
        # req(input$pca_dia_qcontr_norm)
        
        vc<-as.numeric(input$pca_dia_qcontr_nr)
        ncp<-as.numeric(input$pca_dia_qcontr_compN)
        nm<-as.logical(input$pca_dia_qcontr_norm)
        nc<-PCA$res@nVar
        nr<-PCA$res@nObs
        lbl<-names(as.data.frame(PCA$dataset))
        if(ncp<=nc){
          PCA$d_Q <- pcaconplot(vc,PCA,nr,nc,ncp,lbl,nm)
        }else{
          sendSweetAlert(session, title = "Input Error",
                         text = 'Number of component greater than number of variables!',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }
      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Function not allowed with Varimax!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)}
    }
})

output$pca_dia_qcontr_dwl <- downloadHandler(
  filename = "qcontr.xlsx",
  content = function(file) {
    df <- cbind.data.frame(PCA$d_Q)
    df <- as.data.frame(df)
    colnames(df) <- 'Q'
    df<-cbind.data.frame(' '=rownames(df),df)
    write.xlsx(df, file,colNames=TRUE)
  })

  # PCA - ext data projection -----------------------------------------------
  output$pca_ext_data_paste_sp <- renderUI({
    req(input$pca_ext_data_load=='paste')
    br()
  })
  
  output$pca_ext_data_paste_sp1 <- renderUI({
    req(input$pca_ext_data_load=='paste')
    hr()
  })
  
  output$pca_ext_data_paste <- renderUI({
    req(input$pca_ext_data_load=='paste')
    actionButton("pca_ext_data_paste", label = "Paste")
  })
  
  output$pca_ext_data_excel <- renderUI({
    req(input$pca_ext_data_load=='excel')
    fileInput("pca_ext_data_excel", " ",
              multiple = FALSE,
              accept = c(".xlx",".xlsx"))
  })
  
  observeEvent(input$pca_ext_data_excel,{
    df <- tryCatch(
      read_excel(path = input$pca_ext_data_excel$datapath,sheet = 1,col_names = TRUE)  )
    dati_ext$DS<-as.data.frame(df)
    dati_ext$DS_nr <- as.data.frame(df)
  })
  
  observeEvent(input$pca_ext_data_paste,{
    df <- tryCatch(read.DIF(file = "clipboard",header = TRUE,transpose = TRUE),
                   error = function(e) "Selezionare un dataset!")
    df <- type.convert(df)
    dati_ext$DS<-as.data.frame(df)
    dati_ext$DS_nr <- as.data.frame(df)
  })

  output$pca_ext_data_varnames <- renderUI({
    req(!is.null(dati_ext$DS_nr))
    pickerInput("pca_ext_data_varnames", label = "Rows Names variable",
                choices = colnames(dati_ext$DS_nr),
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })

  observeEvent(input$pca_ext_data_varnames,{
    req(!is.null(dati_ext$DS))
    dati_ext$nr <- dati_ext$DS[,colnames(dati_ext$DS)%in%input$pca_ext_data_varnames]
    dati_ext$DS <- dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varnames]
    row.names(dati_ext$DS) <- dati_ext$nr
  })
 
  output$pca_ext_data_varsup <- renderUI({
    req(!is.null(dati_ext$DS))
    pickerInput("pca_ext_data_varsup", label = "Sup. variables",
                # choices = colnames(dati_ext$DS),
                choices = colnames(dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varnames]),
                multiple = TRUE)
  })
  
  output$pca_ext_data_rwnames <- renderUI({
    req(!is.null(dati_ext$DS))
    req(!is.null(input$pca_ext_data_varsup))
    pickerInput("pca_ext_data_rwnames", label = "Label variable",
                choices = input$pca_ext_data_varsup,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pca_ext_data_compx <- renderUI({
    req(!is.null(PCA$res))
    selectInput("pca_ext_data_compx", label = "Component on x-axis",
                choices = 1:PCA$res@nPcs,
                selected = 1)
  })
  
  output$pca_ext_data_compy <- renderUI({
    req(!is.null(PCA$res))
    selectInput("pca_ext_data_compy", label = "Component on y-axis",
                choices = 1:PCA$res@nPcs,
                selected = 2)
  })
  
  output$pca_ext_data_ell <- renderUI({
    req(!is.null(PCA$res))
    checkboxInput("pca_ext_data_ell", label = "Ellipses", value = FALSE)
  })
  
  output$pca_ext_data_rnames_tr <- renderUI({
    req(!is.null(PCA$res))
    checkboxInput("pca_ext_data_rnames_tr", label = "Row names training set", value = FALSE)
  })
  
  output$pca_ext_data_rnames <- renderUI({
    req(!is.null(PCA$res))
    checkboxInput("pca_ext_data_rnames", label = "Row names", value = FALSE)
  })
  
  observeEvent(input$bpcaext,{
    req(!is.null(dati_ext$DS))
    
    if(!is.null(PCA$res)){
      M_<-dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup]
      if(sum(is.na(M_))!=0){
        sendSweetAlert(session, title = "Input Error",
                       text = 'NA found: remove them before evaluation!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }else{
        if(sum(apply(dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup],2,'is.numeric'))!=
           ncol(dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup])){
          sendSweetAlert(session, title = "Input Error",
                         text = 'Le variabili qualitative devono essere selezionate come supplementari!',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }else{
          T_<-PCA$res@loadings
          unity<-matrix(rep(1,nrow(M_)),nrow(M_),1)
          if(PCA$center)M_<-M_-(unity%*%PCA$centered)
          if(PCA$scale)M_<-M_/(unity%*%PCA$scaled)
          D_<-tryCatch(as.matrix(M_) %*% T_,
                       error = function(e) "errore")
          if(D_=='errore'){
            sendSweetAlert(session, title = "Input Error",
                           text = 'Controllare il numero di variabili!',
                           type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
          }else{
            PCA_ext$scores <- D_
          }
        }
      }
    }else{
      sendSweetAlert(session, title = "Input Error",
                     text = 'Run PCA Model First!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  output$pca_ext_data_pl <- renderPlot({
    req(!is.null(PCA_ext$scores))
    
    ans <- list()
    # ans[[1]] <- dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup]
    ans[[2]] <- 'all'
    ans[[3]] <- 'all'
    # ans[[4]] <- 'None'
    ans[[5]] <- as.numeric(input$pca_ext_data_compx)
    ans[[6]] <- as.numeric(input$pca_ext_data_compy)
    ans[[7]] <- input$pca_ext_data_rnames_tr

    ans[[8]] <- input$pca_ext_data_rnames
    # ans[[8]] <- FALSE
    # if(!is.null(input$pca_ext_data_rwnames))ans[[8]] <- TRUE

    ans[[9]] <- input$pca_ext_data_ell
    
    c1_<-as.integer(ans[[5]])
    c2_<-as.integer(ans[[6]])
    
    lb_<-NULL
    if(as.logical(ans[[7]]))lb_<-row.names(PCA$dataset)
    
    lbd<-NULL
    # if(input$pca_ext_data_rnames)lbd <- row.names(dati_ext$DS)
    if(!is.null(input$pca_ext_data_rwnames))lbd<-dati_ext$DS[,input$pca_ext_data_rwnames]
    if(as.logical(ans[[8]]))lbd<-dati_ext$nr
    
    S_<-PCA$res@scores
    v1_<-PCA$res@R2[c1_]*100
    v2_<-PCA$res@R2[c2_]*100
    r<-nrow(S_)
    c<-nrow(PCA$res@loadings)
    if(!PCA$scale)c <- sum(apply(PCA$dataset,2,'var'))
    
    yn.lb<-TRUE
    if(is.null(lb_))yn.lb<-FALSE
    if(yn.lb){
      if(length(lb_)!=nrow(S_)){
        sendSweetAlert(session, title = "Input Error",
                       text = 'Wrong Score Label Dimension!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    }
    # new dataset evaluation
    
    D_<-PCA_ext$scores
    
    # plot standard score plot in the new scale 
    Slim<-c(min(S_[,c(c1_,c2_)],D_[,c(c1_,c2_)]),max(S_[,c(c1_,c2_)],D_[,c(c1_,c2_)]))
    if(PCA$type=='pca'){
      xl_<-paste('Component ',as.character(c1_),' (',as.character(round(v1_,1)),'% of variance)',sep='')
      yl_<-paste('Component ',as.character(c2_),' (',as.character(round(v2_,1)),'% of variance)',sep='')
    }else{
      xl_<-paste('Factor ',as.character(c1_),' (',as.character(round(v1_,1)),'% of variance)',sep='')
      yl_<-paste('Factor ',as.character(c2_),' (',as.character(round(v2_,1)),'% of variance)',sep='')
    }
    tl_=paste('Score Plot (',as.character(round((v1_+v2_),1)),'% of total variance)',sep='')
    
    op<-par(pty='s')
    if(!yn.lb){
      plot(S_[,c(c1_,c2_)],xlim=Slim,ylim=Slim,pty='o',xlab=xl_,ylab=yl_,col='gray')}
    if(yn.lb){
      plot(S_[,c(c1_,c2_)],xlim=Slim,ylim=Slim,xlab=xl_,ylab=yl_,type='n')
      text(S_[,c(c1_,c2_)],as.character(lb_),col='gray',cex=0.6)
    }
    grid()
    text(0,0,'+',cex=1.2,col='red')
    par(new=TRUE)
    
    if(as.logical(ans[[9]])){
      title(main=tl_,sub='Training: black - External: red - Ellipses: critical T^2 value at p=0.05, 0.01 and 0.001',cex.main=1.2,font.main=2,
            col.main="black",cex.sub=0.6,font.sub=2,col.sub="red")
      
      op<-par(pty='s')
      par(new=TRUE)
      
      rad1=sqrt((v1_/100*((r-1)/r)*c)*qf(.95,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((v2_/100*((r-1)/r)*c)*qf(.95,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      plot(x, y, type = "l",col='gray',xlim=Slim,ylim=Slim,xlab='',ylab='')
      par(new=TRUE)
      rad1=sqrt((v1_/100*((r-1)/r)*c)*qf(.99,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((v2_/100*((r-1)/r)*c)*qf(.99,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      plot(x, y, type = "l",col='gray',xlim=Slim,ylim=Slim,xlab='',ylab='',lty=2)
      par(new=TRUE)
      rad1=sqrt((v1_/100*((r-1)/r)*c)*qf(.999,2,r-2)*2*(r^2-1)/(r*(r-2))); 
      rad2=sqrt((v2_/100*((r-1)/r)*c)*qf(.999,2,r-2)*2*(r^2-1)/(r*(r-2)));
      theta <- seq(0, 2 * pi, length=1000)
      x <- rad1 * cos(theta)
      y <- rad2 * sin(theta)
      plot(x, y, type = "l",col='gray',xlim=Slim,ylim=Slim,xlab='',ylab='',lty=3)
      par(new=TRUE)
      
    }else{
      title(main=tl_,sub='Training: black - External: red',cex.main=1.2,font.main=2,
            col.main="black",cex.sub=0.6,font.sub=2,col.sub="red")
    }
    # new dataset plot
    ynld<-TRUE
    nd<-nrow(D_)
    if(is.null(lbd))ynld<-FALSE 
    if(ynld){
      if(length(lbd)!=nd){
        sendSweetAlert(session, title = "Input Error",
                       text = 'Wrong Dataset Label Dimension !',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    }
    if(!ynld)points(D_[,c1_],D_[,c2_],col='red')
    if(ynld){points(D_[,c1_],D_[,c2_],type='n')
      text(D_[,c1_],D_[,c2_],as.character(lbd),col='red',cex=0.7)}
    par(op)
  })
  
  output$pca_ext_data_dwl <- downloadHandler(
    filename = "ext_scores.xlsx",
    content = function(file) {
      df <- as.data.frame(PCA_ext$scores)
      df<-cbind.data.frame(' '=rownames(df),df)
      write.xlsx(df, file,colNames=TRUE)
    })
  
# PCA - ext diagnostic: T2 and Q ----------------------------------------------- 

  output$pca_ext2q_compN <- renderUI({
    req(!is.null(PCA$res))
    selectInput("pca_ext2q_compN", label = "Number of components",
                choices = 1:PCA$res@nPcs,
                selected = 2)
  })

  output$pca_ext_data_t2q_lab <- renderUI({
    req(!is.null(dati_ext$DS))
    req(!is.null(input$pca_ext_data_varsup))
    pickerInput("pca_ext_data_t2q_lab", label = "Label variable",
                choices = input$pca_ext_data_varsup,
                options =  list(
                  "max-options" = 1,
                  "max-options-text" = "No more!"
                ),
                multiple = TRUE)
  })
  
  output$pca_dia_ext_t2andq_pl <- renderPlot({
    req(!is.null(PCA$res))
    req(!is.null(PCA_ext$scores))
    
    pcanewdia<-function(PCA,n,m,ncp,M,lbd){
      X<-as.matrix(PCA$res@completeObs)
      P<-as.matrix(PCA$res@loadings[,1:ncp])
      L<-as.vector((PCA$res@sDev[1:ncp])^2)
      MQ<-diag(rep(1,m))-(P%*%t(P))
      MT<-P%*%(diag(length(L))*(1/L))%*%t(P)
      Q<-diag(X%*%MQ%*%t(X))
      T<-diag(X%*%MT%*%t(X))
      
      # new dataset evaluation
      nr<-nrow(M)
      nc<-ncol(M)
      unity<-matrix(rep(1,nr),nr,1)
      if(PCA$center)M<-M-(unity%*%PCA$centered)
      if(PCA$scale)M<-M/(unity%*%PCA$scaled)
      M<-as.matrix(M)
      QN<-diag(M%*%MQ%*%t(M))
      TN<-diag(M%*%MT%*%t(M))

      if(input$pca_dia_ext_t2andq_joint==FALSE){
        Qlim<-10^(mean(log10(Q))+qt(0.95,n-1)*sd(log10(Q)))
        Tlim<-(n-1)*ncp/(n-ncp)*qf(0.95,ncp,n-ncp)
        Qlim2<-10^(mean(log10(Q))+qt(0.99,n-1)*sd(log10(Q)))
        Tlim2<-(n-1)*ncp/(n-ncp)*qf(0.99,ncp,n-ncp)
        Qlim3<-10^(mean(log10(Q))+qt(0.999,n-1)*sd(log10(Q)))
        Tlim3<-(n-1)*ncp/(n-ncp)*qf(0.999,ncp,n-ncp)
        if(is.na(Tlim))Tlim<-0
        if(is.na(Qlim))Qlim<-0
        
        mQ<-max(Q,QN,Qlim)
        mT<-max(T,TN,Tlim)
        
        plot(T,Q,ylim=c(0,mQ*1.05),xlim=c(0,mT*1.05),cex=0.5, 
             ylab="Q Index",xlab="T^2 Hotelling Index",cex.lab=1.2)
        grid()
        tl<-paste("Number of components:",ncp)
        title(main=tl,sub='Train.: black - Ext.: red - Lines show critical values (solid: p=0.05; dashed: p=0.01; dotted: p=0.001)',cex.main=1.2,font.main=2,
              col.main="black",cex.sub=0.6,font.sub=2,col.sub="red")
        abline(v=Tlim,col='red')
        abline(h=Qlim,col='red')
        abline(v=Tlim2,lty=2,col='red')
        abline(h=Qlim2,lty=2,col='red')
        abline(v=Tlim3,lty=3,col='red')
        abline(h=Qlim3,lty=3,col='red')
        if(is.null(lbd))points(TN,QN,col='red')
        if(!is.null(lbd))text(TN,QN,as.character(lbd),col='red',cex=0.6)
      }
      
      if(input$pca_dia_ext_t2andq_joint==TRUE){
        Qlim<-10^(mean(log10(Q))+qt(0.974679,n-1)*sd(log10(Q)))
        Tlim<-(n-1)*ncp/(n-ncp)*qf(0.974679,ncp,n-ncp)
        Qlim2<-10^(mean(log10(Q))+qt(0.994987,n-1)*sd(log10(Q)))
        Tlim2<-(n-1)*ncp/(n-ncp)*qf(0.994987,ncp,n-ncp)
        Qlim3<-10^(mean(log10(Q))+qt(0.9995,n-1)*sd(log10(Q)))
        Tlim3<-(n-1)*ncp/(n-ncp)*qf(0.9995,ncp,n-ncp)
        
        mQ<-max(Q,QN,Qlim)
        mT<-max(T,TN,Tlim)
        
        plot(T,Q,ylim=c(0,mQ*1.05),xlim=c(0,mT*1.05),cex=0.5, 
             ylab="Q Index",xlab="T^2 Hotelling Index",cex.lab=1.2)
        grid()
        tl<-paste("Joint diagnostics - Number of components:",ncp)
        title(main=tl,sub='Train.: black - Ext.: red - Boxes define acceptancy regions (solid: p=0.05; dashed: p=0.01; dotted: p=0.001)',cex.main=1.2,font.main=2,
              col.main="black",cex.sub=0.6,font.sub=2,col.sub="red")
        abline(v=Tlim,col='red')
        abline(h=Qlim,col='red')
        abline(v=Tlim2,lty=2,col='red')
        abline(h=Qlim2,lty=2,col='red')
        abline(v=Tlim3,lty=3,col='red')
        abline(h=Qlim3,lty=3,col='red')
        if(is.null(lbd))points(TN,QN,col='red')
        if(!is.null(lbd))text(TN,QN,as.character(lbd),col='red',cex=0.6)
      }
      
      t2qext<-cbind.data.frame(TN,QN)
      colnames(t2qext)<-c('T^2','Q')
      t2qext_tbl<-cbind(c(1:length(TN)),t2qext)
      colnames(t2qext_tbl)[1]<-' '
      # write.table(t2qext_tbl,'t2qext.txt',sep="\t",row.names=FALSE,col.names=TRUE)
      return(t2qext)}
    

    if(sum(PCA$res@missing)>0){
      mess<-paste('Not possible to compute Q diagnostics with', sum(PCA$res@missing),'missing data')
      sendSweetAlert(session, title = "Input Error",
                     text = mess,
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      if(PCA$type=='pca'){
        M_<-dati_ext$DS[,!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup]
        if(sum(is.na(M_))!=0){
          sendSweetAlert(session, title = "Input Error",
                         text = '>>NA found: remove them before evaluation<<',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }else{
          lbd<-NULL
          # if(as.logical(input$pca_dia_ext_t2andq_rnames)) lbd<-rownames(M_)
          if(as.logical(input$pca_dia_ext_t2andq_rnumbers)) lbd<-1:nrow(M_)
          if(!is.null(input$pca_ext_data_t2q_lab))lbd <- dati_ext$DS[,input$pca_ext_data_t2q_lab]
          
          req(input$pca_ext2q_compN)
          t2qext<-pcanewdia(PCA,PCA$res@nObs,PCA$res@nVar,as.numeric(input$pca_ext2q_compN),M_,lbd)
          PCA_ext$T2 <- t2qext$'T^2'
          PCA_ext$Q <- t2qext$Q
        }
      }else{
        sendSweetAlert(session, title = "Input Error",
                       text = 'Function not allowed with Varimax!',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
      }
      })

  output$pca_dia_ext_t2andq_dwl <- downloadHandler(
    filename = "t2qext.xlsx",
    content = function(file) {
      df <- cbind.data.frame(t2=PCA_ext$T2,q=PCA_ext$Q)
      df <- as.data.frame(df)
      df<-cbind.data.frame(' '=rownames(df),df)
      write.xlsx(df, file,colNames=TRUE)
    })
  
  # PCA - ext diagnostic: T2 contribution -----------------------------------------------

  output$pca_dia_ext_t2contr_compN <- renderUI({
    req(!is.null(PCA$res))
    selectInput("pca_dia_ext_t2contr_compN", label = "Number of components",
                choices = 1:PCA$res@nPcs,
                selected = 2)
  })

  output$pca_dia_ext_t2contr_nr <- renderUI({
    req(!is.null(PCA_ext$scores))
    selectInput("pca_dia_ext_t2contr_nr", label = "Row number",
                choices = 1:nrow(PCA_ext$scores),
                selected = 1)
  })
  
  output$pca_dia_ext_t2contr_pl <- renderPlot({
    req(!is.null(PCA$res))
    req(!is.null(PCA_ext$scores))
    
    pcaconplot_testset<-function(X,i,PCA,ncp,lbl,nm){
      # new dataset evaluation
      nr<-nrow(X)
      nc<-ncol(X)
      name_r <- rownames(X)
      unity<-matrix(rep(1,nr),nr,1)
      if(PCA$center)X<-X-(unity%*%PCA$centered)
      if(PCA$scale)X<-X/(unity%*%PCA$scaled)
      X<-as.matrix(X)
      P<-PCA$res@loadings[,1:ncp,drop=FALSE]
      S<-X%*%P
      Ls<-PCA$res@sDev[1:ncp]
      sgl<-sum(Ls)
      sgr<-PCA$sgt-sgl
      MQ<-S%*%t(P)
      
      # MT<-P%*%(diag(1/Ls))%*%t(P)
      if(ncp>1){
        MT<-P%*%(diag(1/Ls))%*%t(P)
      }else{
        MT<-P%*%(diag(as.matrix(1/Ls)))%*%t(P)}
      
      T<-X%*%MT
      Ti<-T[i,]
      minT<-min(Ti)
      maxT<-max(Ti)
      if(minT>0){minT<-0}
      if(maxT<0){maxT<-0}
      Tlim<-c(minT,maxT)
      
      if(nm){
        X<-as.matrix(PCA$res@completeObs)
        S<-PCA$res@scores[,1:ncp]
        MQ<-S%*%t(P)
        T<-X%*%MT
        Ti<-Ti/apply(abs(T),2,quantile,probs=0.95)
        Tlim<-c(min(Ti,-1.1),max(Ti,1.1))
      }
      # dev.new(title="T^2 contribution plot external")
      options(scipen=1)
      barplot2(Ti,main=paste('T^2 of object',name_r),ylim=Tlim,cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
      box(which="plot",lty="solid")
      if(nm)abline(h=1,col='red')
      if(nm)abline(h=-1,col='red')
      return(Ti)}

    library(gplots)
    if(PCA$type=='pca'){
      req(input$pca_dia_ext_t2contr_compN)
      req(input$pca_dia_ext_t2contr_nr)
      
      M_<-dati_ext$DS[as.numeric(input$pca_dia_ext_t2contr_nr),!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup]
      if(sum(is.na(M_))!=0){
        sendSweetAlert(session, title = "Input Error",
                       text = '>>NA found: remove them before evaluation<<',
                       type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }else{
        lbl<-names(as.data.frame(PCA$dataset))
        ncp<-as.numeric(input$pca_dia_ext_t2contr_compN)
        nm<-input$pca_dia_ext_t2contr_norm
        nc<-PCA$res@nVar
        if(ncp<=nc){
          if(nrow(M_)==1){
            PCA$c_T2 <- pcaconplot_testset(M_,1,PCA,ncp,lbl,nm)
          }else{
            sendSweetAlert(session, title = "Input Error",
                           text = 'You must choose just one row!',
                           type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
          }
        }else{
          sendSweetAlert(session, title = "Input Error",
                         text = 'Number of component greater than number of variables!',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }
      }
      
    }else{
      sendSweetAlert(session, title = "Input Error",
                     text = 'Function not allowed with Varimax!',
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }
  })
  
  output$pca_dia_ext_t2contr_dwl <- downloadHandler(
    filename = "ext_t2contr.xlsx",
    content = function(file) {
      df <- cbind.data.frame(PCA$c_T2)
      df <- as.data.frame(df)
      colnames(df) <- 'T2'
      df<-cbind.data.frame(' '=rownames(df),df)
      write.xlsx(df, file,colNames=TRUE)
    })

  # PCA - ext diagnostic: Q contribution -----------------------------------------------
  
  output$pca_dia_ext_qcontr_compN <- renderUI({
    req(!is.null(PCA$res))
    selectInput("pca_dia_ext_qcontr_compN", label = "Number of components",
                choices = 1:PCA$res@nPcs,
                selected = 2)
  })
  
  output$pca_dia_ext_qcontr_nr <- renderUI({
    req(!is.null(PCA_ext$scores))
    selectInput("pca_dia_ext_qcontr_nr", label = "Row number",
                choices = 1:nrow(PCA_ext$scores),
                selected = 1)
  })
  
  output$pca_dia_ext_qcontr_pl <- renderPlot({
    req(!is.null(PCA$res))
    req(!is.null(PCA_ext$scores))

    pcaconplot_testset<-function(X,i,PCA,ncp,lbl,nm){
      # new dataset evaluation
      nr<-nrow(X)
      nc<-ncol(X)
      name_r <- rownames(X)
      unity<-matrix(rep(1,nr),nr,1)
      if(PCA$center)X<-X-(unity%*%PCA$centered)
      if(PCA$scale)X<-X/(unity%*%PCA$scaled)
      X<-as.matrix(X)
      P<-PCA$res@loadings[,1:ncp,drop=FALSE]
      S<-X%*%P
      Ls<-PCA$res@sDev[1:ncp]
      sgl<-sum(Ls)
      sgr<-PCA$sgt-sgl
      MQ<-S%*%t(P)
      
      # MT<-P%*%(diag(1/Ls))%*%t(P)
      if(ncp>1){
        MT<-P%*%(diag(1/Ls))%*%t(P)
      }else{
        MT<-P%*%(diag(as.matrix(1/Ls)))%*%t(P)}
      
      Q<-sign(X-MQ)*(X-MQ)^2
      Qi<-Q[i,]
      minQ<-min(Qi)
      maxQ<-max(Qi)
      if(minQ>0){minQ<-0}
      if(maxQ<0){maxQ<-0}
      Qlim<-c(minQ,maxQ)
      if(nm){
        X<-as.matrix(PCA$res@completeObs)
        S<-PCA$res@scores[,1:ncp]
        MQ<-S%*%t(P)
        Q<-sign(X-MQ)*(X-MQ)^2
        Qi<-Qi/apply(abs(Q),2,quantile,probs=0.95)
        Qlim<-c(min(Qi,-1.1),max(Qi,1.1))
      }
      # dev.new(title="Q contribution plot external")
      options(scipen=1)
      barplot2(Qi,main=paste('Q of object',name_r),ylim=Qlim,cex.lab=1.2,names.arg=lbl,cex.names=0.6,plot.grid=TRUE,las=2,cex.axis=0.6)
      box(which="plot",lty="solid")
      if(nm)abline(h=1,col='red')
      if(nm)abline(h=-1,col='red')
      return(Qi)}
    
    library(gplots)

    if(sum(PCA$res@missing)>0){
      mess<-paste('Not possible to compute Q diagnostics with', sum(PCA$res@missing),'missing data')
      sendSweetAlert(session, title = "Input Error",
                     text = mess,
                     type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
    }else{
      if(PCA$type=='pca'){
        req(input$pca_dia_ext_qcontr_compN)
        req(input$pca_dia_ext_qcontr_nr)
  
        M_<-dati_ext$DS[as.numeric(input$pca_dia_ext_qcontr_nr),!colnames(dati_ext$DS)%in%input$pca_ext_data_varsup]
        if(sum(is.na(M_))!=0){
          sendSweetAlert(session, title = "Input Error",
                         text = '>>NA found: remove them before evaluation<<',
                         type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
        }else{
          lbl<-names(as.data.frame(PCA$dataset))
          ncp<-as.numeric(input$pca_dia_ext_qcontr_compN)
          nm<-input$pca_dia_ext_qcontr_norm 
          nc<-PCA$res@nVar
          if(ncp<=nc){
            if(nrow(M_)==1){
              PCA$c_Q <- pcaconplot_testset(M_,1,PCA,ncp,lbl,nm)
            }else{
              sendSweetAlert(session, title = "Input Error",
                             text = 'You must choose just one row!',
                             type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
            }
          }else{
            sendSweetAlert(session, title = "Input Error",
                           text = 'Number of component greater than number of variables!',
                           type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)}
        } 
          }else{
            sendSweetAlert(session, title = "Input Error",
                           text = 'Function not allowed with Varimax!',
                           type = "warning",btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)}
 
    }
  })
  
  output$pca_dia_ext_qcontr_dwl <- downloadHandler(
    filename = "ext_qcontr.xlsx",
    content = function(file) {
      df <- cbind.data.frame(PCA$c_Q)
      df <- as.data.frame(df)
      colnames(df) <- 'Q'
      df<-cbind.data.frame(' '=rownames(df),df)
      write.xlsx(df, file,colNames=TRUE)
    })


}
  



























