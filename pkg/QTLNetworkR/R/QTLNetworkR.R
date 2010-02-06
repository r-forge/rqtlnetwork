`QTLNetworkR` <-
function(){
  #.qtlnetworkr<<-new.env()
  color<-colors()
  cl<-color[c(552,26,32,47,68,73,81,84,100,142,254,362,372,536,541,547,455,445)]#,0:20*5+152,133)]
  linetype<-c("solid","dashed","dotted","dotdash","longdash","twodash","F8","431313","22848222")
  linetype<-rep(linetype,4)
  assign("traitlth",NA, envir=.qtlnetworkr)
  assign("traitname",NA, envir=.qtlnetworkr)
  assign("chromosome",NA, envir=.qtlnetworkr)
  widgets<-list()
  win = gwindow("QTLNetworkR")#, width=700, height=400)
  gp = ggroup(horizontal=FALSE, cont=win, expand=TRUE)  # main group

  tb = list()
  tb$Quit$handler = function(h,...) dispose(win)
  tb$Quit$icon = "quit"
  tb = gtoolbar(tb, cont=gp)

  tmp <- gframe("Files", container=gp, expand=TRUE)
  lyout<-glayout(container=tmp)
  lyout[1,1]<- gbutton("QNK File...", cont = lyout, handler = function(h,...) {
      std <- gfile("Select QNK File...")
      if(std != "") {
        x <- read.table(std,fill=T,colClasses="character",
               col.names=paste("col",1:max(count.fields(std))))
        y <- length(grep("_trait",x[,1]))
        z <- x[grep("^_trait$",x[,1]),3]
        assign("qnkfile",x, envir=.qtlnetworkr)
        assign("traitlth",y, envir=.qtlnetworkr)
        assign("traitname",z, envir=.qtlnetworkr)
        svalue(widgets$qnk)<-std
      }
  })
  lyout[1,2]<-(widgets$qnk<-gedit(text="",cont=lyout))
  lyout[2,1]<-gbutton("Map File...", cont=lyout,
        handler = function(h,...) {
        std<-gfile(text="Select Map File...")
        if(std != "")
        {
          x <- read.table(std,fill=T,colClasses="character")
          y <- 1:as.numeric(x[grep("_c|Chromosome",x[,1]),2])
          assign("mapfile",x, envir=.qtlnetworkr)
          assign("chromosome",y, envir=.qtlnetworkr)
          svalue(widgets$map)<-std
        }
    })
  lyout[2,2]<-(widgets$map<-gedit(text="",cont=lyout))
  ##prepare for data file ready
  nb = gnotebook(cont = gp)
  MI = ggroup(horizontal=FALSE, cont=nb, label="MI")
  QTL =ggroup(horizontal=FALSE, cont=nb, label="QTL")
  QTLeffect =  ggroup(horizontal=FALSE, cont=nb, label="QE")
  MII = ggroup(horizontal=FALSE, cont=nb, label="MII")
  Epistasis =ggroup(horizontal=FALSE, cont=nb, label="Epistasis")
  QTLNetwork = ggroup(horizontal=FALSE, cont=nb, label="QTLNetwork")


  lyout<-glayout(container=MI)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            #assign("std",myselect.list(list=as.character(1:traitlth),multiple=T),envir=.GlobalEnv)
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr),multiple=TRUE)
            svalue(widgets$trait)<-toString(std)
        }
    })
  lyout[1,2]<-(widgets$trait<-gedit("",cont=lyout))
  lyout[1,3]<-gbutton("Color...",cont=lyout,
      handler = function(h,...) {
          if(svalue(widgets$trait)!="")
          {
              #assign("colorVal",cl[length(traitVal)], envir=.GlobalEnv)
              #if(length(get(traitVal)>1)
              #{
              std<-myselect.list(list=cl,multiple=TRUE,title="Same number with trait")
              svalue(widgets$color)<-toString(std)

              #}else  svalue(widgets$color)<-cl[1]
          }
      })
  lyout[1,4]<-(widgets$color<-gedit("",cont=lyout))
  lyout[2,1]<-gbutton("Chromosome...",cont=lyout,handler = function(h,...) {
              std<-myselect.list(list=c("all",as.character(get("chromosome",envir=.qtlnetworkr))),multiple=TRUE,title="Select chromosomes...")
              svalue(widgets$chr)<-toString(std)
      })
  lyout[2,2]<-(widgets$chr<-gedit("all",cont=lyout))
  lyout[2,3]<-gbutton("Threshold",cont=lyout)
  lyout[2,4]<-(widgets$threshold<-gcombobox(c("mean","each"),cont=lyout))
  lyout[3,1]<-gbutton("Clean",cont=lyout)
  lyout[3,2]<-(widgets$clean<-gcombobox(c("FALSE","TRUE"),cont=lyout))
  lyout[3,3]<-gbutton("LineType",cont=lyout)
  lyout[3,4]<-(widgets$LineType<-gcombobox(c("FALSE","TRUE"),cont=lyout))
  lyout[4,1]<-gbutton("Xlabel",cont=lyout)
  lyout[4,2]<-(widgets$Xlabel<-gcombobox(c("Chromosome","ChromosomeLength"),cont=lyout))
  lyout[4,3]<-gbutton("XlabelRot",cont=lyout)
  lyout[4,4]<-(widgets$XlabelRot<-gedit("0",cont=lyout))
  lyout[5,1]<-gbutton("XlabelFont",cont=lyout)
  lyout[5,2]<-(widgets$XlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[5,3]<-gbutton("XlabelFontSize",cont=lyout)
  lyout[5,4]<-(widgets$XlabelFontSize<-gedit("1",cont=lyout))
  lyout[6,1]<-gbutton("YlabelFont",cont=lyout)
  lyout[6,2]<-(widgets$YlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[6,3]<-gbutton("YlabelFontSize",cont=lyout)
  lyout[6,4]<-(widgets$YlabelFontSize<-gedit("1",cont=lyout))
  lyout[7,1]<-gbutton("NotationFont",cont=lyout)
  lyout[7,2]<-(widgets$NotationFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[7,3]<-gbutton("NotationFontSize",cont=lyout)
  lyout[7,4]<-(widgets$NotationFontSize<-gedit("1",cont=lyout))
  lyout[8,1]<-gbutton("LineWidth",cont=lyout)
  lyout[8,2]<-(widgets$LineWidth<-gedit("1",cont=lyout))
  button.group <- ggroup(container = MI)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      MIPlot()
      }, container=button.group)
  ##MIPlot over

  ##QTL Plot
  lyout<-glayout(container=QTL)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            #assign("std",myselect.list(list=as.character(1:traitlth),multiple=T),envir=.GlobalEnv)
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr),multiple=TRUE)
            svalue(widgets$QTLtrait)<-toString(std)
        }
    })
  lyout[1,2]<-(widgets$QTLtrait<-gedit("",cont=lyout))
  lyout[1,3]<-gbutton("Color...",cont=lyout,
      handler = function(h,...) {
          #if(svalue(widgets$QTLtrait)!="")
          #{
              #assign("colorVal",cl[length(traitVal)], envir=.GlobalEnv)
              #if(length(get(traitVal)>1)
              #{
              std<-myselect.list(list=cl,multiple=TRUE,title="Same number with trait")
              svalue(widgets$QTLcolor)<-toString(std)

              #}else  svalue(widgets$color)<-cl[1]
          #}
      })
  lyout[1,4]<-(widgets$QTLcolor<-gedit("",cont=lyout))
  lyout[2,1]<-gbutton("Chromosome...",cont=lyout,
      handler = function(h,...) {
              std<-myselect.list(list=c("all",as.character(get("chromosome",envir=.qtlnetworkr))),multiple=TRUE,title="Select chromosomes...")
              svalue(widgets$QTLchr)<-toString(std)
      })
  lyout[2,2]<-(widgets$QTLchr<-gedit("all",cont=lyout))

  lyout[2,3]<-gbutton("Threshold",cont=lyout)
  lyout[2,4]<-(widgets$QTLthreshold<-gcombobox(c("mean","each"),cont=lyout))
  lyout[3,1]<-gbutton("Clean",cont=lyout)
  lyout[3,2]<-(widgets$QTLclean<-gcombobox(c("FALSE","TRUE"),cont=lyout))
  lyout[3,3]<-gbutton("LineType",cont=lyout)
  lyout[3,4]<-(widgets$QTLlineType<-gcombobox(c("FALSE","TRUE"),cont=lyout))
  lyout[4,1]<-gbutton("Xlabel",cont=lyout)
  lyout[4,2]<-(widgets$QTLXlabel<-gcombobox(c("Chromosome","chromosomeLength"),cont=lyout))
  lyout[4,3]<-gbutton("XlabelRot",cont=lyout)
  lyout[4,4]<-(widgets$QTLXlabelRot<-gedit("0",cont=lyout))
  lyout[5,1]<-gbutton("XlabelFont",cont=lyout)
  lyout[5,2]<-(widgets$QTLXlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[5,3]<-gbutton("XlabelFontSize",cont=lyout)
  lyout[5,4]<-(widgets$QTLXlabelFontSize<-gedit("1",cont=lyout))
  lyout[6,1]<-gbutton("YlabelFont",cont=lyout)
  lyout[6,2]<-(widgets$QTLYlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[6,3]<-gbutton("YlabelFontSize",cont=lyout)
  lyout[6,4]<-(widgets$QTLYlabelFontSize<-gedit("1",cont=lyout))
  lyout[7,1]<-gbutton("NotationFont",cont=lyout)
  lyout[7,2]<-(widgets$QTLNotationFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[7,3]<-gbutton("NotationFontSize",cont=lyout)
  lyout[7,4]<-(widgets$QTLNotationFontSize<-gedit("1",cont=lyout))
  lyout[8,1]<-gbutton("LineWidth",cont=lyout)
  lyout[8,2]<-(widgets$QTLlineWidth<-gedit("1",cont=lyout))
  button.group <- ggroup(container = QTL)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      QTLPlot()
      }, container=button.group)
  ##QTLPlot over

  ##QTLdetection
  lyout<-glayout(container=QTLeffect)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr),multiple=FALSE)
            svalue(widgets$Effectrait)<-toString(std)
        }
    })
  lyout[1,2]<-(widgets$Effectrait<-gedit("",cont=lyout))
  lyout[1,3]<-gbutton("XlabelRot",cont=lyout)
  lyout[1,4]<-(widgets$EffectXlabelRot<-gedit("0",cont=lyout))
  lyout[2,1]<-gbutton("XlabelFont",cont=lyout)
  lyout[2,2]<-(widgets$EffectXlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[2,3]<-gbutton("XlabelFontSize",cont=lyout)
  lyout[2,4]<-(widgets$EffectXlabelFontSize<-gedit("1",cont=lyout))
  lyout[3,1]<-gbutton("YlabelFont",cont=lyout)
  lyout[3,2]<-(widgets$EffectYlabelFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[3,3]<-gbutton("YlabelFontSize",cont=lyout)
  lyout[3,4]<-(widgets$EffectYlabelFontSize<-gedit("1",cont=lyout))
  lyout[4,1]<-gbutton("NotationFont",cont=lyout)
  lyout[4,2]<-(widgets$EffectNotationFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[4,3]<-gbutton("NotationFontSize",cont=lyout)
  lyout[4,4]<-(widgets$EffectNotationFontSize<-gedit("1",cont=lyout))
  lyout[5,1]<-gbutton("MinYLabel",cont=lyout)
  lyout[5,2]<-(widgets$EffectMinY<-gedit("",cont=lyout))
  lyout[5,3]<-gbutton("MaxYLabel",cont=lyout)
  lyout[5,4]<-(widgets$EffectMaxY<-gedit("",cont=lyout))
  button.group <- ggroup(container = QTLeffect)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      QTLeffectPlot()
      }, container=button.group)
  ##over

  ##MII2DPlot
  lyout<-glayout(container=MII)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr),multiple=FALSE)
            svalue(widgets$twodtrait)<-toString(std)
        }
    })
  lyout[1,2]<-(widgets$twodtrait<-gedit("",cont=lyout))
  lyout[2,1]<-gbutton("ColorkeyFont",cont=lyout)
  lyout[2,2]<-(widgets$colorkeyFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[2,3]<-gbutton("ColorkeyFontSize",cont=lyout)
  lyout[2,4]<-(widgets$colorkeyFontSize<-gedit("1",cont=lyout))
  lyout[3,1]<-gbutton("ThresholdFont",cont=lyout)
  lyout[3,2]<-(widgets$thresholdFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[3,3]<-gbutton("ThresholdFontSize",cont=lyout)
  lyout[3,4]<-(widgets$thresholdFontSize<-gedit("1",cont=lyout))
  lyout[4,1]<-gbutton("LeftBottomFont",cont=lyout)
  lyout[4,2]<-(widgets$leftBottomFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[4,3]<-gbutton("LeftBottomFontSize",cont=lyout)
  lyout[4,4]<-(widgets$leftBottomFontSize<-gedit("1",cont=lyout))
  
  button.group <- ggroup(container = MII)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      MII2DPlot()
      }, container=button.group)
  ##MII2DPlot over

  ##epiPlot

  lyout<-glayout(container=Epistasis)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            allow<-AllowTrait()
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr)[allow],multiple=FALSE)
            svalue(widgets$epitrait)<-toString(std)
        }
    })
  lyout[1,2]<-(widgets$epitrait<-gedit("",cont=lyout))
  lyout[1,3]<-gbutton("Blank",cont=lyout)
  lyout[1,4]<-(widgets$blank<-gedit("10",cont=lyout))
  lyout[2,1]<-gbutton("ColorkeyFont",cont=lyout)
  lyout[2,2]<-(widgets$epi.colorkeyFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[2,3]<-gbutton("ColorkeyFontSize",cont=lyout)
  lyout[2,4]<-(widgets$epi.colorkeyFontSize<-gedit("1",cont=lyout))
  lyout[3,1]<-gbutton("ThresholdFont",cont=lyout)
  lyout[3,2]<-(widgets$epi.thresholdFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[3,3]<-gbutton("ThresholdFontSize",cont=lyout)
  lyout[3,4]<-(widgets$epi.thresholdFontSize<-gedit("1",cont=lyout))
  lyout[4,1]<-gbutton("LeftBottomFont",cont=lyout)
  lyout[4,2]<-(widgets$epi.leftBottomFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[4,3]<-gbutton("LeftBottomFontSize",cont=lyout)
  lyout[4,4]<-(widgets$epi.leftBottomFontSize<-gedit("1",cont=lyout))
  lyout[5,1]<-gbutton("RightTopFont",cont=lyout)
  lyout[5,2]<-(widgets$epi.rightTopFont<-gcombobox(c("1","2","3","4"),cont=lyout))
  lyout[5,3]<-gbutton("RightTopFontSize",cont=lyout)
  lyout[5,4]<-(widgets$epi.rightTopFontSize<-gedit("0.6",cont=lyout))
    
  button.group <- ggroup(container = Epistasis)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      epiPlot()
      }, container=button.group)
  ##epistasis over


  ##QTLNetwork Plot
  lyout<-glayout(container=QTLNetwork)
  lyout[1,1]<-gbutton("Trait...", cont=lyout,
    handler = function(h,...)
    {
        if(!is.na(get("traitname",envir=.qtlnetworkr)[1]))
        {
            std<-myselect.list(list=get("traitname",envir=.qtlnetworkr),multiple=TRUE)
            svalue(widgets$QTLNtrait)<-toString(std)

        }
    })
  lyout[1,2]<-(widgets$QTLNtrait<-gedit("",cont=lyout))
  lyout[1,3]<-gbutton("chromosome",cont=lyout)
  lyout[1,4]<-(widgets$QTLNchr<-gedit("chr",cont=lyout))
  lyout[2,1]<-gbutton("ChromosomeWidth",cont=lyout)
  lyout[2,2]<-(widgets$chrWidth<-gedit("15",cont=lyout))
  lyout[2,3]<-gbutton("EpistasisLineWidth",cont=lyout)
  lyout[2,4]<-(widgets$epiLineWidth<-gedit("1",cont=lyout))
  lyout[3,1]<-gbutton("Text",cont=lyout)
  lyout[3,2]<-(widgets$Text<-gedit("Yes",cont=lyout))
  lyout[3,3]<-gbutton("TextPosition",cont=lyout)
  lyout[3,4]<-(widgets$textpos<-gedit("3",cont=lyout))
  lyout[4,1]<-gbutton("ChromosomeFont",cont=lyout)
  lyout[4,2]<-(widgets$chrFont<-gedit("2",cont=lyout))
  lyout[4,3]<-gbutton("ChromosomeFontSize",cont=lyout)
  lyout[4,4]<-(widgets$chrFontSize<-gedit("1",cont=lyout))
  lyout[5,1]<-gbutton("NotationFont",cont=lyout)
  lyout[5,2]<-(widgets$notationFont<-gedit("1",cont=lyout))
  lyout[5,3]<-gbutton("NotationFontSize",cont=lyout)
  lyout[5,4]<-(widgets$notationFontSize<-gedit("1",cont=lyout))
  lyout[6,1]<-gbutton("TextFont",cont=lyout)
  lyout[6,2]<-(widgets$TextFont<-gedit("1",cont=lyout))
  lyout[6,3]<-gbutton("TextFontSize",cont=lyout)
  lyout[6,4]<-(widgets$TextFontSize<-gedit("1",cont=lyout))
  lyout[7,1]<-gbutton("SymbolSize",cont=lyout)
  lyout[7,2]<-(widgets$symbol<-gedit("1",cont=lyout))
  button.group <- ggroup(container = QTLNetwork)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Run", handler=function(h,...){
      close.cur.dev()
      QTLNetworkPlot()
      }, container=button.group)
  ##QTLNetwork Plot over
  bind<-function(a,b){
        syn<-a
        syn[(length(a)+1):(length(a)+length(b))]<-b
        syn}
  AllowTrait<-function()
  {
      qnkfile<-get("qnkfile",envir=.qtlnetworkr)
      traitlth<-get("traitlth",envir=.qtlnetworkr)
      #print(traitlth)
      trait_qtl<-grep("_plot_QTL",qnkfile[,1])
      trait_epi<-grep("_plot_epistasis",qnkfile[,1])
      #print(trait_epi)
      u<-n<-1
      delete<-0

      if(length(trait_epi)!=traitlth){
          for(i in 1:traitlth){
              if(trait_epi[u]==(trait_qtl[i]+5)){if(u<length(trait_epi)) u<-u+1}else{delete[n]<-i;n<-n+1}
          }
      }
      allow_trait<-1:traitlth
      if(delete[1]!=0) allow_trait<-allow_trait[-delete]
      return(allow_trait)
  }

   close.cur.dev<-function(){
      if(!is.null(dev.list())) dev.off()
  }
  myselect.list<-
  function (list, multiple = TRUE, title = NULL,...)
  {
       if(is.null(title)) title <- ifelse(multiple, "Select one or more", "Select one")else title<-title
       helper <- function() {
           ans <- new.env()
           x1 <- ggroup(horizontal = FALSE)
           x2 <- gtable(list, multiple = multiple, con = x1, expand = TRUE)
           size(x2)<-c(200,500)
           ret <- gbasicdialog(title = title, widget = x1, handler = function(h,
              ...) {
              value <- svalue(x2)
              assign("selected", value, env = h$action$env)
              dispose(x1)
          }, action = list(env = ans))
          ans
      }
      items <- helper()$selected
      if (is.null(items))
          items <- ""
      items
  }
  grid.semiCircle<-function(start=-pi/2,end=pi/2,x=0,y=0,radii=0.1,col="black",fill=NULL)
  {
      theta=seq(start,end,length.out=1000)
      x=cos(theta)*radii+x
      y=sin(theta)*radii+y
      grid.polygon(x,y,gp=gpar(col=col,fill=fill))
  }

  MIPlot<-function(){
      if(svalue(widgets$trait)=="") stop("Select Trait...")
      if(svalue(widgets$color)=="")  stop("Slect Color...")
      if(svalue(widgets$QTLchr)=="") stop("You should select at least one chromosome...")
      #traitVal<-which(is.element(strsplit(svalue(widgets$trait),split=", ")[[1]],get("traitname",envir=.qtlnetworkr)))
      traitVal<-as.numeric(lapply(strsplit(svalue(widgets$trait),split=", ")[[1]],function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
      chr<-strsplit(svalue(widgets$chr),split=",")[[1]]
      if(chr[1]!="all") chr<-as.numeric(chr)
      #print(traitVal)
      traitname<-get("traitname",envir=.qtlnetworkr)
      traitValname<-traitname[traitVal]
      #traitVal<-as.numeric(strsplit(svalue(widgets$trait),split=",")[[1]])
      colorVal<-strsplit(svalue(widgets$color),split=",")[[1]]
      criticalValue<-svalue(widgets$threshold)
      clean<-svalue(widgets$clean)
      LineType<-svalue(widgets$LineType)
      if(LineType!=TRUE) linetype<-"solid"
      xlabelform<-svalue(widgets$Xlabel)
      xlabelRot<-svalue(widgets$XlabelRot)
      xlabelFont<-as.numeric(svalue(widgets$XlabelFont))
      xlabelFontSize<-as.numeric(svalue(widgets$XlabelFontSize))
      ylabelFont<-as.numeric(svalue(widgets$YlabelFont))
      ylabelFontSize<-as.numeric(svalue(widgets$YlabelFontSize))
      notationFont<-as.numeric(svalue(widgets$NotationFont))
      notationFontSize<-as.numeric(svalue(widgets$NotationFontSize))
      LineWidth<-as.numeric(svalue(widgets$LineWidth))
      qnkfile<-get("qnkfile",envir=.qtlnetworkr)
      mapfile<-get("mapfile",envir=.qtlnetworkr)

      mapfile<-mapfile[-length(mapfile[,1]),]
      for(i in 1:length(mapfile[,1])){
             if(mapfile[i,1]=="_Chromosomes") chrnm<-as.numeric(mapfile[i,2])
             }
      chrlth<-0
      for(i in 1:length(mapfile[,1])){
               if(mapfile[i,1]=="_MarkerNumbers"|mapfile[i,1]=="_Markernumbers") {
                       for(j in 1:chrnm){chrlth[j]<-mapfile[i,(j+1)]}
                       }
               }
      chrlth<-as.numeric(chrlth)
      for(i in 1:length(mapfile[,1])){
                if(mapfile[i,1]=="Marker"|mapfile[i,1]=="marker") del<-i
                }
      mapfile<-mapfile[-(1:del),]
      mapfile<-mapfile[,-1]
      #delete annotations over
      mapfile<-mapfile[1:chrnm]
      inst<-mapfile
      lth<-length(mapfile[,1])
      for(u in 1:(chrnm-1)){
         for(i in u:(chrnm-1)){
            if(chrlth[u]!=lth) mapfile[(chrlth[u]+1):lth,(i+1)]<-inst[(chrlth[u]+1):lth,i]
                 }
                 inst<-mapfile
            }
      for(i in 1:chrnm){
               if(chrlth[i]!=lth) mapfile[(chrlth[i]+1):lth,i]<-NA
               mapfile[,i]<-as.numeric(mapfile[,i])
               }
      #change mapfile to map file style
      if(chr[1]!="all") mapfile<-mapfile[,chr]
      if(chr[1]!="all"&length(chr)==1) dim(mapfile)<-c(length(mapfile),1)
      i<-j<-k<-u<-1
      x<-xmax<-xxlabel<-0
      xatt<-xlabel<-0
      for(i in 1:length(mapfile[1,])){
                for(j in 1:length(mapfile[,1])){
                     if(is.na(mapfile[j,i])) break
                     else{
                      xmax<-xmax+mapfile[j,i]
                      if(is.na(xlabel[u])) xlabel[u]<-mapfile[j,i]
                      else xlabel[u]<-xlabel[u]+mapfile[j,i]
                      x[k]<-xmax;xxlabel[k]<-mapfile[j,i];k=k+1
                         }
                }
                      xatt[u]<-xmax
                      u<-u+1
       }
      #delete rep numbers
      if(chr[1]!="all"){
          if(length(chr)>1){
              a<-0
              for(i in 1:(length(chrlth[chr])-1)){
                  a[i]<-sum(chrlth[chr][1:i])
              }
              MI_x<-x[-a]
          }else MI_x<-x
          chr_start<-1
          chr_end<-0
          u<-2
          for(i in 2:length(chrlth)-1){
              chr_end[i]<-sum((chrlth-1)[1:i])
              chr_start[u]<-chr_end[i]+1
              u<-u+1
          }
          chr_end[i+1]<-sum(chrlth-1)
          chr_label<-chr
      #MI_x<-x
      }else{
          a<-0
          for(i in 1:(length(chrlth)-1)){
              a[i]<-sum(chrlth[1:i])
          }
          MI_x<-x[-a]
          chr_label<-1:chrnm
      }
      #map file done



      if(length(traitVal)==1) criticalValue<-"mean"
      traitnum<-grep("_trait",qnkfile[,1])[traitVal]
      #traitname<-as.character(qnkfile[traitnum,3])
      c_value<-as.numeric(qnkfile[traitnum+2,2])


      for(i in 1:length(qnkfile[traitnum[1]+3,])){
          if(qnkfile[traitnum[1]+3,i]==";") {k<-i-2;break}
      }
      MI_y<-matrix(0,nrow=k,ncol=length(traitVal))
      for(i in 1:length(traitVal)){
          MI_y[,i]<-as.numeric(qnkfile[traitnum[i]+4,1:k])
      }
      if(chr[1]!="all")
      {
          plot.index<-chr_start[chr][1]:chr_end[chr][1]
          if(length(chr)>1){
            for(i in 2:length(chr)){
                plot.index<-bind(plot.index,chr_start[chr][i]:chr_end[chr][i])
            }
          }
          MI.Y<-matrix(0,nrow=length(plot.index),ncol=length(traitVal))
          for(i in 1:length(traitVal)){
              MI.Y[,i]<-MI_y[,i][plot.index]
          }
          MI_y<-MI.Y
      }

      if(criticalValue=="mean"){
          crt_value<-mean(c_value)
          }else{
              stay<-which.min(c_value)
              std<-sort(c_value)
              index<-order(c_value)
              u<-2
              hold<-stay
              for(i in 2:length(std)){
                  if(abs(c_value[stay]-std[i])*30>5*ceiling(max(MI_y)/5)){
                      stay<-index[i]
                      hold[u]<-stay
                      u<-u+1
                  }
              }
              crt_value<-c_value[hold]
          }

      vplay<-grid.layout(nrow=2,height=unit(c(ceiling(length(traitVal)/5)+1,1),c("lines","null")))
        pushViewport(viewport(layout=vplay))
        pushViewport(viewport(layout.pos.row=1,name="row1"))
          floors<-ceiling(length(traitVal)/5)
          legendx<-unit(rep(c(0.1,1:4*0.175+0.1),each=floors),"npc")
          repnum<-rep(5,floors)
          instead<-1:floors-1
          instead<-rep(instead,5)
          ruler<-rep("lines",floors)
          ruler<-rep(ruler,5)
          legendy<-unit(instead,ruler)
          legendy<-rev(legendy)
          #for(i in 1:length(traitVal)){
              grid.segments(x0=legendx[1:length(traitVal)],x1=legendx[1:length(traitVal)]+unit(0.05,"npc"),
                       y0=legendy[1:length(traitVal)],y1=legendy[1:length(traitVal)],
                       gp=gpar(col=colorVal,lty=linetype))
              grid.text(traitValname,x=legendx[1:length(traitVal)]+unit(0.06,"npc"),
                        y=legendy[1:length(traitVal)],just=c("left"),
                        gp=gpar(cex=notationFontSize,font=notationFont))
          #}
        popViewport()
      pushViewport(viewport(layout.pos.row=2,name="row2"))
      pushViewport(plotViewport(c(5,4,1,3)))
      pushViewport(viewport(xscale=c(0,max(MI_x)),yscale=c(0,5*ceiling(max(MI_y)/5))))
      for(j in 1:length(traitVal)){
          grid.move.to(MI_x[1],MI_y[1,j],default.units="native")
          for(i in 1:(length(MI_x)-2)){
                 grid.line.to(MI_x[i+1],MI_y[i,j],default.units="native",gp=gpar(col=colorVal[j],lwd=LineWidth,if(LineType){lty=linetype[j]}))
                 grid.line.to(MI_x[i+1],MI_y[i+1,j],default.units="native",gp=gpar(col=colorVal[j],lwd=LineWidth,if(LineType){lty=linetype[j]}))
                 }
          grid.line.to(MI_x[length(MI_x)],MI_y[length(MI_y[,j]),j],default.units="native",gp=gpar(col=colorVal[j],lwd=LineWidth,if(LineType){lty=linetype[j]}))
      }
      #for(i in 1:length(xatt)){
        if(chr[1]=="all"|length(chr)>1){
               grid.segments(x0=unit(xatt,"native"),y0=unit(0,"npc"),
                             x1=unit(xatt,"native"),y1=unit(1,"npc"),
                             gp=gpar(col="white",lwd=LineWidth+0.2))
               grid.segments(x0=unit(xatt,"native"),y0=unit(0,"npc"),
                             x1=unit(xatt,"native"),y1=unit(1,"npc"))
        }
        #}
        if(criticalValue=="mean"){
          if(clean) grid.rect(x=unit(c(0,xatt[-length(xatt)]),"native"),y=unit(0,"npc"),
                                   width=unit(xatt-c(0,xatt[-length(xatt)]),"native"),height=unit(crt_value,"native"),
                                   just=c("left","bottom"),gp=gpar(fill="white"))
          grid.segments(x0=unit(0,"npc"),y0=unit(crt_value,"native"),
                        x1=unit(1,"npc"),y1=unit(crt_value,"native"))
          grid.text(crt_value,x=unit(1.01,"npc"),y=unit(crt_value,"native"),just="left")
        }else{
          if(clean) grid.rect(x=unit(c(0,xatt[-length(xatt)]),"native"),y=unit(0,"npc"),
                                   width=unit(xatt-c(0,xatt[-length(xatt)]),"native"),height=unit(min(c_value),"native"),
                                   just=c("left","bottom"),gp=gpar(fill="white"))
          grid.segments(x0=unit(0,"npc"),y0=unit(c_value,"native"),
                        x1=unit(1,"npc"),y1=unit(c_value,"native"),
                        gp=gpar(col=colorVal[1:length(c_value)],
                                if(LineType){lty=linetype[1:length(c_value)]}))
          grid.text(crt_value,x=unit(1.01,"npc"),y=unit(crt_value,"native"),just="left",
                    gp=gpar(col=colorVal[1:length(c_value)][hold]))
        }
        if(xlabelform=="Chromosome") {
            grid.xaxis(name="axis1",at=(c(0,xatt[-length(xatt)])+xatt)/2,
                       label=paste("Chr",chr_label,sep=""))
            grid.segments(x0=unit(0,"npc"),y0=unit(0,"npc"),
                        x1=unit(1,"npc"),y1=unit(0,"npc"))
        }else grid.xaxis(name="axis1",at=c(0,xatt),label=c(0,xlabel))
        grid.edit(gPath("axis1","labels"),rot=xlabelRot,gp=gpar(font=xlabelFont,cex=xlabelFontSize))
        grid.yaxis(name="axis2",at=0:5*ceiling(max(MI_y)/5),label=c(0:5*ceiling(max(MI_y)/5)))
        grid.edit(gPath("axis2","labels"),gp=gpar(font=ylabelFont,cex=ylabelFontSize))
        grid.segments(x0=unit(unique(x),"native"),y0=unit(0,"npc"),
                      x1=unit(unique(x),"native"),y1=unit(0.01,"npc"))
        grid.text("Value",x=unit(-3,"lines"), y=unit(0.54,"npc"),rot=90)
        grid.text("F",x=unit(-3,"lines"), y=unit(0.35,"npc"),rot=90,gp=gpar(font=3))

    }


  MII2DPlot<-function()
  {
    if(svalue(widgets$twodtrait)!="")
    {
          trait<-as.numeric(lapply(svalue(widgets$twodtrait),function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
          #trait<-as.numeric(svalue(widgets$twodtrait))
          colorkeyFont<-as.numeric(svalue(widgets$colorkeyFont))
          colorkeyFontSize<-as.numeric(svalue(widgets$colorkeyFontSize))
          ThresholdFont<-as.numeric(svalue(widgets$thresholdFont))
          ThresholdFontSize<-as.numeric(svalue(widgets$thresholdFontSize))
          leftBottomFont<-as.numeric(svalue(widgets$leftBottomFont))
          leftBottomFontSize<-as.numeric(svalue(widgets$leftBottomFontSize))


          qnkfile<-get("qnkfile",envir=.qtlnetworkr)

          traitnum<-grep("_trait",qnkfile[,1])[trait]
          traitname<-as.character(qnkfile[traitnum,3])
          trait2d<-grep("_plot_interval_interaction",qnkfile[,1])[trait]
          c_value<-as.numeric(qnkfile[trait2d+1,2])
          semico<-grep(";",qnkfile[trait2d+2,])-1
          num<-qnkfile[traitnum+3,1:semico+1]
          num<-as.numeric(num)
          lth<-length(num)

          u=1;int_v<-0
          for(i in 1:(lth-1)){
                    if(num[i]!=(num[i+1]-1)){int_v[u]<-i+1;u<-u+1}
          }

          int_lth<-lth+length(int_v)
          x<-y<-1:int_lth
          gr<-expand.grid(x=x,y=y)
          TwoD<-qnkfile[trait2d+2:(trait2d+lth+1),1:lth]
          std<-TwoD
          std[,(lth+1):int_lth ]<-TwoD[,1]
          std[,1:(int_v[1]-1)]<-TwoD[,1:(int_v[1]-1)]
          for(m in 2:length(int_v)){
              std[,(int_v[m-1]+1):(int_v[m]-1)]<-TwoD[,(int_v[m-1]-m+2):(int_v[m]-m)]
          }
          std[,(int_v[m]+1):int_lth]<-TwoD[,(int_v[m]-m+1):lth]
          std[,int_v]<-""
          Td<-std
          Td[(lth+1):int_lth,]<-std[1,]
          Td[1:(int_v[1]-1),]<-std[1:(int_v[1]-1),]
          for(n in 2:length(int_v)){
              Td[(int_v[n-1]+1):(int_v[n]-1),]<-std[(int_v[n-1]-n+2):(int_v[n]-n),]
          }
          Td[(int_v[n]+1):int_lth,]<-std[(int_v[n]-n+1):lth,]
          Td[int_v,]<-""
          zz<-0
          u<-1
          for(m in 1:int_lth){
              for(n in 1:int_lth){
                  zz[u]<-Td[m,n];u<-u+1
              }
          }
          gr$z<-zz
          top<-as.numeric(max(gr$z))
          vplay<-grid.layout(2,1,heights=unit(c(2,1),c("lines","null")))
          pushViewport(viewport(layout=vplay))
          pushViewport(viewport(layout.pos.row=1))
          colorkey <- function(colors){
          n <- 100
          breakss <- seq(0.1,0.9,length=51)
          xleft <- breakss[-51]
          xright <- breakss[-1]
          x<-unit((xleft+xright)/2,"npc")
          y<-unit(0,"npc")
          col <- colors
          grid.rect(x,y,gp=gpar(col=col,fill=col),height=unit(1,"lines"),
                    width=unit(0.016,"npc"),just=c("center","bottom"))
          grid.rect(0.5,0,height=unit(1,"lines"),width=unit(0.8,"npc"),just=c("center","bottom"))
          }
          colorkey(rev(heat.colors(50)))
          grid.text(round(0:4*top/4,digits=1),x=unit(seq(0.1,0.9,length=5),"npc"),y=unit(-0.5,"lines")
                    ,gp=gpar(font=colorkeyFont,cex=colorkeyFontSize)
                    )
          cv<-c_value/top*0.8+0.1
          grid.lines(x = unit(c(cv, cv), "npc"),y = unit(c(0.5, 0.7), "npc"))
          grid.text(paste("Threshold =",c_value,sep=" "),x=unit(cv,"npc")+unit(1,"mm"),
                    y=unit(0.5,"npc")+unit(1,"mm"),just=c("left","bottom")
                    ,gp=gpar(font=ThresholdFont,cex=ThresholdFontSize)
                    )
          popViewport()
          pushViewport(viewport(layout.pos.row=2))
          #pushViewport(plotViewport(c(2,2,0,0)))
          TwoDplot<-levelplot(z~x*y,gr,col.regions=rev(heat.colors(50)),
                              xlab="interval",ylab="interval",colorkey=F,
                              xlim=range(x),ylim=range(y),
                              scales=list(at=round(0:5*max(x)/5),labels=round(0:5*max(x)/5),
                                          cex=leftBottomFontSize,font=leftBottomFont))
          print(TwoDplot,newpage=F)
          popViewport()
    }
  }
  QTLPlot<-function()
  {
    if(svalue(widgets$QTLtrait)=="") stop("Select traits...")
    if(svalue(widgets$QTLcolor)=="") stop("Select colors...")
    if(svalue(widgets$QTLchr)=="") stop("You should select at least one chromosome...")
      traitVal<-as.numeric(lapply(strsplit(svalue(widgets$QTLtrait),split=", ")[[1]],function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
      #traitVal<-as.numeric(strsplit(svalue(widgets$QTLtrait),split=",")[[1]])
      colorVal<-strsplit(svalue(widgets$QTLcolor),split=",")[[1]]
      chr<-strsplit(svalue(widgets$QTLchr),split=",")[[1]]
      if(chr[1]!="all") chr<-as.numeric(chr)
      criticalValue<-svalue(widgets$QTLthreshold)
      clean<-svalue(widgets$QTLclean)
      LineType<-svalue(widgets$QTLlineType)
      if(LineType!=TRUE) linetype<-"solid"
      xlabelform<-svalue(widgets$QTLXlabel)
      xlabelRot<-svalue(widgets$QTLXlabelRot)
      xlabelFont<-as.numeric(svalue(widgets$QTLXlabelFont))
      xlabelFontSize<-as.numeric(svalue(widgets$QTLXlabelFontSize))
      ylabelFont<-as.numeric(svalue(widgets$QTLYlabelFont))
      ylabelFontSize<-as.numeric(svalue(widgets$QTLYlabelFontSize))
      notationFont<-as.numeric(svalue(widgets$QTLNotationFont))
      notationFontSize<-as.numeric(svalue(widgets$QTLNotationFontSize))
      LineWidth<-as.numeric(svalue(widgets$QTLlineWidth))
      mapfile<-get("mapfile",envir=.qtlnetworkr)
      qnkfile<-get("qnkfile",envir=.qtlnetworkr)

        mapfile<-mapfile[-length(mapfile[,1]),]
        for(i in 1:length(mapfile[,1])){
               if(mapfile[i,1]=="_Chromosomes") chrnm<-as.numeric(mapfile[i,2])
               }
        chrlth<-0
        for(i in 1:length(mapfile[,1])){
                 if(mapfile[i,1]=="_MarkerNumbers"|mapfile[i,1]=="_Markernumbers") {
                         for(j in 1:chrnm){chrlth[j]<-mapfile[i,(j+1)]}
                         }
                 }
        chrlth<-as.numeric(chrlth)
        for(i in 1:length(mapfile[,1])){
                  if(mapfile[i,1]=="Marker"|mapfile[i,1]=="marker") del<-i
                  }
        mapfile<-mapfile[-(1:del),]
        mapfile<-mapfile[,-1]
        #delete annotations over
        mapfile<-mapfile[1:chrnm]
        inst<-mapfile
        lth<-length(mapfile[,1])
        for(u in 1:(chrnm-1)){
           for(i in u:(chrnm-1)){
              if(chrlth[u]!=lth) mapfile[(chrlth[u]+1):lth,(i+1)]<-inst[(chrlth[u]+1):lth,i]
                   }
                   inst<-mapfile
              }
        for(i in 1:chrnm){
                 if(chrlth[i]!=lth) mapfile[(chrlth[i]+1):lth,i]<-NA
                 mapfile[,i]<-as.numeric(mapfile[,i])
                 }
        #change mapfile to map file style
        if(chr[1]!="all") mapfile<-mapfile[,chr]
        if(chr[1]!="all"&length(chr)==1) dim(mapfile)<-c(length(mapfile),1)
        i<-j<-k<-u<-1
        x<-xmax<-xxlabel<-0
        xatt<-xlabel<-0
        for(i in 1:length(mapfile[1,])){
                  for(j in 1:length(mapfile[,1])){
                       if(is.na(mapfile[j,i])) break
                       else{
                        xmax<-xmax+mapfile[j,i]
                        if(is.na(xlabel[u])) xlabel[u]<-mapfile[j,i]
                        else xlabel[u]<-xlabel[u]+mapfile[j,i]
                        x[k]<-xmax;xxlabel[k]<-mapfile[j,i];k=k+1
                           }
                  }
                        xatt[u]<-xmax
                        u<-u+1
         }
        #map file done

        if(length(traitVal)==1) criticalValue<-"mean"
        traitValnum<-grep("_plot_QTL",qnkfile[,1])[traitVal]
        traitValname<-as.character(qnkfile[grep("_trait",qnkfile[,1])[traitVal],3])
        c_value<-as.numeric(qnkfile[traitValnum+1,2])
        for(i in 1:length(qnkfile[traitValnum[1]+2,])){
            if(qnkfile[traitValnum[1]+2,i]==";") {k<-i-2;break}
        }
        qtl_intvl<-qtl_dis<-qtl_value<-qtl_x<-matrix(0,nrow=k,ncol=length(traitVal))
        for(i in 1:length(traitVal)){
            qtl_intvl[,i]<-as.numeric(qnkfile[traitValnum[i]+2,1:k+1])
            qtl_dis[,i]<-as.numeric(qnkfile[traitValnum[i]+3,1:k+1])
            qtl_value[,i]<-as.numeric(qnkfile[traitValnum[i]+4,1:k])
        }

        if(chr[1]!="all"){
            confirmChrRow<-grep("^_plot_interval$",qnkfile[,1])[traitVal]+2
            semico<-grep(";",qnkfile[confirmChrRow[1],])-2
            chrInfo<-as.numeric(qnkfile[confirmChrRow[1],1:semico+1])
            u<-2;k<-1;chr_start<-1;chr_end<-0
            for(i in 1:(length(chrInfo)-1)){
                      if(chrInfo[i]!=(chrInfo[i+1]-1)){
                          chr_start[u]<-i+1;u<-u+1
                          chr_end[k]<-i;k<-k+1
                      }
            }
            chr_end[k]<-length(chrInfo)


            qtl_chr<-0
            chr_pos<-0
            for(i in 1:length(chr)){
                if(i==1){
                  qtl_chr<-which(qtl_intvl[,1]>=chrInfo[chr_start[chr[i]]]&qtl_intvl[,1]<=chrInfo[chr_end[chr[i]]])
                  chr_pos<-chr_start[chr[i]]:chr_end[chr[i]]
                }else{
                  qtl_chr<-bind(qtl_chr,which(qtl_intvl[,1]>=chrInfo[chr_start[chr[i]]]&qtl_intvl[,1]<=chrInfo[chr_end[chr[i]]]))
                  chr_pos<-bind(chr_pos,chr_start[chr[i]]:chr_end[chr[i]])
                }
            }

            qtl_intvl_sd<-qtl_dis_sd<-qtl_value_sd<-matrix(0,nrow=length(qtl_chr),ncol=length(traitVal))
            for(i in 1:length(traitVal)){
                qtl_intvl_sd[,i]<-qtl_intvl[qtl_chr,i]
                qtl_dis_sd[,i]<-qtl_dis[qtl_chr,i]
                qtl_value_sd[,i]<-qtl_value[qtl_chr,i]
            }
            qtl_intvl<-qtl_intvl_sd
            qtl_dis<-qtl_dis_sd
            qtl_value<-qtl_value_sd
            rowlth<-length(qtl_chr)
            if(length(chr)>1){
              chr_del<-chrlth[chr][1]
              for(i in 2:length(chr)) chr_del[i]<-sum(chrlth[chr][1:i])
              uniq_x<-x[-chr_del]
            }else uniq_x<-x
            chr_label<-chr
            qtl_x<-0
            u<-1;k<-1
            for(j in chrInfo[chr_pos]){
                for(m in 1:rowlth){
                    if(qtl_intvl[m,1]==j) {qtl_x[u]<-uniq_x[k]+qtl_dis[m,1];u<-u+1}
                }
                k<-k+1
            }
        }else{
            #for(i in 1:length(traitVal)){
                chr_label<-1:chrnm
                u<-1
                qtl_x<-0
                for(j in 1:(length(x)-1)){
                    for(m in 1:k){
                        if(qtl_intvl[m,1]==j) {qtl_x[u]<-x[j]+qtl_dis[m,1];u<-u+1}
                    }
                }
            #}
        }


        if(criticalValue=="mean"){
            crt_value<-mean(c_value)
        }else{
            stay<-which.min(c_value)
            std<-sort(c_value)
            index<-order(c_value)
            u<-2
            hold<-stay
            for(i in 2:length(std)){
                if(abs(c_value[stay]-std[i])*30>5*ceiling(max(qtl_value)/5)){
                    stay<-index[i]
                    hold[u]<-stay
                    u<-u+1
                }
            }
            crt_value<-c_value[hold]
        }

        floors<-ceiling(length(traitVal)/5)
        legendx<-unit(rep(c(0.1,1:4*0.175+0.1),each=floors),"npc")
        repnum<-rep(5,floors)
        instead<-1:floors-1
        instead<-rep(instead,5)
        ruler<-rep("lines",floors)
        ruler<-rep(ruler,5)
        legendy<-unit(instead,ruler)
        legendy<-rev(legendy)

        vplay<-grid.layout(nrow=2,height=unit(c(ceiling(length(traitVal)/5)+1,1),c("lines","null")))
        pushViewport(viewport(layout=vplay))
        pushViewport(viewport(layout.pos.row=1,name="row1"))

          #for(i in 1:length(traitVal)){
              grid.segments(x0=legendx[1:length(traitVal)],x1=legendx[1:length(traitVal)]+unit(0.05,"npc"),
                       y0=legendy[1:length(traitVal)],y1=legendy[1:length(traitVal)],
                       gp=gpar(col=colorVal,lty=linetype))
              grid.text(traitValname,x=legendx[1:length(traitVal)]+unit(0.06,"npc"),
                        y=legendy[1:length(traitVal)],just=c("left"),
                        gp=gpar(cex=notationFontSize,font=notationFont))
          #}
        popViewport()
        pushViewport(viewport(layout.pos.row=2,name="row2"))
        pushViewport(plotViewport(c(5,4,1,3)))
        pushViewport(viewport(xscale=c(0,max(qtl_x)),yscale=c(0,5*ceiling(max(qtl_value)/5))))

        for(i in 1:length(traitVal)){
            grid.lines(x=unit(qtl_x,"native"),y=unit(qtl_value[,i],"native"),
                       gp=gpar(col=colorVal[i],lwd=LineWidth,if(LineType) lty=linetype[i]))

        }
        #for(i in 1:length(xatt)){
        if(chr[1]=="all"|length(chr)>1){       
               grid.segments(x0=unit(xatt,"native"),y0=unit(0,"npc"),
                             x1=unit(xatt,"native"),y1=unit(1,"npc"),
                             gp=gpar(col="white",lwd=0.2+LineWidth))
               grid.segments(x0=unit(xatt,"native"),y0=unit(0,"npc"),
                             x1=unit(xatt,"native"),y1=unit(1,"npc"))
        }
        #}
        if(criticalValue=="mean"){
          if(clean) grid.rect(x=unit(c(0,xatt[-length(xatt)]),"native"),y=unit(0,"npc"),
                                   width=unit(xatt-c(0,xatt[-length(xatt)]),"native"),height=unit(crt_value,"native"),
                                   just=c("left","bottom"),gp=gpar(fill="white"))
          grid.segments(x0=unit(0,"npc"),y0=unit(crt_value,"native"),
                        x1=unit(1,"npc"),y1=unit(crt_value,"native"))
          grid.text(crt_value,x=unit(1.02,"npc"),y=unit(crt_value,"native"),just="left")
        }else{
          if(clean) grid.rect(x=unit(c(0,xatt[-length(xatt)]),"native"),y=unit(0,"npc"),
                                   width=unit(xatt-c(0,xatt[-length(xatt)]),"native"),height=unit(min(c_value),"native"),
                                   just=c("left","bottom"),gp=gpar(fill="white"))
          grid.segments(x0=unit(0,"npc"),y0=unit(c_value,"native"),
                        x1=unit(1,"npc"),y1=unit(c_value,"native"),
                        gp=gpar(col=colorVal[1:length(c_value)],
                                if(LineType) lty=linetype[1:length(c_value)]))
          grid.text(crt_value,x=unit(1.02,"npc"),y=unit(crt_value,"native"),just="left",
                    gp=gpar(col=colorVal[1:length(c_value)][hold]))
        }
        if(xlabelform=="Chromosome") {
            grid.xaxis(name="axis1",at=(c(0,xatt[-length(xatt)])+xatt)/2,
                       label=paste("Chr",chr_label,sep=""))
            grid.segments(x0=unit(0,"npc"),y0=unit(0,"npc"),
                        x1=unit(1,"npc"),y1=unit(0,"npc"))
        }else grid.xaxis(name="axis1",at=c(0,xatt),label=c(0,xlabel))
        grid.edit(gPath("axis1","labels"),rot=xlabelRot,gp=gpar(font=xlabelFont,cex=xlabelFontSize))
        grid.yaxis(name="axis2",at=0:5*ceiling(max(qtl_value)/5),label=c(0:5*ceiling(max(qtl_value)/5)))
        grid.edit(gPath("axis2","labels"),gp=gpar(font=ylabelFont,cex=ylabelFontSize))
        grid.segments(x0=unit(unique(x),"native"),y0=unit(0,"npc"),
                      x1=unit(unique(x),"native"),y1=unit(0.01,"npc"))
        grid.text("Value",x=unit(-3,"lines"), y=unit(0.54,"npc"),rot=90)
        grid.text("F",x=unit(-3,"lines"), y=unit(0.35,"npc"),rot=90,gp=gpar(font=3))
  }

  QTLeffectPlot<-function(){
      trait<-as.numeric(lapply(strsplit(svalue(widgets$Effectrait),split=",")[[1]],function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
      #trait<-which(is.element(strsplit(svalue(widgets$Effectrait),split=", ")[[1]],get("traitname",envir=.qtlnetworkr)))
      #print(get("traitname",envir=.qtlnetworkr))
      #print(svalue(widgets$Effectrait))
      #print(trait)
      #widgets$Effectrait
      XlabelRot<-as.numeric(svalue(widgets$EffectXlabelRot))
      XlabelFont<-as.numeric(svalue(widgets$EffectXlabelFont))
      XlabelFontSize<-as.numeric(svalue(widgets$EffectXlabelFontSize))
      YlabelFont<-as.numeric(svalue(widgets$EffectYlabelFont ))
      YlabelFontSize<-as.numeric(svalue(widgets$EffectYlabelFontSize ))
      NotationFont<-as.numeric(svalue(widgets$EffectNotationFont ))
      NotationFontSize<-as.numeric(svalue(widgets$EffectNotationFontSize ))
      MinY<-svalue(widgets$EffectMinY)
      MaxY<-svalue(widgets$EffectMaxY)

      qnkfile<-get("qnkfile",envir=.qtlnetworkr)

      color<-c("blue","blueviolet","brown","cyan","green","magenta","yellow","tomato","slateblue2","dodgerblue","grey")
      pchIndex<-rep(21:25,3)
      Qe_start<-grep("^_QTL_effect$",qnkfile[,1])[trait]+1
      Qe_end<-grep("^_QTL_heritability$",qnkfile[,1])[trait]-1
      Qe_ptest<-grep(";",qnkfile[Qe_start,])-1
      Apos<-grep("^A$",qnkfile[Qe_start,])
      AEpos<-grep("^AE",qnkfile[Qe_start,])
      Avalue<-qnkfile[(Qe_start+1):Qe_end,Apos]
      Avalue<-as.numeric(Avalue)
      #Apvalue<-qnkfile[(Qe_start[1]+1):Qe_end[1],Apos+2]
      AEvalue<-qnkfile[(Qe_start+1):Qe_end[1],AEpos]
      AEpvalue<-qnkfile[(Qe_start+1):Qe_end[1],AEpos+2]
      AEvalue<-apply(AEvalue,2,function(item){
                      item<-as.numeric(item)
                      })
      colnames(AEvalue)<-qnkfile[Qe_start,AEpos]
      rownames(AEvalue)<-qnkfile[(Qe_start+1):Qe_end,1]

      AEpvalue<-apply(AEpvalue,c(1,2),function(item){
                      x<-ifelse(item<0.05,TRUE,FALSE)
                      })
      ##detect D
      Dpos<-grep("^D$",qnkfile[Qe_start,])
      if(length(Dpos)==1)
      {
      DEpos<-grep("^DE",qnkfile[Qe_start,])
      Dvalue<-qnkfile[(Qe_start+1):Qe_end,Dpos]
      Dvalue<-as.numeric(Dvalue)
      #Apvalue<-qnkfile[(Qe_start[1]+1):Qe_end[1],Apos+2]
      DEvalue<-qnkfile[(Qe_start+1):Qe_end[1],DEpos]
      DEpvalue<-qnkfile[(Qe_start+1):Qe_end[1],AEpos+2]
      DEvalue<-apply(DEvalue,2,function(item){
                      item<-as.numeric(item)
                      })
      colnames(DEvalue)<-qnkfile[Qe_start,DEpos]
      rownames(DEvalue)<-qnkfile[(Qe_start+1):Qe_end,1]

      DEpvalue<-apply(DEpvalue,c(1,2),function(item){
                      x<-ifelse(item<0.05,TRUE,FALSE)
                      })
      DDEvalue<-DEvalue+Dvalue
      }

      AAEvalue<-AEvalue+Avalue
      if(MinY!="") {Miny<-as.numeric(MinY)
                    }else{
                        if(length(Dpos)!=1) {
                        MinY<-floor(min(Avalue,AAEvalue))
                        }else{
                        MinY<-floor(min(Avalue,AAEvalue,Dvalue,DDEvalue))
                        }
                    }
      if(MaxY!="") {MaxY<-as.numeric(MaxY)
                    }else{
                        if(length(Dpos)!=1) {
                        MaxY<-ceiling(max(Avalue,AAEvalue))
                        }else{
                        MaxY<-ceiling(max(Avalue,AAEvalue,Dvalue,DDEvalue))
                        }
                    }

      vplay<-grid.layout(ncol=2,widths=unit(c(8.5,1.5),c("null","null")))
      pushViewport(viewport(layout=vplay))
      pushViewport(viewport(layout.pos.col=2,name="col2"))
      #heights<-convertHeight(stringHeight("A"),"npc")
      legendy<-unit(19:1/21,"npc")
      legendx<-unit(rep(0,19),"npc")
      grid.points(x=legendx[1:(length(AEvalue[1,])+1)],y=legendy[1:(length(AEvalue[1,])+1)],
                  pch=c(20,pchIndex),gp=gpar(col=c("red",color),fill=c("red",color)))
      grid.points(x=legendx[2:3+length(AEvalue[1,])],y=legendy[2:3+length(AEvalue[1,])],
                  pch=c(19,1))
      #grid.text(c("Detected","Undetected"),
      #          x=legendx[2:3+length(AEvalue[1,])]+unit(0.1,"npc"),
      #          y=legendy[2:3+length(AEvalue[1,])],just="left",
      #          gp=gpar(cex=NotationFontSize,font=NotationFont))
      #grid.text(c("A",colnames(AEvalue)),
      #          x=legendx[1:(length(AEvalue[1,])+1)]+unit(0.1,"npc"),
      #          y=legendy[1:(length(AEvalue[1,])+1)],just="left",
      #          gp=gpar(cex=NotationFontSize,font=NotationFont))
      grid.text(c("QTL",paste("Q+QE",1:length(AEvalue[1,]),sep="")),
                x=legendx[1:(length(AEvalue[1,])+1)]+unit(0.1,"npc"),
                y=legendy[1:(length(AEvalue[1,])+1)],just="left",
                gp=gpar(cex=NotationFontSize,font=NotationFont))


      popViewport()

      pushViewport(viewport(layout.pos.col=1,name="col1"))
      pushViewport(plotViewport(c(4,4,2,1)))
      pushViewport(viewport(xscale=c(0,length(Avalue)),yscale=c(MinY,MaxY)))
      grid.segments(x0=unit(0,"npc"),y0=unit(0,"native"),
                    x1=unit(1,"npc"),y1=unit(0,"native"))
      #apply(AEvalue,2,function(item){
      #     grid.points(x=unit(1:length(Avalue)-0.5,"native"),y=unit(item,"native"),
      #            pch=21,gp=gpar(col="red",fill="red"))
      #})
      posA<-ifelse(length(Dpos)==1,0.66,0.5)
      if(length(Dpos)==1) posD<-0.33
      for(i in 1:length(AEpos)){
          for(j in 1:length(AEpvalue[,1])){
              if(AEpvalue[j,i]) {grid.points(x=unit(1:length(Avalue)-posA,"native")[j],y=unit(AAEvalue[j,i],"native"),
                                pch=pchIndex[i],gp=gpar(col=color[i], fill=color[i]))
                                }else{
                                grid.points(x=unit(1:length(Avalue)-posA,"native")[j],y=unit(AAEvalue[j,i],"native"),
                                pch=pchIndex[i],gp=gpar(col=color[i]))
                                }
          }
      }
      if(length(Dpos)==1){
          for(i in 1:length(DEpos)){
              for(j in 1:length(DEpvalue[,1])){
                  if(DEpvalue[j,i]) {grid.points(x=unit(1:length(Avalue)-posD,"native")[j],y=unit(DEvalue[j,i],"native"),
                                    pch=pchIndex[i],gp=gpar(col=color[i], fill=color[i]))
                                    }#else{
                                    #grid.points(x=unit(1:length(Avalue)-posD,"native")[j],y=unit(DEvalue[j,i],"native"),
                                    #pch=pchIndex[i],gp=gpar(col=color[i]))
                                    #}
              }
          }
      }

      #for(i in 1:length(AEpos)){
      #    grid.points(x=unit(1:length(Avalue)-0.5,"native"),y=unit(AEvalue[,i],"native"),
      #                pch=pchIndex[i],gp=gpar(col=color[i],fill=color[i]))
      #}
      #for(i in 1:length(AEpos)){
      #    for(j in 1:length(AEpvalue[,1])){
      #        if(!AEpvalue[j,i]) grid.points(x=unit(1:length(Avalue)-0.5,"native")[j],
      #                                       y=unit(AEvalue[j,i],"native"),pch=4)
      #    }
      #}
      grid.points(x=unit(1:length(Avalue)-posA,"native"),y=unit(Avalue,"native"),
                  pch=20,gp=gpar(col="red",fill="red"))
      if(length(Dpos)==1) grid.points(x=unit(1:length(Dvalue)-posD,"native"),y=unit(Dvalue,"native"),
                                      pch=20,gp=gpar(col="red",fill="red"))
      grid.xaxis(name="xaxis",at=1:length(Avalue)-0.5,
                 label=rownames(AEvalue))
      grid.edit(gPath("xaxis","labels"),rot=XlabelRot,gp=gpar(font=XlabelFont,cex=XlabelFontSize))
      grid.segments(x0=unit(0,"npc"),y0=unit(0,"npc"),
                    x1=unit(1,"npc"),y1=unit(0,"npc"))
      grid.yaxis(name="yaxis",at=round(0:5*(MaxY-MinY)/5,1)+MinY,label=round(0:5*(MaxY-MinY)/5,1)+MinY)
      grid.edit(gPath("yaxis","labels"),gp=gpar(font=YlabelFont,cex=YlabelFontSize))
      grid.text("QTL",x=unit(0.5,"npc"),y=unit(-3,"lines"))
      grid.text("Effect",x=unit(-3,"lines"),y=unit(0.5,"npc"),rot=90)
      grid.segments(x0=unit(1:(length(Avalue)-1),"native"),y0=unit(0,"npc"),
                    x1=unit(1:(length(Avalue)-1),"native"),y1=unit(1,"npc"),
                    gp=gpar(lty="dashed"))
      grid.segments(x0=unit(length(Avalue),"native"),y0=unit(0,"npc"),
                    x1=unit(length(Avalue),"native"),y1=unit(1,"npc"))
      if(length(Dpos)==1) grid.text(rep(c("A","D"),each=c(length(Avalue),length(Dvalue))),
                                    x=unit(c(1:length(Avalue)-posA,1:length(Avalue)-posD),"native"),
                                    y=unit(1,"npc")+unit(1,"lines"))
      popViewport()

  }




  epiPlot<-function()
  {
    trait<-as.numeric(lapply(svalue(widgets$epitrait),function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
    #trait<-as.numeric(svalue(widgets$epitrait))
    blank<-as.numeric(svalue(widgets$blank))
    colorkeyFont<-as.numeric(svalue(widgets$epi.colorkeyFont))
    colorkeyFontSize<-as.numeric(svalue(widgets$epi.colorkeyFontSize))
    ThresholdFont<-as.numeric(svalue(widgets$epi.thresholdFont))
    ThresholdFontSize<-as.numeric(svalue(widgets$epi.thresholdFontSize))
    leftBottomFont<-as.numeric(svalue(widgets$epi.leftBottomFont))
    leftBottomFontSize<-as.numeric(svalue(widgets$epi.leftBottomFontSize))
    rightTopFont<-as.numeric(svalue(widgets$epi.rightTopFont))
    rightTopFontSize<-as.numeric(svalue(widgets$epi.rightTopFontSize))

    mapfile<-get("mapfile",envir=.qtlnetworkr)
    qnkfile<-get("qnkfile",envir=.qtlnetworkr)

    mapfile<-mapfile[-length(mapfile[,1]),]
    for(i in 1:length(mapfile[,1])){
           if(mapfile[i,1]=="_Chromosomes") chrnm<-as.numeric(mapfile[i,2])
    }
    chrlth<-0
    for(i in 1:length(mapfile[,1])){
             if(mapfile[i,1]=="_MarkerNumbers"|mapfile[i,1]=="_Markernumbers") {
                     for(j in 1:chrnm){chrlth[j]<-mapfile[i,(j+1)]}
             }
    }
    chrlth<-as.numeric(chrlth)
    for(i in 1:length(mapfile[,1])){
              if(mapfile[i,1]=="Marker"|mapfile[i,1]=="marker") del<-i
    }
    mapfile<-mapfile[-(1:del),]
    mapfile<-mapfile[,-1]
    mapfile<-mapfile[1:chrnm]
    inst<-mapfile
    lth<-length(mapfile[,1])
    for(u in 1:(chrnm-1)){
       for(i in u:(chrnm-1)){
          if(chrlth[u]!=lth) mapfile[(chrlth[u]+1):lth,(i+1)]<-inst[(chrlth[u]+1):lth,i]
       }
       inst<-mapfile
    }
    for(i in 1:chrnm){
             if(chrlth[i]!=lth) mapfile[(chrlth[i]+1):lth,i]<-NA
             mapfile[,i]<-as.numeric(mapfile[,i])
    }
    #map information over


    traitnum<-grep("_trait",qnkfile[,1])[trait]
    traitname<-as.character(qnkfile[traitnum,3])
    trait_epi<-grep("_plot_epistasis",qnkfile[,1])[trait]
    c_value<-as.numeric(qnkfile[trait_epi+1,2])
    epix_lth<-grep(";",qnkfile[trait_epi+2,])-1
    epiy_lth<-grep(";",qnkfile[trait_epi+3,])-1
    semico<-grep(";",qnkfile[traitnum+3,])-2
    num<-qnkfile[traitnum+3,1:semico+1]
    num<-as.numeric(num)
    lth<-length(num)
    u=1;int_v<-0
    for(i in 1:(lth-1)){
              if(num[i]!=(num[i+1]-1)){int_v[u]<-i+1;u<-u+1}
    }

    #get interval_x
    epi_intvlx<-as.numeric(qnkfile[trait_epi+2,2:epix_lth])
    #get interval_y
    epi_intvly<-as.numeric(qnkfile[trait_epi+3,2:epiy_lth])

    epi_disx<-as.numeric(qnkfile[trait_epi+4,2:epix_lth])
    epi_disy<-as.numeric(qnkfile[trait_epi+5,2:epiy_lth])
    epi_xyp<-qnkfile[(trait_epi+6):(trait_epi+5+length(epi_intvly)),1:length(epi_intvlx)]

    numx<-0
    u<-2
    l<-1
    epi_xint<-epi_xlbl<-1
    for(i in 1:(length(epi_intvlx)-1)){
         if(epi_intvlx[i]!=epi_intvlx[i+1]&(epi_intvlx[i]+1)!=epi_intvlx[i+1]){
                 numx<-numx+1;epi_xint[l]<-epi_xlbl[u]<-i;epi_xlbl[u+1]<-i+1;u<-u+2
                 l<-l+1
         }
    }
    epi_xlbl[u]<-length(epi_intvlx)
    epi_xint[l]<-length(epi_intvlx)


    numy<-0
    k<-2
    l<-1
    epi_yint<-epi_ylbl<-1
    for(i in 1:(length(epi_intvly)-1)){
         if(epi_intvly[i]!=epi_intvly[i+1]&(epi_intvly[i]+1)!=epi_intvly[i+1]){
         numy<-numy+1;epi_yint[l]<-epi_ylbl[k]<-i;epi_ylbl[k+1]<-i+1;k<-k+2;l<-l+1
         }
    }
    epi_ylbl[k]<-length(epi_intvly)
    epi_yint[l]<-length(epi_intvly)

    stead<-epi_xyp
    stead[,(length(epi_disx)+1):(length(epi_disx)+blank*(length(epi_xint)-1))]<-"0"
    stead[,]<-""

    epi_xy<-stead
    epi_xy[(length(epi_disy)+1):(length(epi_disy)+blank*(length(epi_yint)-1)),]<-"0"
    epi_xy[,]<-""

    stead[,1:epi_xint[1]]<-epi_xyp[,1:epi_xint[1]]
    for(i in 2:length(epi_xint)){
         stead[,(epi_xint[i-1]+1+blank*(i-1)):(epi_xint[i]+blank*(i-1))]<-epi_xyp[,(epi_xint[i-1]+1):epi_xint[i]]
    }

    epi_xy[1:epi_yint[1],]<-stead[1:epi_yint[1],]
    for(i in 2:length(epi_yint)){
         epi_xy[(epi_yint[i-1]+1+blank*(i-1)):(epi_yint[i]+blank*(i-1)),]<-stead[(epi_yint[i-1]+1):epi_yint[i],]
    }

    #get dis info
    mark_dis<-ceiling(mapfile)
    u<-1
    epi_mkdis<-0
    for(i in 1:length(mark_dis[1,])){
       for(j in 1:length(mark_dis[,1])){
         if(j==1) {epi_mkdis[u]<-0;u<-u+1}
           else if(!is.na(mark_dis[j,i])) {epi_mkdis[u]<-epi_mkdis[u-1]+mark_dis[j,i];u<-u+1}
       }
    }

    epi_x<-epi_y<-0
    for(i in 1:length(epi_intvlx)){
             epi_x[i]<-epi_mkdis[epi_intvlx[i]]+epi_disx[i]
    }

    for(i in 1:length(epi_intvly)){
             epi_y[i]<-epi_mkdis[epi_intvly[i]]+epi_disy[i]
    }

    epi_choosechr<-epi_xlbl[1:(length(epi_xlbl)/2)*2-1]#get odd number
    epi_choosechry<-epi_ylbl[1:(length(epi_ylbl)/2)*2-1]

    #get chr info
    k<-1
    epi_xlabel_up<-1
    epi_chrint<-c(1,int_v,length(num))
    for(i in 1:(length(epi_chrint)-1)){
        for(j in 1:length(epi_choosechr)){
              if(epi_intvlx[epi_choosechr[j]]>=num[epi_chrint[i]]
                  &epi_intvlx[epi_choosechr[j]]<num[epi_chrint[i+1]])
                    {epi_xlabel_up[k]<-paste("CH",i,sep="");k<-k+1}
        }
    }

    k<-1
    epi_ylabel_right<-1
    for(i in 1:(length(epi_chrint)-1)){
        for(j in 1:length(epi_choosechry)){
              if(epi_intvly[epi_choosechry[j]]>=num[epi_chrint[i]]
                  &epi_intvly[epi_choosechry[j]]<num[epi_chrint[i+1]])
                    {epi_ylabel_right[k]<-paste("CH",i,sep="");k<-k+1}
        }
    }
    epi_xat_dn<-1
    for(i in 1:length(epi_xlbl)){
         epi_xat_dn[i]<-epi_xlbl[i]+floor((i-1)/2)* blank
    }
    epi_xlabel_dn<-epi_x[epi_xlbl]
    epi_xat_up<-(epi_xat_dn[1:(length(epi_xat_dn)/2)*2-1]+epi_xat_dn[1:(length(epi_xat_dn)/2)*2])/2

    epi_yat_left<-1
    for(i in 1:length(epi_ylbl)){
         epi_yat_left[i]<-epi_ylbl[i]+floor((i-1)/2)* blank
    }
    epi_ylabel_left<-epi_y[epi_ylbl]
    epi_yat_right<-(epi_yat_left[1:(length(epi_yat_left)/2)*2-1]+epi_yat_left[1:(length(epi_yat_left)/2)*2])/2

    epx<-1:length(epi_xy[1,])
    epy<-1:length(epi_xy[,1])
    epi<-expand.grid(x=epx,y=epy)
    u<-1
    epiz<-0
    for(i in 1:length(epi_xy[,1])){
       for(j in 1:length(epi_xy[1,])){
          epiz[u]<-epi_xy[i,j];u<-u+1
          }
    }
    epi$z<-epiz
    epiplot<-levelplot(z~x*y,epi,cuts=50,col.regions=rev(heat.colors(50)),
                      xlab="",ylab="",xlim=range(epx),ylim=range(epy),colorkey=F,
                      scales=list(x=list(at=c(epi_xat_dn),
                                  labels=c(epi_xlabel_dn),rot=90),
                                  y=list(at=c(epi_yat_left),
                                  labels=c(epi_ylabel_left)),
                                  cex=leftBottomFontSize,font=leftBottomFont)
    )
    colorkey <- function(colors){
    n <- 100
    breakss <- seq(0.1,0.9,length=51)
    xleft <- breakss[-51]
    xright <- breakss[-1]
    x<-unit((xleft+xright)/2,"npc")
    y<-unit(1,"lines")
    col <- colors
    grid.rect(y,x,gp=gpar(col=col,fill=col),width=unit(1,"lines"),
              height=unit(0.016,"npc"),just=c("left","center"))
    grid.rect(unit(1,"lines"),0.5,width=unit(1,"lines"),height=unit(0.8,"npc"),just=c("left","center"))
    }
    top<-as.numeric(max(epi$z))
    vplay<-grid.layout(1,3,widths=unit(c(3,1,1),c("lines","null","lines")))
    pushViewport(viewport(layout=vplay))
    pushViewport(viewport(layout.pos.col=1))
    colorkey(rev(heat.colors(50)))
    grid.text(round(0:4*top/4,digits=1),y=unit(seq(0.1,0.9,length=5),"npc"),x=unit(0.5,"lines"),
              gp=gpar(font=colorkeyFont,cex=colorkeyFontSize),rot=90
    )
    cv<-c_value/top
    grid.lines(y = unit(c(cv*0.8+0.1, cv*0.8+0.1), "npc"),x = unit(c(2.0, 2.2), "lines"))
    grid.text(paste("Threshold =",round(c_value,digits=1),sep=" "),y=unit(cv*0.8+0.1,"npc")+unit(1,"mm"),
              x=unit(2.5,"lines")+unit(1,"mm"),just=c("left","top"),rot=90,
              gp=gpar(font=ThresholdFont,cex=ThresholdFontSize)
    )
    popViewport()
    pushViewport(viewport(layout.pos.col=2))
    print(epiplot,newpage=F)
    trellis.focus("panel", 1, 1, clip.off=T, highlight = FALSE)
    panel.axis(at=epi_xat_up,labels=epi_xlabel_up,side=c("top"),outside=T,rot=0,
               text.font=rightTopFont,text.cex=rightTopFontSize,line.lty="dotted")
    panel.axis(at=epi_yat_right,labels=epi_ylabel_right,side=c("right"),outside=T,
               text.font=rightTopFont,text.cex=rightTopFontSize,line.lty="dotted")
    trellis.unfocus()
  }
  QTLNetworkPlot<-function(){
    #trait<-which(is.element(strsplit(svalue(widgets$QTLNtrait),split=", ")[[1]],get("traitname",envir=.qtlnetworkr)))
    trait<-as.numeric(lapply(strsplit(svalue(widgets$QTLNtrait),split=", ")[[1]],function(item){
                x<-grep(item,get("traitname",envir=.qtlnetworkr))
            }))
    #trait<-as.numeric(svalue(widgets$QTLNtrait))
    chr<-svalue(widgets$QTLNchr)
    chrWidth<-as.numeric(svalue(widgets$chrWidth))/1000
    epiLineWidth<-as.numeric(svalue(widgets$epiLineWidth))
    Text<-svalue(widgets$Text)
    TextPosition<-as.numeric(svalue(widgets$textpos))/100
    TextFont<-as.numeric(svalue(widgets$TextFont))
    TextFontSize<-as.numeric(svalue(widgets$TextFontSize))
    chrFont<-as.numeric(svalue(widgets$chrFont))
    chrFontSize<-as.numeric(svalue(widgets$chrFontSize))
    notationFont<-as.numeric(svalue(widgets$notationFont))
    notationFontSize<-as.numeric(svalue(widgets$notationFontSize))
    SymbolSize<-as.numeric(svalue(widgets$symbol))



    qnkfile<-get("qnkfile",envir=.qtlnetworkr)
    mapfile<-get("mapfile",envir=.qtlnetworkr)
    traitlth<-get("traitlth",envir=.qtlnetworkr)


    trait_qtl<-grep("_plot_QTL",qnkfile[,1])
    trait_epi<-grep("_plot_epistasis",qnkfile[,1])
    traitlth<-length(grep("_trait",qnkfile[,1]))

    ###manage linkage map file
    mapfile<-mapfile[-length(mapfile[,1]),]
    for(i in 1:length(mapfile[,1])){
           if(mapfile[i,1]=="_Chromosomes") chrnm<-as.numeric(mapfile[i,2])
           }
    chrlth<-0
    for(i in 1:length(mapfile[,1])){
             if(mapfile[i,1]=="_MarkerNumbers"|mapfile[i,1]=="_Markernumbers") {
                     for(j in 1:chrnm){chrlth[j]<-mapfile[i,(j+1)]}
                     }
             }
    chrlth<-as.numeric(chrlth)
    for(i in 1:length(mapfile[,1])){
              if(mapfile[i,1]=="Marker"|mapfile[i,1]=="marker") del<-i
              }
    mapfile<-mapfile[-(1:del),]
    mapfile<-mapfile[,-1]
    #delete annotations over
    mapfile<-mapfile[1:chrnm]
    inst<-mapfile
    lth<-length(mapfile[,1])
    for(u in 1:(chrnm-1)){
       for(i in u:(chrnm-1)){
          if(chrlth[u]!=lth) mapfile[(chrlth[u]+1):lth,(i+1)]<-inst[(chrlth[u]+1):lth,i]
               }
               inst<-mapfile
          }
    for(i in 1:chrnm){
             if(chrlth[i]!=lth) mapfile[(chrlth[i]+1):lth,i]<-NA
             mapfile[,i]<-as.numeric(mapfile[,i])
             }
    #change mapfile to map file style
    i<-j<-k<-u<-1
    x<-xmax<-xxlabel<-0
    xatt<-xlabel<-0
    for(i in 1:length(mapfile[1,])){
              for(j in 1:length(mapfile[,1])){
                   if(is.na(mapfile[j,i])) break
                   else{
                    xmax<-xmax+mapfile[j,i]
                    if(is.na(xlabel[u])) xlabel[u]<-mapfile[j,i]
                    else xlabel[u]<-xlabel[u]+mapfile[j,i]
                    x[k]<-xmax;xxlabel[k]<-mapfile[j,i];k=k+1
                       }
              }
                    xatt[u]<-xmax
                    u<-u+1
     }
    #map file done
    
    u<-n<-1
    delete<-0
    if(length(trait_epi)!=traitlth){
        for(i in 1:traitlth){
            if(trait_epi[u]==(trait_qtl[i]+5)){if(u<length(trait_epi)) u<-u+1}else{delete[n]<-i;n<-n+1}
        }
    }
    allow_trait<-1:traitlth
    if(delete[1]!=0) allow_trait<-allow_trait[-delete]

    Qe_start<-grep("_QTL_effect",qnkfile[,1])[trait]+2
    Qe_end<-grep("_QTL_heritability",qnkfile[,1])[trait]-1
    Qe<-Qe_end-Qe_start+1      #各个性状的QTL数量
    sum.Qe<-0
    for(i in 1:length(Qe))
    {
        sum.Qe[i]<-sum(Qe[1:i])
    }

    for(i in 1:length(trait)){
        if(i == 1){
          Qe_ptest<-grep(";",qnkfile[Qe_start[i],])-1
          Qe_Apos<-match("A",qnkfile[Qe_start[i]-1,])
          Qe_AEpos<-grep("AE",qnkfile[Qe_start[i]-1,])
          if(length(Qe_AEpos)>0) lth_QeAE1<-length(Qe_AEpos)
          Qe_Dpos<-match("D",qnkfile[Qe_start[i]-1,])
          Qe_DEpos<-grep("DE",qnkfile[Qe_start[i]-1,])
          if(length(Qe_DEpos)>0) lth_QeDE1<-length(Qe_DEpos)
        }else{
          Qe_ptest<-bind(Qe_ptest,(grep(";",qnkfile[Qe_start[i],])-1))
          Qe_Apos<-bind(Qe_Apos,match("A",qnkfile[Qe_start[i]-1,]))
          Qe_AEpos<-bind(Qe_AEpos,grep("AE",qnkfile[Qe_start[i]-1,]))
          Qe_Dpos<-bind(Qe_Dpos,match("D",qnkfile[Qe_start[i]-1,]))
          Qe_DEpos<-bind(Qe_DEpos,grep("DE",qnkfile[Qe_start[i]-1,]))
        }
    }

    u<-1
    Qe_chr_info<-Qe_pos_st<-Qe_pos_ed<-Qe_pos<-Qe_A<-Qe_D<-check_Qechr<-0
    Qe_mkst<-Qe_mked<-0
    #if(length(trait)>1){
        for(j in 1:length(trait)){
            for(i in Qe_start[j]:Qe_end[j]){
                   check_Qechr[u]<-qnkfile[i,1]
                   Qe_chr_info[u]<-strsplit(qnkfile[i,1],"-")[[1]][1]
                   Qe_pos_st[u]<-strsplit(qnkfile[i,4],"-")[[1]][1]
                   Qe_pos_ed[u]<-strsplit(qnkfile[i,4],"-")[[1]][2]
                   Qe_mkst[u]<-strsplit(qnkfile[i,2],"-")[[1]][1]
                   Qe_mked[u]<-strsplit(qnkfile[i,2],"-")[[1]][2]
                   Qe_pos[u]<-qnkfile[i,3]
                   if(!is.na(Qe_Dpos[j])){
                       if(qnkfile[i,Qe_Dpos[j]+2]<=0.05) Qe_D[u]<-1
                       else Qe_D[u]<-0
                   }
                   if(!is.na(Qe_Apos[j])){
                       if(qnkfile[i,Qe_Apos[j]+2]<=0.05) Qe_A[u]<-1
                       else Qe_A[u]<-0
                   }
                   u<-u+1
            }
        }
    #}
    Qe_pos_lth<-as.numeric(Qe_pos_ed)-as.numeric(Qe_pos_st)
    Qe_chr_info<-as.numeric(Qe_chr_info)
    if(length(trait)>1) chr_num <- rep(1:length(trait),Qe_end-Qe_start+1)

    #get AE
    if(!is.na(Qe_Apos[1])){
      #
      u<-1
      Qe_AE<-matrix(0,nrow=sum(Qe_end+1-Qe_start),ncol=length(Qe_AEpos))
      for(k in 1:length(trait)){
          st<-ifelse(k==1,1,lth_QeAE1+1)
          ed<-ifelse(k==1,lth_QeAE1,length(Qe_AEpos))
          for(i in Qe_start[k]:Qe_end[k]){
              for(j in st:ed){
                if(qnkfile[i,Qe_AEpos[j]+2]<=0.05) Qe_AE[u,j]<-1
                else Qe_AE[u,j]<-0
              }
              u<-u+1
          }
      }
      Qe_Ae<-0
      Qe_Ae[1:length(Qe_AE[,1])]<-0
      for(i in 1:length(Qe_AE[,1])){
          for(j in 1:length(Qe_AE[1,])){
            if(Qe_AE[i,j]==1) {Qe_Ae[i]<-1;break}
          }
      }
    }

    if(!is.na(Qe_Dpos[1])){
      u<-1
      Qe_DE<-matrix(2,nrow=sum(Qe_end+1-Qe_start),ncol=length(Qe_DEpos))
      for(k in 1:length(trait)){
          st<-ifelse(k==1,1,lth_QeDE1+1)
          ed<-ifelse(k==1,lth_QeDE1,length(Qe_DEpos))
          for(i in Qe_start[k]:Qe_end[k]){
              for(j in st:ed){
                if(qnkfile[i,Qe_DEpos[j]+2]<=0.05) Qe_DE[u,j]<-1
                else Qe_DE[u,j]<-0
              }
              u<-u+1
          }
      }
      Qe_De<-0
      Qe_De[1:length(Qe_DE[,1])]<-0
      for(i in 1:length(Qe_DE[,1])){
          for(j in 1:length(Qe_DE[1,])){
            if(Qe_DE[i,j]==1) {Qe_De[i]<-1;break}
          }
      }
    }

    #get color of qtl with:QTL without AE,red,QTL with AE,blue,AE without QTLe,green
    col_A<-0
    for(i in 1:length(Qe_A)){
        if(Qe_A[i]==1&Qe_Ae[i]==0) col_A[i]<-"red"
        else if(Qe_A[i]==1&Qe_Ae[i]==1) col_A[i]<-"blue"
        else if(Qe_A[i]==0&Qe_Ae[i]==1) col_A[i]<-"green"
    }
    if(!is.na(Qe_Dpos[1])){
        col_D<-0
        for(i in 1:length(Qe_D)){
            if(Qe_D[i]==1&Qe_De[i]==0) col_D[i]<-"red"
            else if(Qe_D[i]==1&Qe_De[i]==1) col_D[i]<-"blue"
            else if(Qe_D[i]==0&Qe_De[i]==1) col_D[i]<-"green"
        }
    }



   traitWithEe<-which(is.element(trait,allow_trait))
  if(length(traitWithEe)==0)
  {
      check_chr<-check_Qechr
      chr_info<-Qe_chr_info
      pos_st<-as.numeric(Qe_pos_st)
      po_ed<-as.numeric(Qe_pos_ed)
      mkst<-Qe_mkst
      mked<-Qe_mked
      pos<-as.numeric(Qe_pos)
  }
  if(length(traitWithEe)>0){
    #Ee info
    Eetrait<-0
    for(i in 1:length(traitWithEe)){
        Eetrait[i]<-which(trait[traitWithEe][i]==allow_trait)
    }

    Ee_start<-grep("_epistasis_effect",qnkfile[,1])[Eetrait]+2
    Ee_end<-grep("_epistasis_heritability",qnkfile[,1])[Eetrait]-1
    Ee<-Ee_end-Ee_start+1 #各个性状的上位性数量
    sum.Ee<-0
    for(i in 1:length(Ee))
    {
        sum.Ee[i]<-sum(Ee[1:i])
    }


    u<-1
    check_Eechr<-check_Eechr2<-Ee_chr_info<-Ee_chr_info2<-0
    Ee_pos_st<-Ee_pos_st2<-Ee_pos_ed<-Ee_pos_ed2<-Ee_pos<-Ee_pos2<-Ee_A<-0
    Ee_mkst<-Ee_mkst2<-Ee_mked<-Ee_mked2<-0
    for(j in 1:length(trait[traitWithEe])){
        for(i in Ee_start[j]:Ee_end[j]){
               check_Eechr[u]<-qnkfile[i,1]
               check_Eechr2[u]<-qnkfile[i,5]
               Ee_chr_info[u]<-strsplit(qnkfile[i,1],"-")[[1]][1]
               Ee_chr_info2[u]<-strsplit(qnkfile[i,5],"-")[[1]][1]
               Ee_mkst[u]<-strsplit(qnkfile[i,2],"-")[[1]][1]
               Ee_mkst2[u]<-strsplit(qnkfile[i,6],"-")[[1]][1]
               Ee_mked[u]<-strsplit(qnkfile[i,2],"-")[[1]][2]
               Ee_mked2[u]<-strsplit(qnkfile[i,6],"-")[[1]][2]
               Ee_pos_st[u]<-strsplit(qnkfile[i,4],"-")[[1]][1]
               Ee_pos_st2[u]<-strsplit(qnkfile[i,8],"-")[[1]][1]
               Ee_pos_ed[u]<-strsplit(qnkfile[i,4],"-")[[1]][2]
               Ee_pos_ed2[u]<-strsplit(qnkfile[i,8],"-")[[1]][2]
               Ee_pos[u]<-qnkfile[i,3]
               Ee_pos2[u]<-qnkfile[i,7]
               u<-u+1
        }
    }

    check_Eechr<-bind(check_Eechr,check_Eechr2)
    Ee_chr_info<-bind(Ee_chr_info,Ee_chr_info2)
    Ee_pos_st<-bind(Ee_pos_st,Ee_pos_st2)
    Ee_pos_ed<-bind(Ee_pos_ed,Ee_pos_ed2)
    Ee_pos<-as.numeric(bind(Ee_pos,Ee_pos2))
    Ee_mkst<-bind(Ee_mkst,Ee_mkst2)
    Ee_mked<-bind(Ee_mked,Ee_mked2)
    Ee_pos_lth<-as.numeric(Ee_pos_ed)-as.numeric(Ee_pos_st)

    check_chr<-bind(check_Qechr,check_Eechr)


        pos_st<-as.numeric(bind(Qe_pos_st,Ee_pos_st))
        pos_ed<-as.numeric(bind(Qe_pos_ed,Ee_pos_ed))
        pos<-as.numeric(bind(Qe_pos,Ee_pos))
        pos_lth<-bind(Qe_pos_lth,Ee_pos_lth)
        mkst<-bind(Qe_mkst,Ee_mkst)
        mked<-bind(Qe_mked,Ee_mked)
        chr_info<-as.numeric(bind(Qe_chr_info,Ee_chr_info))



    #####
    if(length(check_chr)>length(col_A)){
        col_A[(length(col_A)+1):length(check_chr)]<-"black"
        if(!is.na(Qe_Dpos[1]))  col_D[(length(col_D)+1):length(check_chr)]<-"black"
        for(j in traitWithEe)
        {
           if(which(j==traitWithEe)==1){
               for(i in c((sum(Qe)+1):(sum(Qe)+Ee[1]),(sum(Qe)+sum(Ee)+1):(sum(Qe)+sum(Ee)+Ee[1])))
               {
                  if(j==1)
                  {
                      if(is.element(check_chr[i],check_chr[1:Qe[1]])) col_A[i]<-col_A[which(check_chr[i]==check_chr[1:Qe[1]])]
                  }else
                  {
                      if(is.element(check_chr[i],check_chr[(Qe[j-1]+1):sum.Qe[j]])) col_A[i]<-col_A[which(check_chr[i]==check_chr[(Qe[j-1]+1):sum.Qe[j]])]
                  }
               }
           }else{
               for(i in c((sum(Qe)+sum.Ee[which(j==traitWithEe)-1]+1):(sum(Qe)+sum.Ee[which(j==traitWithEe)]),(sum(Qe)+sum.Ee[which(j==traitWithEe)-1]+sum(Ee)+1):(sum(Qe)+sum(Ee)+sum.Ee[which(j==traitWithEe)])))
               {
                  if(is.element(check_chr[i],check_chr[(sum.Qe[j-1]+1):sum.Qe[j]])) col_A[i]<-col_A[which(check_chr[i]==check_chr[(sum.Qe[j-1]+1):sum.Qe[j]])]
               }
           }
        }
    }


   # for(i in (sum(Qe)+1):length(check_chr))
    #{
    #    if(is.element(check_chr[i],check_chr[1:sum(Qe)])) col_A[i]<-col_A[which(check_chr[i]==check_chr[1:sum(Qe)])]
    #}

    #get EE
    for(i in 1:length(trait[traitWithEe])){
        if(i == 1){
        Ee_ptest<-grep(";",qnkfile[Ee_start[i],])-1
        Ee_AApos<-match("AA",qnkfile[Ee_start[i]-1,])
        Ee_AAEpos<-grep("AAE",qnkfile[Ee_start[i]-1,])
        if(length(Ee_AAEpos)>0) lth_EeAAE1<-length(Ee_AAEpos)
        }else{
        Ee_ptest<-bind(Ee_ptest,(grep(";",qnkfile[Ee_start[i],])-1))
        Ee_AApos<-bind(Ee_AApos,match("AA",qnkfile[Ee_start[i]-1,]))
        Ee_AAEpos<-bind(Ee_AAEpos,grep("AAE",qnkfile[Ee_start[i]-1,]))
        }
    }

    u<-1;Ee_AA<-0
    #Ee_AAE<-matrix(0,nrow=(Ee_end-Ee_start+1),ncol=length(Ee_AAEpos))
    Ee_AE<-0
    for(j in 1:length(trait[traitWithEe])){
        for(i in Ee_start[j]:Ee_end[j]){
            if(qnkfile[i,Ee_AApos[j]+2]<0.05) Ee_AA[u]<-1 else Ee_AA[u]<-0
            u<-u+1
        }
    }
    u<-1
    Ee_AAE<-matrix(0,nrow=sum(Ee_end+1-Ee_start),ncol=length(Ee_AAEpos))
      for(k in 1:length(trait[traitWithEe])){
          st<-ifelse(k==1,1,lth_EeAAE1+1)
          ed<-ifelse(k==1,lth_EeAAE1,length(Ee_AAEpos))
          for(i in Ee_start[k]:Ee_end[k]){
              for(j in st:ed){
                if(qnkfile[i,Ee_AAEpos[j]+2]<=0.05) Ee_AAE[u,j]<-1
                else Ee_AAE[u,j]<-0
              }
              u<-u+1
          }
      }
    Ee_AE<-apply(Ee_AAE,1,function(item){
               if(is.element(1,item))  n<-1 else n<-0
           })


    col_AA<-0
    for(i in 1:length(Ee_AA)){
        if(Ee_AA[i]==1&Ee_AE[i]==0) col_AA[i]<-"red"
        else if(Ee_AA[i]==1&Ee_AE[i]==1) col_AA[i]<-"blue"
        else if(Ee_AA[i]==0&Ee_AE[i]==1) col_AA[i]<-"green"
    }

    if(!is.na(Qe_Dpos[1])){
        Ee_ADpos<-match("AD",qnkfile[Ee_start-1,])
        Ee_DApos<-match("DA",qnkfile[Ee_start-1,])
        Ee_DDpos<-match("DD",qnkfile[Ee_start-1,])
        Ee_ADEpos<-grep("ADE",qnkfile[Ee_start-1,])
        Ee_DAEpos<-grep("DAE",qnkfile[Ee_start-1,])
        Ee_DDEpos<-grep("DDE",qnkfile[Ee_start-1,])
        Ee_AD<-Ee_DA<-Ee_DD<-Ee_ADE<-Ee_DAE<-Ee_DDE<-0
        u<-1
        for(k in 1:length(trait[traitWithEe])){
            for(i in Ee_start[k]:Ee_end[k]){
                if(qnkfile[i,Ee_ADpos+2]<0.05) Ee_AD[u]<-1 else Ee_AD[u]<-0
                if(qnkfile[i,Ee_DApos+2]<0.05) Ee_DA[u]<-1 else Ee_DA[u]<-0
                if(qnkfile[i,Ee_DDpos+2]<0.05) Ee_DD[u]<-1 else Ee_DD[u]<-0
                u<-u+1
                for(j in 1:length(Ee_ADEpos)){
                   if(qnkfile[i,Ee_ADEpos[j]+2]<=0.05) {Ee_ADE[i-Ee_start[k]+1]<-1;break}
                   else Ee_ADE[i-Ee_start[k]+1]<-0
                }
                for(j in 1:length(Ee_DAEpos)){
                   if(qnkfile[i,Ee_DAEpos[j]+2]<=0.05) {Ee_DAE[i-Ee_start[k]+1]<-1;break}
                   else Ee_DAE[i-Ee_start[k]+1]<-0
                }
                for(j in 1:length(Ee_DDEpos)){
                   if(qnkfile[i,Ee_DDEpos[j]+2]<=0.05) {Ee_DDE[i-Ee_start[k]+1]<-1;break}
                   else Ee_DDE[i-Ee_start[k]+1]<-0
                }
            }
        }
        col_AD<-col_DA<-col_DD<-0
        for(i in 1:length(Ee_AD)){
            if(Ee_AD[i]==1&Ee_ADE[i]==0) col_AD[i]<-"red"
            else if(Ee_AD[i]==1&Ee_ADE[i]==1) col_AD[i]<-"blue"
            else if(Ee_AD[i]==0&Ee_ADE[i]==1) col_AD[i]<-"green"
        }
        for(i in 1:length(Ee_DA)){
            if(Ee_DA[i]==1&Ee_DAE[i]==0) col_DA[i]<-"red"
            else if(Ee_DA[i]==1&Ee_DAE[i]==1) col_DA[i]<-"blue"
            else if(Ee_A[i]==0&Ee_DAE[i]==1) col_DA[i]<-"green"
        }
        for(i in 1:length(Ee_DD)){
            if(Ee_DD[i]==1&Ee_DDE[i]==0) col_DD[i]<-"red"
            else if(Ee_DD[i]==1&Ee_DDE[i]==1) col_DD[i]<-"blue"
            else if(Ee_DD[i]==0&Ee_DDE[i]==1) col_DD[i]<-"green"
        }
    }
  }

    ##data ready


    ###
    col_unit<-1/(length(unique(chr_info))+1)
    uniq_chr_info<-sort(unique(chr_info))
    misschr<-which(!is.element(1:max(uniq_chr_info),uniq_chr_info))
    adjust.chrinfo<-chr_info
    for(i in 1:length(chr_info))
    {
       if(which(chr_info[i]==sort(c(chr_info[i],misschr)))>1) adjust.chrinfo[i]<-chr_info[i]-which(chr_info[i]==sort(c(chr_info[i],misschr)))+1
    }
    myline<-c("dotted","dotdash","dashed","solid","431313")
    if(length(traitWithEe)) myLine<-rep(myline[1:length(Ee)],Ee)
     #####adjust
     #col_A[38]<-"black"   ##different traits
    # pos[38]<-pos[11]-1 #"1-3"
     #pos[14]<-pos[5]+1  ##4-7
     #pos[23]<-pos[5]-1.5 ##"4-7"
     #pos[22]<-pos[4]+1 ## "3-2"

    #legend
    vplay<-grid.layout(nrow=2,height=unit(c(1,9),c("null","null")))
    pushViewport(viewport(layout=vplay))
    pushViewport(viewport(layout.pos.row=1,name="row1"))
    mytextA<-c("QTL with A","QTL with AE","QTL with both A and AE")
    mytextI<-c("Epistasis with AA","Epistasis with AAE","Epistasis with both AA and AAE")
    mytextT<-c("grain rate","1000 grain weight","full grain number")
    mycol<-c("red","blue","green")
    m<-3;n<-length(trait)
    ver<-unit(c(0.75,0.5,0.25),"npc")
    x3<-rep(unit(0.75,"npc"),time=n)
    x2<-rep(unit(0.3,"npc"),time=m)
    x1<-rep(unit(0.02,"npc"),time=m)
    notpch<-c(22,24,21,23)
    grid.points(x=x1,y=ver,pch=20,gp=gpar(col=mycol,cex=SymbolSize))
    for(i in 1:m){
        grid.segments(x0=x2[i],x1=x2[i]+unit(0.05,"npc"),
                   y0=ver[i],y1=ver[i],
                   gp=gpar(col=mycol[i]))
    }
    grid.text(mytextA,x=x1+unit(0.02,"npc"),y=ver,just="left",
            gp=gpar(cex=notationFontSize,font=notationFont))
    grid.text(mytextI,x=x2+unit(0.07,"npc"),y=ver,just="left",
            gp=gpar(cex=notationFontSize,font=notationFont))
    if(n<5){
        trait.ver<-unit(c(0.75,0.5,0.25,0),"npc")
        grid.points(x=x3,y=trait.ver[1:n],pch=notpch[1:n],gp=gpar(cex=SymbolSize))
        grid.text("&",x=x3+unit(0.02,"npc"),y=trait.ver[1:n],just="left",gp=gpar(cex=SymbolSize))
        for(i in 1:n){
        grid.segments(x0=x3[i]+unit(0.05,"npc"),x1=x3[i]+unit(0.1,"npc"),
                      y0=trait.ver[i],y1=trait.ver[i],gp=gpar(lty=myline[i]))
        }
        grid.text(mytextT,x=x3+unit(0.12,"npc"),y=trait.ver[1:n],just="left",
                gp=gpar(cex=notationFontSize,font=notationFont))
    }
    
    popViewport()
    pushViewport(viewport(layout.pos.row=2,name="row2"))
    pushViewport(plotViewport(c(3,4,0,2)))

    for(i in 1:length(unique(chr_info)))
    {
        grid.rect(x=unit(0.5,"npc"),y=unit(col_unit*(i-1),"npc"),
                  #width=unit(pos_lth[drawchr][ord][j]*row_unit,"npc"),
                  height=unit(chrWidth,"npc"),
                  gp=gpar(col="darkgrey",fill="darkgrey"))
    }

    mypch<-c(15,17,16,18)
    ### QTL
    k<-1
    for(i in 1:length(Qe))
    {
        if(i==1)
        {
           grid.points(
                       x=unit(pos[1:Qe[i]]/xlabel[chr_info[1:Qe[i]]],"npc"),
                       y=unit(col_unit*(adjust.chrinfo[1:Qe[i]]-1),"npc"),
                       pch=mypch[i],gp=gpar(col=col_A[1:Qe[i]],cex=SymbolSize)
                       )
           if(length(traitWithEe)>0)
           {
               if(traitWithEe[k]==i)
               {
                   grid.points(
                               x=unit(pos[c(1:Ee[i],1:Ee[i]+sum(Ee))+sum(Qe)]/xlabel[chr_info[c(1:Ee[i],1:Ee[i]+sum(Ee))+sum(Qe)]],"npc"),
                               y=unit(col_unit*(adjust.chrinfo[c(1:Ee[i],1:Ee[i]+sum(Ee))+sum(Qe)]-1),"npc"),
                               pch=mypch[i],gp=gpar(col=col_A[c(1:Ee[i],1:Ee[i]+sum(Ee))+sum(Qe)],cex=SymbolSize)
                               )
                   k<-k+1
               }
           }
        }else{
           grid.points(
                       x=unit(pos[(sum.Qe[i-1]+1):sum.Qe[i]]/xlabel[chr_info[(sum.Qe[i-1]+1):sum.Qe[i]]],"npc"),
                       y=unit(col_unit*(adjust.chrinfo[(sum.Qe[i-1]+1):sum.Qe[i]]-1),"npc"),
                       pch=mypch[i],gp=gpar(col=col_A[(sum.Qe[i-1]+1):sum.Qe[i]],cex=SymbolSize)
                       )
               if(k==1){
                   if(length(traitWithEe)>0)
                   {
                   if(traitWithEe[k]==i)
                       {
                           grid.points(
                                       x=unit(pos[c(1:Ee[k],1:Ee[k]+sum(Ee))+sum(Qe)]/xlabel[chr_info[c(1:Ee[k],1:Ee[k]+sum(Ee))+sum(Qe)]],"npc"),
                                       y=unit(col_unit*(adjust.chrinfo[c(1:Ee[k],1:Ee[k]+sum(Ee))+sum(Qe)]-1),"npc"),
                                       pch=mypch[i],gp=gpar(col=col_A[c(1:Ee[k],1:Ee[k]+sum(Ee))+sum(Qe)],cex=SymbolSize)
                                       )
                           k<-k+1
                       }
                   }
               }else{
                   if(length(traitWithEe)>1)
                   {
                       if(traitWithEe[k]==i)
                       {
                           grid.points(
                                       x=unit(pos[c((sum.Ee[k-1]+1):sum.Ee[k],(sum.Ee[k-1]+1):sum.Ee[k]+sum(Ee))+sum(Qe)]/xlabel[chr_info[c((sum.Ee[k-1]+1):sum.Ee[k],(sum.Ee[k-1]+1):sum.Ee[k]+sum(Ee))+sum(Qe)]],"npc"),
                                       y=unit(col_unit*(adjust.chrinfo[c((sum.Ee[k-1]+1):sum.Ee[k],(sum.Ee[k-1]+1):sum.Ee[k]+sum(Ee))+sum(Qe)]-1),"npc"),
                                       pch=mypch[i],gp=gpar(col=col_A[c((sum.Ee[k-1]+1):sum.Ee[k],(sum.Ee[k-1]+1):sum.Ee[k]+sum(Ee))+sum(Qe)],cex=SymbolSize)
                                       )
                           k<-k+1
                       }
                   }
               }
        }
    }
    ###epistasis
    if(length(traitWithEe)>0)
    {
        if(any(adjust.chrinfo[sum(Qe)+1:sum(Ee)]==adjust.chrinfo[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]))
        {
            sameIdx<-which(adjust.chrinfo[sum(Qe)+1:sum(Ee)]==adjust.chrinfo[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))])
            grid.segments(x0=unit(pos[sum(Qe)+1:sum(Ee)][-sameIdx]/xlabel[chr_info[sum(Qe)+1:sum(Ee)]][-sameIdx],"npc"),
                          x1=unit(pos[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))][-sameIdx]/xlabel[chr_info[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]][-sameIdx],"npc"),
                          y0=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)]-1)[-sameIdx],"npc"),
                          y1=unit(col_unit*(adjust.chrinfo[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]-1)[-sameIdx],"npc"),
                          gp=gpar(col=col_AA[-sameIdx],lty=myLine[-sameIdx],lwd=epiLineWidth)
                          )
            grid.segments(x0=unit(pos[sum(Qe)+1:sum(Ee)][sameIdx]/xlabel[chr_info[sum(Qe)+1:sum(Ee)]][sameIdx],"npc"),
                          x1=unit(pos[sum(Qe)+1:sum(Ee)][sameIdx]/xlabel[chr_info[sum(Qe)+1:sum(Ee)]][sameIdx],"npc"),
                          y0=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1),"npc"),
                          y1=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1)+0.04,"npc"),
                          gp=gpar(col=col_AA[sameIdx],lty=myLine[sameIdx],lwd=epiLineWidth)
                          )
            grid.segments(x0=unit(pos[sum(Qe)+1:sum(Ee)][sameIdx]/xlabel[chr_info[sum(Qe)+1:sum(Ee)]][sameIdx],"npc"),
                          x1=unit(pos[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))][sameIdx]/xlabel[chr_info[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]][sameIdx],"npc"),
                          y0=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1)+0.04,"npc"),
                          y1=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1)+0.04,"npc"),
                          gp=gpar(col=col_AA[sameIdx],lty=myLine[sameIdx],lwd=epiLineWidth)
                          )
            grid.segments(x0=unit(pos[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))][sameIdx]/xlabel[chr_info[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]][sameIdx],"npc"),
                          x1=unit(pos[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))][sameIdx]/xlabel[chr_info[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]][sameIdx],"npc"),
                          y0=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1)+0.04,"npc"),
                          y1=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)][sameIdx]-1),"npc"),
                          gp=gpar(col=col_AA[sameIdx],lty=myLine[sameIdx],lwd=epiLineWidth)
                          )
        }else grid.segments(x0=unit(pos[sum(Qe)+1:sum(Ee)]/xlabel[chr_info[sum(Qe)+1:sum(Ee)]],"npc"),
                            x1=unit(pos[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]/xlabel[chr_info[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]],"npc"),
                            y0=unit(col_unit*(adjust.chrinfo[sum(Qe)+1:sum(Ee)]-1),"npc"),
                            y1=unit(col_unit*(adjust.chrinfo[sum(Qe)+(sum(Ee)+1):(2*sum(Ee))]-1),"npc"),
                            gp=gpar(col=col_AA,lty=myLine,lwd=epiLineWidth)
                            )
    }

    grid.text(paste(chr,uniq_chr_info,sep=""),x=unit(-1.5,"lines"),y=unit(col_unit*(1:length(uniq_chr_info)-1),"npc"),
              gp=gpar(cex=chrFontSize,font=chrFont))

    if(Text=="Yes"){
        dup<-which(duplicated(check_chr))
        undup<- which(!duplicated(check_chr))
        uniq_check_chr<-unique(check_chr)
        if (length(dup)>0){
            uniq_chrinfo<-chr_info[-dup]
            dup.pos<-pos[-dup]
        }else{
            uniq_chrinfo<-chr_info
            dup.pos<-pos
        }

        grid.text(uniq_check_chr,x=unit(dup.pos/xlabel[uniq_chrinfo],"npc"),
                  y=unit(col_unit*(adjust.chrinfo[undup]-1)-TextPosition,"npc"),
                  gp=gpar(cex=TextFontSize,font=TextFont))
    }
    popViewport(0)
  }
}