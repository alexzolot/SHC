#! /usr/bin/Rscript 
# Project  : 56_block_highchartsTS
# File     : 56_block_highchartsTS/56_block_highchartsTS.r
# Author   : Alex Zolotoviski
# Project  : 56_block_highchartsTS
# File     : 56_block_highchartsTS/56_block_highchartsTS.r
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 2015-05-17 08:59:19
# License  : GPL-2
###############################################################################

{ #== init ===
	rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=80, scipen=5)
	options(error= recover)    # options(error= NULL)  # options(error=dump) 

	options(datatable.print.nrows=200)

 	onWin= Sys.getenv('R_PLATFORM')==''
    if(onWin) setwd('M:/56_block_highchartsTS/clean')   

    source('global.R')  
    source('lib/funcs.R')  # , T, verbose=T)
	
} #--
##########################################################

m= fread('in/data.tsv')

hmd= function(...) HTML(markdownToHTML(..., fragment.only =T))

ui<-  fluidPage( h1('Dynamic synchronized time series with Shiny and  Highcharts JS, by Alex Zolot'),
        tabsetPanel(type = "pills", id='tsp1', position= "left", selected= "TS1"  #type = "tabs"  # Model Performance, Time Series"
                 , tabPanel('Data',dataTableOutput('dtable'))
                 , tabPanel('TS-fixed'
                                    ,  mainPanel(
                                            HTML(HC.header %+% '\n')
                                            , hmd(text ='Model independent stats `imp  ~ date` by  `treat`:')
                                            , HTML(build.1hChart.htm(scsv= ecof('dcast.data.table(m, day ~ treat, sum, value.var="imp")') %>%
                                                                        setkey(day)  %>% dtt2scsv
                                                                , ma= 'Model independent stats:  imps  by  treat'
                                                                , ylab= yy['imp']$label, add= F, div.id='HiCharts', yAxisLog=FALSE
                                                        , htm1templ = "www/hCharts.csv1.templ.htm")
                                                   )
                                            , HTML(build.1hChart.htm(scsv= ecof('dcast.data.table(m, date ~ pars+model, mean, value.var="auc")') %>%
                                                                    setkey(date) %>% dtt2scsv %>% gsub('NA|NaN', 'null',.)
                                                    , ma= 'Model dependent KPI: AUC  by  pars + model'
                                                    , ylab= yy['auc']$label, add= T, div.id='HiCharts', htm1templ = "www/hCharts.csv1.templ.htm"))
                                            #, hmd(text ='Dynamic output from shiny server:')
                                            , htmlOutput('hO')
                                        )
                        , id='tp1')

                        , tabPanel("TS-Interactive", sidebarLayout(sidebarPanel(
                                         selIn(selectizeInput,'kpi2', 'Oy value (KPI)', kpis.list, multiple = T, options=list(sortField=''))
                                        , hr()
                                        , selIn(,'byyTS2', 'By',  bys.list, multiple = T)
                                        , actionButton("go", "Go"), HTML('<hr/>') 
                                        , hmd(text ='Note: `AUC` was evaluated for`treat = 0`  only
													and aggregated over offer groups, so depends only on `date+par+model`')
                                        , width = '2', id='sb2')
                                ,  mainPanel(# h3('main Panel'),  #, uiOutput('hCharts')
                                         div(id= 'HiCharts2', class='HC_Block')
                                        , htmlOutput('hO2')
                                        , htmlOutput("log")  # http://shiny.rstudio.com/articles/action-buttons.html
                                   )
                             ) 
                         , id='tp2')))


server= function(input, output, pl= I) {o= output; i= input
                red= function(...) renderText(sf('<font  color="red"><b>%s</b></font >', ...))
                
              m= fread('in/data.tsv')
              #saveRDS(m, 'in/data.rds')
              #m= readRDS('in/m0.rds')
              o$dtable= renderDataTable(m[,cn('date treat ogroup	og	pars model  imp	clk	ctr auc	mse	nmse xe	nxe'), with=F])
              
              byy= list('pars', 'model')  #   byy= list('pars')
              y= 'nxe'  # y= list('nxe', 'mse')
              
              strr(byy)
              strr(m)
              
              m.wd= ecof('dcast.data.table(m, day ~ %s, mean, value.var="%s")', pasc(byy, coll='+'),  y) %>%
                      setkey(day)
        
        mm.htm= build.1hChart.htm(scsv= dtt2scsv(m.wd)
                , ma= sf('Model dependent KPI: Dynamic output from shiny server: %s  by  %s', yy[y]$label, pasc(byy))
                , ylab= yy[y]$label, add= T, htm1templ = "www/hCharts.csv1.templ.htm")
        

        o$hO= renderText(mm.htm)
           
        m.wd= eventReactive(input$go, { strr(i$kpi2); strr(i$byyTS2);
                    
              if(le(i$kpi2)== 0){s= 'Must be at least one y variable'; o$log= red(s); message(s); return(dtt(msg= s)) } # no y  
              if(0 && le(i$kpi2)==1){  # single y  
                                          if(le(i$byyTS2) > 0) {
                        ecof('dcast(m, day ~ %s, %s, value.var="%s")', pasc(i$byyTS2, coll='+')
                                 , if(kpis[i$kpi2]$additive) 'sum' else 'mean'
                                 , as.list(i$kpi2))
                    } else ecof('m[, list(%s= sumn(%1$s %s)), day]',  i$kpi2
                                , if(kpis[i$kpi2]$additive) '' else '* n)/sumn(n')
              } else {
                  res= dtt()
                              
                  L= llply(i$kpi2, function(y){catn(y)
                          u=  if(le(i$byyTS2) > 0) {
                                  strr(i$byyTS2)  
                                  strr(m)  
                                  ecof('dcast.data.table(m, day ~ %s, %s, value.var="%s")', pasc(i$byyTS2, coll='+')
                                           , if(kpis[y]$additive) 'sum' else 'mean',  y)
                              } else ecof('m[, list(%s= sumn(%1$s %s)), day]',  y
                                                          , if(kpis[y]$additive) '' else '* n)/sumn(n')   
                          setkey(u, day)
                          strr(u)
                          if(le(i$kpi2)>1)setnames(u, gna(,'day', u), sf('%s_%s', y, gna(,'day', u)))
                          strr(u)
                          u
                          res<<- if(le(res)==0) u else res[u]  # merge
                  })
                  res
           }})

           m.htm= eventReactive(i$go, ({
                            scsv= gsub('NA|NaN', 'null', dtt2scsv(m.wd()))
                            
                            strr(scsv)
                            strr(pasc(yy[i$kpi2]$label))
                            
                            build.1hChart.htm(scsv= scsv
                                    , ma= sf('%s  by  %s', pasc(yy[i$kpi2]$label, coll=', ')
                                                         , pasc(i$byyTS2, coll=', '))
                                    , ylab= pasc(yy[i$kpi2]$label), add= T
                                    , yAxisLog= FALSE  # i$yAxisLog
                                    , div.id='HiCharts2', htm1templ = "www/hCharts.csv1.templ.htm") # %>% 
                    }))
            o$hO2= renderText({m.htm()}) 
      output= o
    }
#  <---  dbl clk here --(for meetup) -----------------------------------------<<<
test.use.Shiny1choiseHCharts= function(){
    shinyApp(ui, server)
    shinyAppDir('.')
    use.Shiny1choiseHCharts()
}
For.Meetup= function(){
    use.Shiny1choiseHCharts()
    if(0) { # Aug 12, 2015
        'RS=/cygdrive/c/R/R-3.2.0/bin/x64/Rscript.exe
        $RS shiny.R' 
        
        # https://www.shinyapps.io/admin/#/dashboard
    	 libra(devtools)
         devtools::install_github('rstudio/shinyapps')
         library(shinyapps)
         shinyapps::setAccountInfo(name='alexzolot', token='6FC9BFE0E441A67200046D6D0B0F95D1', secret='GPxDhI+Rpv+lziX5NYBvLwzulpWuulhOabHInsZ1') 
         # Error in formatDL(rep.int(tag, length(val)), val, style = "list", width = width,  : 
         #   incorrect values of 'indent' and 'width'

         # sw("M:/56_block_highchartsTS/clean");  expl()
         shinyapps::deployApp()        
        
         # local Shiny server
         runApp(display.mode = "showcase")
            
         shinyApp(ui, server, options= list(display.mode = "showcase"))  
            
         #
        shiny::runGitHub( "SHC", "alexzolot")       
        shiny::runGitHub( "SHC", "alexzolot", 'pub')       
        
            
            `ssh googmete@googmeter.com
            </proc/version                                                                                                                                            ~
            Linux version 3.12.35.1418868052 (root@openvpn) (gcc version 4.4.7 20120313 (Red Hat 4.4.7-11) (GCC) ) #1 SMP Wed Dec 17 20:04:02 CST 2014
 
			`
    }
   
    
}

# For.Meetup()
shinyApp(ui, server)


