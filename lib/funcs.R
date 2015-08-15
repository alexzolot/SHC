
ecof= function(codf,..., v=T, exec=T, e=parent.frame()){code<-sf(codf,...); if(v) prr(sf('ecof("%s")', code));  if(exec)eval(parse(text=code), envir= e) }
pasc= function(..., coll= ',') pas(..., collapse= coll)

#file.copy('C:/Users/zolotovi/Dropbox/Public/csv.tmp/BHighchartsClass.TS.js','M:/56_block_highchartsTS/out/BHighchartsClass.TS.js')
#HC.header= readLines('M:/56_block_highchartsTS/highcharts.csv.templBlocks.htm')[5:39] # :32] prr(HC.header)
HC.header= readLines('www/HC_Header.htm') # :32] prr(HC.header)
selIn=  function(cntrl=selectInput, ...) div(cntrl(...), class = "span2");

#bys= dtt(v=cn('ctrl_grp treat og ogroup pars model'), label= cn('Control Group|Treatment|og|Offer Group|Batch/Online|Model', '\\|'))  %>% setkey(v)
bys= dtt(v=cn('treat og ogroup pars model'), label= cn('Treatment|og|Offer Group|Batch/Online|Model', '\\|'))  %>% setkey(v)
yy= kpis=  dtt(v= cn('imp clk ctr mse xe nmse nxe auc')
                , label= cn('Impressions|Clicks|CTR|mse|Cross-entropy|Norm.mse|Norm.cross-entr.|AUC', '\\|')
                , aggrf= c(sum, sum, mean, mean, mean, mean, mean, mean)
                , additive= c(T, T, F, F, F, F, F, F)) %>% 
            setkey(v)

# transform dtt to lists for shiny
bys.list= {L= list(); bys[, L[label]<<- v ]; L}
kpis.list= {L= list(); kpis[, L[label]<<- v ]; L}


#' build htm for  1 highchart from csv file or string
build.1hChart.htm= function(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
        , scsv= pasc(readLines(fcsv), coll="\\\\\\\\n' +'")   # , coll="' +\\\n'"
        , ma='nxe by  pars + model', ylab='nxe', yAxisLog= FALSE, add= TRUE, div.id='HiCharts'
        , htm1templ='M:/56_block_highchartsTS/hCharts.csv1.templ.htm', forShiny=T) {
    #pasc2= function(...) I  # pasc
    htm= gsub('_csv', scsv, readLines(htm1templ)) %>%
            gsub('_ma', ma, . ) %>%
            #gsub('_yAxisLog', if(yAxisLog) '_yAxisLog */' else '', .)  %>%
            gsub('_ylab', ylab, . ) %>% gsub('HighCharts', div.id, . ) 
    
    
    #if(yAxisLog) htm= sub('_yAxisLog','_yAxisLog */', htm) 
    if(yAxisLog) htm= sub('linear', 'logarithmic', htm) 

    if(forShiny) htm= gsub('\\\\\\\\','\\\\', htm) %+% '\n'
    
    return( if(add) gre2(, 'new_block', htm) else htm )
}
test.build.1hChart.htm= function(){
    cat(r<- build.1hChart.htm(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
                    , ma='nxe by  pars + model', ylab='nxe', add= TRUE
            # , scsv=pasc(readLines(fjs1templ), coll="\\\n")
            # , fjs1templ='M:/56_block_highchartsTS/hCharts.csv1.templ.js'
            ))
    cat(r<- build.1hChart.htm(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
                    , ma='nxe by  pars + model', ylab='nxe', add= FALSE, forShiny=F
                    ))
    
    1:3 %>% I
    1:3 %>% c
}  

#' build js for  1 highchart from csv file or string
build.1hChart.js= function(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
        , scsv= pasc(readLines(fcsv), coll="\\\n")
        , ma='nxe by  pars + model', ylab='nxe', yAxisLog=FALSE, add= TRUE, div.id='hCharts'
        , fjs1templ='M:/56_block_highchartsTS/hCharts.csv1.templ.js') {
    #pasc2= function(...) I  # pasc
    gsub('_csv', scsv, fjs1templ) %>%
            gsub('_ma', ma, . ) %>%
            if(yAxisLog) gsub('_yAxisLog', '_yAxisLog*/',.) else I %>%
            gsub('_ylab', ylab, . ) %>% gsub('hCharts', div.id, . ) %>%
            gsub('_new_block', if(add) '0' else '1',. )
}
test.build.1hChart.js= function(){
    cat(build.1hChart.js(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
                    , ma='nxe by  pars + model', ylab='nxe', add= TRUE
            # , scsv=pasc(readLines(fjs1templ), coll="\\\n")
            # , fjs1templ='M:/56_block_highchartsTS/hCharts.csv1.templ.js'
            ))
    
    m4.js2= build.1hChart.js(fcsv='m:/62_TMoRod/out/nxe.by.prmod.wd.csv'
            , ma='nxe by  pars + model', ylab='nxe', add= TRUE)
    identical(m4.js, m4.js2)
    # [1] TRUE
    
    m4m.js2= build.1hChart.js(fcsv='m:/62_TMoRod/out/nmse.by.prmod.wd.csv'
            , ma='nmse by pars + model', ylab='nmse', add= TRUE
            , fjs1templ='M:/56_block_highchartsTS/hCharts.csv1.templ.js')
    
    identical(m4m.js, m4m.js2)
    # [1] FALSE
    
    identical(gsub(' ', '', m4m.js), gsub(' ','', m4m.js2))
    # [1] TRUE
    
    
    u= sapply(1:nchar(m4m.js2), function(i)substr(m4m.js, i,i)!= substr(m4m.js2, i,i))  # i=2
    i= which(u)[1]    # [1] 664
    substr(m4m.js, i,i+20)   # [1] " pars + model'} \\\n   "
    substr(m4m.js2, i,i+20)  # [1] "pars + model'} \\\n    "
}    



#' dtt to string csv consumed by highcharts
dtt2scsv= function(dtta= mse.by.prmod.wd) c(na(dtta) %>% pasc, apply(dtta, 1, pasc)) %>% pasc(coll="\\\\n' +'")
test.dtt2scsv= function(){    
    dtt2scsv(dtta=mse.by.prmod.wd) 
    # [1] "date,0_ba_by.gr,0_ba_oa,0_ol_by.gr,0_ol_oa,1_ba_by.gr,1_ba_oa,1_ol_by.gr,1_ol_oa\\\\n' +'2015-01-01,0.002021,0.002021,0.002021,0.002021,0.001898,0.001897,0.001897,0.001897\\\\n' +'2015-01-02,0.002205,0.002205,0.002205,0.002205,0.001979,0.001979,0.001979,0.001979\\\\n' +'2015-01-03,0.002170,0.002170,0.002170,0.002170,0.002006,0.002006,0.002006,0.002005\\\\n' +'2015-01-04,0.002060,0.002060,0.002060,0.002060,0.001781,0.001781,0.001781,0.001781\\\\n' +'2015-01-05,0.002032,0.002032,0.002032,0.002032,0.001838,0.001838,0.001838,0.001838\\\\n' +'2015-01-06,0.001880,0.001880,0.001880,0.001880,0.001821,0.001821,0.001821,0.001821\\\\n' +'2015-01-07,0.001766,0.001766,0.001766,0.001766,0.001707,0.001707,0.001707,0.001707\\\\n' +'2015-01-08,0.001741,0.001741,0.001741,0.001741,0.001660,0.001660,0.001660,0.001660\\\\n' +'2015-01-09,0.001590,0.001590,0.001590,0.001590,0.001456,0.001456,0.001456,0.001456\\\\n' +'2015-01-10,0.001337,0.001337,0.001337,0.001337,0.001102,0.001102,0.001102,0.001102\\\\n' +'2015-01-11,0.001315,0.001315,0.001315,0.001315,0.000267,0.000267,0.000267,0.000267\\\\n' +'2015-01-12,0.001289,0.001288,0.001288,0.001288,0.001071,0.001071,0.001071,0.001071\\\\n' +'2015-01-13,0.001318,0.001317,0.001317,0.001317,0.001240,0.001240,0.001240,0.001240\\\\n' +'2015-01-14,0.001610,0.001609,0.001610,0.001609,0.001532,0.001532,0.001532,0.001532\\\\n' +'2015-01-15,0.001551,0.001551,0.001551,0.001551,0.001437,0.001436,0.001437,0.001437\\\\n' +'2015-01-16,0.001524,0.001524,0.001524,0.001523,0.001362,0.001362,0.001362,0.001362\\\\n' +'2015-01-17,0.001427,0.001427,0.001427,0.001427,0.001335,0.001335,0.001335,0.001335\\\\n' +'2015-01-18,0.001410,0.001410,0.001410,0.001410,0.001325,0.001324,0.001325,0.001324\\\\n' +'2015-01-19,0.001387,0.001387,0.001387,0.001387,0.001313,0.001312,0.001313,0.001312\\\\n' +'2015-01-20,0.001360,0.001360,0.001360,0.001360,0.001269,0.001269,0.001269,0.001268\\\\n' +'2015-01-21,0.001477,0.001477,0.001477,0.001476,0.001377,0.001377,0.001377,0.001377\\\\n' +'2015-01-22,0.001428,0.001428,0.001428,0.001428,0.001390,0.001389,0.001389,0.001389\\\\n' +'2015-01-23,0.001368,0.001368,0.001368,0.001368,0.001321,0.001321,0.001321,0.001321\\\\n' +'2015-01-24,0.001369,0.001369,0.001369,0.001369,0.001300,0.001300,0.001300,0.001300\\\\n' +'2015-01-25,0.001333,0.001333,0.001333,0.001333,0.001292,0.001292,0.001292,0.001292\\\\n' +'2015-01-26,0.001451,0.001451,0.001451,0.001451,0.001343,0.001343,0.001343,0.001343\\\\n' +'2015-01-27,0.001656,0.001655,0.001656,0.001655,0.001552,0.001552,0.001552,0.001552\\\\n' +'2015-01-28,0.001600,0.001600,0.001600,0.001600,0.001491,0.001491,0.001491,0.001491\\\\n' +'2015-01-29,0.001573,0.001573,0.001573,0.001573,0.001523,0.001523,0.001523,0.001523\\\\n' +'2015-01-30,0.001533,0.001533,0.001533,0.001533,0.001405,0.001405,0.001405,0.001404\\\\n' +'2015-01-31,0.001475,0.001475,0.001475,0.001475,0.001408,0.001408,0.001408,0.001408\\\\n' +'2015-02-01,0.001021,0.001021,0.001021,0.001021,0.000970,0.000970,0.000970,0.000970\\\\n' +'2015-02-02,0.001441,0.001441,0.001441,0.001441,0.001307,0.001307,0.001307,0.001307\\\\n' +'2015-02-03,0.001435,0.001435,0.001435,0.001434,0.001403,0.001403,0.001403,0.001403\\\\n' +'2015-02-04,0.001415,0.001415,0.001415,0.001415,0.001338,0.001338,0.001338,0.001338\\\\n' +'2015-02-05,0.001348,0.001348,0.001348,0.001348,0.001233,0.001232,0.001233,0.001232\\\\n' +'2015-02-06,0.001315,0.001315,0.001315,0.001315,0.001260,0.001260,0.001260,0.001259\\\\n' +'2015-02-07,0.001349,0.001349,0.001349,0.001349,0.001289,0.001289,0.001289,0.001289\\\\n' +'2015-02-08,0.001365,0.001365,0.001365,0.001364,0.001289,0.001289,0.001289,0.001288\\\\n' +'2015-02-09,0.001321,0.001321,0.001321,0.001321,0.001251,0.001251,0.001251,0.001251\\\\n' +'2015-02-10,0.001723,0.001723,0.001723,0.001723,0.001608,0.001608,0.001608,0.001608\\\\n' +'2015-02-11,0.002258,0.002258,0.002258,0.002258,0.001971,0.001971,0.001971,0.001971\\\\n' +'2015-02-12,0.002162,0.002162,0.002162,0.002162,0.001968,0.001967,0.001968,0.001967\\\\n' +'2015-02-13,0.001784,0.001784,0.001784,0.001784,0.001738,0.001738,0.001738,0.001738\\\\n' +'2015-02-14,0.001862,0.001861,0.001862,0.001862,0.001638,0.001637,0.001637,0.001637\\\\n' +'2015-02-15,0.001871,0.001870,0.001870,0.001870,0.001729,0.001728,0.001728,0.001728\\\\n' +'2015-02-16,0.001558,0.001557,0.001557,0.001557,0.001505,0.001505,0.001505,0.001505\\\\n' +'2015-02-17,0.001389,0.001389,0.001389,0.001388,0.001331,0.001331,0.001331,0.001331\\\\n' +'2015-02-18,0.001366,0.001366,0.001366,0.001366,0.001295,0.001295,0.001295,0.001295\\\\n' +'2015-02-19,0.001304,0.001304,0.001304,0.001303,0.001284,0.001284,0.001284,0.001284\\\\n' +'2015-02-20,0.001365,0.001365,0.001365,0.001365,0.001290,0.001290,0.001290,0.001289\\\\n' +'2015-02-21,0.001524,0.001524,0.001524,0.001524,0.001374,0.001374,0.001374,0.001374\\\\n' +'2015-02-22,0.001533,0.001533,0.001533,0.001533,0.001380,0.001380,0.001380,0.001380\\\\n' +'2015-02-23,0.001459,0.001459,0.001459,0.001459,0.001381,0.001381,0.001381,0.001380\\\\n' +'2015-02-24,0.001281,0.001281,0.001281,0.001281,0.001247,0.001247,0.001247,0.001247\\\\n' +'2015-02-25,0.001456,0.001456,0.001456,0.001456,0.001326,0.001326,0.001326,0.001326\\\\n' +'2015-02-26,0.001754,0.001753,0.001753,0.001753,0.001540,0.001540,0.001540,0.001540\\\\n' +'2015-02-27,0.001801,0.001801,0.001801,0.001801,0.001604,0.001604,0.001604,0.001604\\\\n' +'2015-02-28,0.001729,0.001728,0.001728,0.001728,0.001610,0.001610,0.001610,0.001610"
    mse.by.prmod.wd= fread('m:/62_TMoRod/out/mse.by.prmod.wd.csv')
    sm2= c(na(mse.by.prmod.wd) %>% pasc, apply(mse.by.prmod.wd, 1, pasc)) %>% pasc(coll="\\\\n' +'")
    sa2= c(na(auc.by.prmod.wd) %>% pasc, apply(auc.by.prmod.wd, 1, pasc)) %>% pasc(coll="\\\\n")
    
    identical(sm2, sm)
}


