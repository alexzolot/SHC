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
	#rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe')
	#options(error= NULL)  # options(error= recover) options(error=dump) 

	#options(error= NULL) # options(error= recover) options(error=dump)
	options(datatable.print.nrows=200)
    
	onWin= Sys.getenv('R_PLATFORM')==''
#	root= '.'
    if(onWin) setwd('M:/56_block_highchartsTS/clean')


#	source('lib/HLP.r')  # or hard coded paths to source files
    
    # from HLP.r
    
    #====  aliases and 1-liners  ====
    ch= as.character
    fa= factor
    nu= as.numeric
    df= function(..., saf = FALSE) data.frame(..., stringsAsFactors = saf) #df= data.frame
    
    
    #w cat + sprintf
    catf= function(...) cat(sprintf(...))
    catt= function(...) {cat(...); cat('\n')} 
    #'  catt with names
    #e	z= 1:5; v= letters[1:2];  catn(z, v, 7, u<-'a', v=88)
    catn= function(..., file=stdout()) { catt('\n__')  # catn:')
	        nargs= unlist(strsplit(ch(match.call()),'(),', fixed =T))[-1]  # names of args
	        #for (i in 1:le(nargs)) catf('%15s= |%s|\n', nargs[[i]], coernu1(list(...)[[i]],'NULL'), file=file)
	        for (i in 1:le(list(...))) catf('%15s= |%s|\n', nargs[[i]], coernu1(list(...)[[i]],'NULL'), file=file)
    }
    
    #w grep
    gre2= function(patt='', pattNeg='^$', x, v=T, ...){ a=grepl(patt, x,...) & !grepl(pattNeg, x,...); return( if(v) x[a] else a) }
    
    
    #' fun to split column names
    #e  cn('aa bb 2013')
    cn= function(cnn,  sep='[ ,\n\t\\+]+') unlist(strsplit(cnn, sep))  # b='',`%a%` = '%c%' =  col.names= cn(cnn)  'aa b 1'  %a% 1  aa b 1'  cn('aa b 1')
    #w for paste
    pas= function(x, sep=' ',collapse=' ')paste(x,sep=sep,collapse=collapse)
    pasc= function(..., coll= ',') pas(..., collapse= coll)
    '%+%' = paste0
    
    
    #w str
    #'e u= strr(cars); prr(u)
    strr= function(x) {catf('\nstr(%s):\n', desu(x)); str(x); capture.output(str(x))}
    
    
    le= length
    na= names
    sf= sprintf
    
    #' print list in 1 column
    #e prr(letters[1:5])
    #e prr(cars); prr(dir()); prr(character(0));
    prr= function(x, ma='', header=T) { if(header)catf('\n&&& %s == %s ==\n', ma, deparse(substitute(x))); ns= na(x)
		if(le(x)==0){message('length=0'); return(0)}
     	for(i in if(is.null(ns)) 1:le(x) else ns) catf('%3s= %s\n', i,  x[[i]]); catt('-------------------------\n')
		invisible(x)
 	} 
    
#' install + library
#p gh github username 
    libra= function(libs, verb=TRUE, gh='',...){
#	libs= if(!grepl('\\"|\'| ', li<- deparse(substitute(libs)))) li else cn(libs)  # now libs is a char vector
#	misspk= nin(gsub('.*\\/','', libs), installed.packages()[,1])
	        
            libs= if(!grepl('\\"|\'| ', li<- gsub('"','', deparse(substitute(libs))))) li else cn(li)  # now libs is a char vector
            #if(verb)catn(li, libs)
            
	        misspk= setdiff(gsub('.*\\/','', li), installed.packages()[,1])
	        
            #	if(le(misspk) > 0) {
#		if(gh==''){install.packages(misspk, repos= "http://cran.stat.ucla.edu/", dependencies= T)
#		} else {if(le(misspk) > 1) message('we can load only one github package!')
#			    devtools::install_github(misspk[1], gh, dependencies= T,...)
#		}
#	}
	        
	        for(li in libs){
            li2= gsub('.*\\/','', li)
            #if(verb)catn(li, li2)
            try({ 
    	                if(li2 %in% misspk){
                            if(grepl('\\/', li)) devtools::install_github(li, dependencies= T,...)
                            else install.packages(li, dependencies= T, repos= "http://cran.stat.ucla.edu/",...)
                        }
                        catt('libra:: Call library ', li) 
    		            do.call(library, list(li2, character.only =T))
    		            if(verb) catf('libra:: demo(%s); example(%1$s); vignette("%1$s")\n', li2)
                    })
	        }
    } #--
    
    
	
	libra(plyr) 
	libra(data.table);  dtt= data.table
    
    libra(magrittr)
    
    
    options(shiny.error=browser )  # NULL
    libra(shiny)
    libra(markdown)
    #source('M:/62_TMoRod/shiny/hiCharts/funcs.R')
    #source('lib/funcs.R')	
#	proot= '.'  # project root
#	setwd(file.path(proot, '.'))
	

} #--
##########################################################



#m= monitor_stats3a= readRDS('m:/62_TMoRod/shiny/monitor_stats3a.rds')[,imps:=imp]

#' alias for deparse(substitute())
desu= function(x, n=1) deparse(substitute(x, env= parent.frame(n) ))

coernu1= function(x, a='', f=I)  if(is.null(x)) a else f(x)


hmd= function(...) HTML(markdownToHTML(..., fragment.only =T))

gna= function(patt='', pattNeg='^0z', x, pref='', suff=''){u=colnames(x); u= u[grepl(patt, u) & !grepl(pattNeg, u)]; names(u)= pref %+% u %+% suff; u}  # ex: gna('Se', 'Wi', iris, 'p.', '.s')


