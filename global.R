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


	source('lib/HLP.r')  # or hard coded paths to source files
	
	libra(plyr) 
	libra(data.table)
    libra(magrittr)
    
    
    options(shiny.error= NULL)  # browser
    libra(shiny)
    libra(markdown)
    #source('M:/62_TMoRod/shiny/hiCharts/funcs.R')
    #source('lib/funcs.R')	
	proot= '.'  # project root
	sw(fp(proot, '.'))
	

} #--
##########################################################



#m= monitor_stats3a= readRDS('m:/62_TMoRod/shiny/monitor_stats3a.rds')[,imps:=imp]

#' alias for deparse(substitute())
desu= function(x, n=1) deparse(substitute(x, env= parent.frame(n) ))

coernu1= function(x, a='', f=I)  if(is.null(x)) a else f(x)


hmd= function(...) HTML(markdownToHTML(..., fragment.only =T))

gna= function(patt='', pattNeg='^0z', x, pref='', suff=''){u=colnames(x); u= u[grepl(patt, u) & !grepl(pattNeg, u)]; names(u)= pref %+% u %+% suff; u}  # ex: gna('Se', 'Wi', iris, 'p.', '.s')


