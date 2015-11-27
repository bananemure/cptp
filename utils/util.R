#################################################################################################
#                                                                                               #
#               SET of useful utilities to help exploratory analysis of OPAL data: obiba.org    #
#               author: stephane Mbatchou @ MUHC                                                #
#################################################################################################

##### -----------Load dependencies
if(!suppressWarnings(require('data.table',warn.conflicts = F,quietly = T))){
  install.packages('data.table')
  library('data.table',quietly = T,warn.conflicts = F)
}

if(!suppressWarnings(require('stringi',warn.conflicts = F,quietly = T))){
  install.packages('stringi')
  library('stringi',quietly = T,warn.conflicts = F)
}

if(!suppressWarnings(require('opal',warn.conflicts = F,quietly = T))){
  install.packages('opal', repos='http://cran.obiba.org', type='source')
  library('opal',quietly = T,warn.conflicts = F)
}


################# put everything in an evironment ##########
my.util<-new.env()

##################################################################################################################
# This script is a more general settings
# it will contain  useful helper function
##################################################################################################################
#########################################
#utility functions are defined here
#########################################

#function get vars from opal
my.util$var.assign<-function(opal,datasource,table,variables=NULL)
{
  datafile<-paste0(datasource, ".", table)
  if(length(variables)==1){
    variable<-paste0(datafile,':',variables)
    data.err <- try(opal.assign(opal,'D', variable,missings=T),silent = T)
  }else{
    data.err <- try(opal.assign(opal,'D', datafile,variables,missings=T),silent = T)
  }
  
  if(inherits(data.err,'try-error')) { 
    message(paste0('-- The required data is not assigned\n'),(data.err),'-- Check the error message and try again!')
  }else { cat(paste0('The required data from ', datafile,' was correctly assigned.'))}
  
  var.value<-opal.execute(opal,'D')
  return (var.value)
}

################################################
####      PATTERN FUNCTIONS
###############################################

####function get vars with a specific pattern
my.util$has.pattern<-function(var,pattern)
{
  T %in% stri_detect_regex(var,pattern,case_insensitive=T)
}

my.util$vars.with.pattern<-function(datatable,pattern)
{ #check all the variable(s) that have a pattern (e.g '3 | 35', will check for "|") 
  dtable<-as.data.table(datatable)
  return (names(which( dtable[,sapply(.SD,has.pattern,pattern)])))
}

#get duplicate value with this specific pattern
#is_duplicate with pattern
my.util$has.duplicate<-function(var,pattern= NULL)
{
  if(is.null(pattern)) { 
    pattern <-"(^.+)(?=\\s*\\|\\s*\\1\\b)"
  }
  has.pattern(stri_trim(var),pattern)
}


my.util$vars.with.duplicate <- function(datatable,pattern = NULL)
{
  dtable<-as.data.table(datatable)
  return (names(which( dtable[,sapply(.SD,has.duplicate,pattern)])))
}


################################################## NUMERIC ################################################################
##########################################################
#         2-      NUMERIC FUNCTIONS 
######################################################

#check if  the whole variable is missing value
my.util$is.allNA<-function(var)
{
  all(is.na(var))
}


#return the name of variables with all values = null 
my.util$vars.with.all.NA <- function(datatable)
{
  dtable<-as.data.table(datatable)
  return (names(which( dtable[,sapply(.SD,is.allNA)])))
}

#######################################
# internal functions#
#is.decimal: return TRUE if the vector value(s) is(are) decimal
my.util$is.decimal <-function(x)
{
  x<-suppressWarnings(as.numeric(x))
  result <- (x != floor(x))
  result[which(is.na(result))]<-F
  return(result)
}

# internal functions#
#is.whole: return TRUE if the vector value(s) is(are) whole
my.util$is.whole <-function(x)
{
  x<-suppressWarnings(as.numeric(x))
  result <- (x == floor(x)) & !is.boolean(x)
  result[which(is.na(result))]<-F
  return(result)
}

############################################
#very utils function to test decimal vs integer
## function to test if a value is a number. ex: '2' is number
#install.packages('schoolmath')
#ex: schoolmath::is.whole(x) vs schoolmath::is.decimal

my.util$is.Number<-function(var){#check that all valid-value (non missing) are number
  if(is.allNA(var) || has.Boolean(var)) return (FALSE)
  var<-suppressWarnings(as.numeric(na.omit(var)))
  #all(stri_detect_regex(var,pattern = '^\\d+$|^\\d\\.*(?=\\d+$)'))
  !any(is.na(var))
}


my.util$is.Decimal<-function(var){ #check that all valid-value (non missing) are Decimal
  if(!is.Number(var)){
    FALSE
  }else { 
    var<-na.omit(var)
    T %in% is.decimal(var)
  }
}

my.util$is.Integer<-function(var){ #check that all valid-value (non missing) are Integer
  if(!is.Number(var)){
    FALSE
  }else { 
    var<-na.omit(var)
    all(is.whole(var))
  }
}

my.util$has.Number<-function(var){  #check if var has at least one value of type number
  if(is.allNA(var)) return (FALSE)
  var<-na.omit(var)
  var.num <- suppressWarnings(as.numeric(var))
  any((!is.boolean(var)) & (!is.na(var.num)))
}

my.util$is.Categorical <- function(var,numlevels) { #check if var is categorical variable with max 10 categories 
  if(missing(numlevels)) numlevels <- 10
  totest <- dim(table(var))
  as.logical(totest) & (totest <= numlevels)
}


##############################boolean#################################
my.util$is.boolean <-function(x){
  check <- stri_detect_regex(stri_trim(x),pattern = '^TRUE$|^FALSE$')
  check[is.na(check)] <- F
  check
}

my.util$has.Boolean <- function(var){
  any(is.boolean(var))
}

my.util$is.Boolean <-function(var){
  all(is.boolean(na.omit(var)))
}


####################################### STRING ###################################################################
################################TEXT ########################
## CASE OF 'NA' string in opal
my.util$has.NA.string <- function(var){ #check that the variable has at least one string of 'NA'
  has.pattern(var,'^NA$')
}

#similar to is.allNA but the 'NA' are in text format in OPAL
my.util$is.allNA.string <- function(var){
  all(stri_detect_regex(var,'^NA$',case_insensitive=T))
}

#return the name of variables with 'NA' text 
my.util$vars.with.NA.string <- function(datatable)
{
  dtable<-as.data.table(datatable)
  return (names(which( dtable[,sapply(.SD,has.NA.string)])))
}


#return the name of variables with all values = 'NA' text 
my.util$vars.with.all.NA.string <- function(datatable)
{
  dtable<-as.data.table(datatable)
  return (names(which( dtable[,sapply(.SD,is.allNA.string)])))
}

#############################################
#     TEXT FUNCTIONS
#############################################
my.util$has.non.numeric<-function(var){
  var<-na.omit(var)
  has.Boolean(var) || any(is.na(suppressWarnings(as.numeric(var))))
}

###################################OPAL VARIABLE TYPE ###########
####################################
#####       TYPE PREDICTION
###################################
my.util$predict.opal.type<-function(var){
  #THIS FUNCTION ATTEMPTS TO PREDICT THE TYPE OF THE VAR...
  #USEFUL TO DETECT ERROR IN DATA
  
  if(all(is.na(var))){
    'missing'
  }else{
    num <-  mean(stri_detect_regex(var,pattern = '^\\d+$|^\\d*\\.(?=\\d+$)'),na.rm = T)*100
    if (num == 50) 'undetermined'
    else if (num>50) 'numeric'
    else {
      bool <- mean(is.boolean(var),na.rm = T)*100
      ifelse(bool > 50, 'boolean', ifelse(bool == 50 ,'undetermined','text'))
    }
  }
}

#####
#function to get valueType
####

my.util$get.opal.type<-function(var){ #compute the type of a variable from a text data type(e.g '2.5' ==> decimal )
  if(!is.Number(var)){
    type = ifelse(is.Boolean(var),'boolean','text')
  }else { 
    if(is.Decimal(var)){
      type = 'decimal'
    }else{
      type = 'integer'
    }
  }
  return (type)
}




#############################################GENERAL BASE FUNCTION ################################
my.util$which.na <- function(var) 
{
  which(is.na(var))
}

#################################CONNECTING TO OPAL ##########################################
# This function is a more general settings
# it will contain credentials to connect to opal and useful helper function
#
my.util$connect.to.opal <- function(server) 
{
    if(missing(server)) stop('server (name as character) is required ',call. = F)
    
    #connect to server
    source(paste0('~/login/.',server,'-login.R'))
    
    #test connection
    message('Testing connection ....')
    
    dstest <- try(opal.datasources(o),silent = T)
    test<- !inherits(dstest, what = "try-error") & !inherits(o, what = "try-error")
    
    if (test) {
      cat(paste0('Connection to server: ',o$name,' is OK'))
    }else {
      message(paste0('Connection to server: ',o$name,' failed.\nPlease try again!'))
    }
    
}




###############################CLOSING OPALS and/or clean work space#####################################
#close everything
my.util$run.close<-function(all=F)
{
  #detect opals 
  objs <- ls(name=.GlobalEnv)
  if(length(objs)){
    for(obj in objs){
      obj<- eval(parse(text=obj))
      if ('opal' %in% class(obj)){
        message('Closing opal(s) server connection(s)')
        obj.opal <- obj
        opal.logout(obj.opal)
        cat(paste0('< ', obj.opal$name,' > server is disconnected...'),sep='\n')
      }
    }
  }
  
  if(as.logical(all)) rm(list=objs,envir=.GlobalEnv)
  else rm(my.util,pos=search())  
  detach(my.util,pos=search())
  cat('my.util environnment is now detached from memory...')  
}



######################### -> attach util env <- ###########
attach(my.util)