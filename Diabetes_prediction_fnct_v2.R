
######### FUNCTION VARIABLES ###############
### year - Transition start year
### sourceid - Client sourceid (1,2,3,4)
### n - number of periods(integer) to predict
############################################

predict.Diabetes <- function(year,sourceid,n) {

### Load packages ####
  require ("expm")
  require ("RODBC") 
#################### SQL ODBC Connection ##############################
  
  exec.store.proc <- function(start_year, sourceid){
    # establish a connection
    ch<-odbcConnect('benchmark_database') ;
    # construct a query string with a parameter
    query <- paste("exec wrk.sp_DiabetesTransitionsProbability @start_year = ", start_year, ", @sourceid = ", sourceid, sep="");
    # execute the query
    df<-sqlQuery(ch, query);
    # print the results
    print(df);
  }
  
  exec.store.proc1 <- function(start_year, sourceid){
    # establish a connection
    ch<-odbcConnect('benchmark_database') ;
    # construct a query string with a parameter
    query <- paste("exec wrk.sp_DiabetesTransitions @start_year = ", start_year, ", @sourceid = ", sourceid, sep="");
    # execute the query
    df<-sqlQuery(ch, query);
    # print the results
    print(df);
  }
  
  
  ############### Get transition matrix and initial state/Population data #############
    
  initialState<-as.matrix(exec.store.proc1(year, sourceid)[,3])
  initialState
  
  population <- as.matrix(exec.store.proc1(year, sourceid)[,2])
  population
  
  t<-as.matrix(exec.store.proc(year, sourceid)[,2:6])
  t
  
  ####################################
  ####################################### Get predicted probability ########
  
  predicted<-t %^% n
  predicted
  
  
  predicted_n<- t(initialState) %*% predicted  
  predicted_n
  
  ###################################
  ###################################### Predicted population #############
  
  predicted_population <- predicted_n * sum(population)
  t(round(predicted_population))

}

predict.Diabetes(2013,3,2)





