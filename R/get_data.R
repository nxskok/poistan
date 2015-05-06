#' connect to database
#' @param what type of database (default SQLite)
#' @param name name of database (default soccer.db)
#' 
#' @return connection
#' 
connect=function(what="SQLite",name="/home/ken/sports/scoresway/soccer.db") {
  m=DBI::dbDriver("SQLite")
  con=RSQLite::dbConnect(m,"/home/ken/sports/scoresway/soccer.db")
  con
}

#' get competition from data base
#' 
#' @param id competition id
#' 
#' @return data frame of results

get_comp=function(id) {
  con=connect()
  q1="select s.date, t1.name, t2.name, s.score from scores as s, teams as t1, teams as t2  where s.comp="
  q2=" and s.result>=0  and s.t1=t1.id and s.t2=t2.id;"
  query=paste0(q1,id,q2)
  rs=RSQLite::dbSendQuery(con,query)
  z=DBI::fetch(rs,n=-1)
  RSQLite::dbClearResult(rs)
#  dbDisconnect(m)
  # untangle scores
  ss=strsplit(z$score," - ")
  s1=sapply(ss,function(x) x[1])
  s2=sapply(ss,function(x) x[2])
  data.frame(t1=z[,2],t2=z[,3],s1=as.numeric(s1),
             s2=as.numeric(s2),stringsAsFactors=F)
}


# sql code to get max date for each comp
# select max(date), comp from scores group by comp;

#' Date of latest (scheduled) match for each competition
#' @param none
#' @return data frame of competition IDs (in \code{comp}) and last date (in \code{date}, as \code{Date})
#' 
#' @import dplyr
last_date=function() {
  con=connect()
  query="select max(date), comp from scores group by comp;"
  rs=RSQLite::dbSendQuery(con,query)
  z=DBI::fetch(rs,n=-1)
  RSQLite::dbClearResult(rs)
  z %>% mutate(date=as.Date(`max(date)`)) %>%
    select(c(date,comp)) %>% arrange(date)
}

#' Data frame of competition names and ids
#' @param none
#' @return data frame of competition numbers (in \code{id}) and competition names (in \code{name})

comp_list=function() {
  con=connect()
  query="select * from comps"
  rs=RSQLite::dbSendQuery(con,query)
  z=DBI::fetch(rs,n=-1)
  RSQLite::dbClearResult(rs)
  z
}

#' Competitions whose last scheduled game is after input date
#' @param lim_date Date that matches must be after
#' @return Data frame: \code{date} is date of last scheduled match in league,
#'   \code{comp} is competition number, \code{name} is name of competition
comp_by_last_date=function(lim_date) {
  last=last_date()
  comps=comp_list()
  inner_join(last,comps,by=c("comp"="id")) %>% filter(date>lim_date) %>% arrange(name)
}

#' Find league by date of last match and name pattern
#' @param date Date that matches must be after
#' @param pat Pattern to match in league name
#' @return data frame: \code{date} is date of last game in league, \code{comp}
#'   is competition number, \code{name} is competition name
find_comp=function(date,pat) {
  comp_by_last_date(date) %>% filter(grepl(pat,name)) %>% arrange(date)
}