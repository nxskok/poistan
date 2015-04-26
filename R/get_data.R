#' get competition from data base
#' 
#' @param id competition id
#' 
#' @return data frame of results

get_comp=function(id) {
  m=dbDriver("SQLite")
  con=dbConnect(m,"/home/ken/sports/scoresway/soccer.db")
  q1="select s.date, t1.name, t2.name, s.score from scores as s, teams as t1, teams as t2  where s.comp="
  q2=" and s.result>=0  and s.t1=t1.id and s.t2=t2.id;"
  query=paste0(q1,id,q2)
  rs=dbSendQuery(con,query)
  z=fetch(rs,n=-1)
  dbClearResult(rs)
#  dbDisconnect(m)
  # untangle scores
  ss=strsplit(z$score," - ")
  s1=sapply(ss,function(x) x[1])
  s2=sapply(ss,function(x) x[2])
  data.frame(t1=z[,2],t2=z[,3],s1=as.numeric(s1),
             s2=as.numeric(s2),stringsAsFactors=F)
}


