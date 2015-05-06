#' show mean ratings
#' 
#' offensive, defensive and home field
#' 
#' @param z output from sampling (list: `z` of class `stanfit`, `names` vector of team names)
#' @param ordering how to order output: `rat` (default) orders by rating, `alph` orders by team name
#' 
#' @return list containing `r`: table of ratings, `h` home field advantage
#' @export

show_rating=function(z,ordering="rat") {
  pars=extract(z$z)
  nt=ncol(pars$o)
  s=numeric(nt)
  o=numeric(nt)
  d=numeric(nt)
  for (i in 1:nt)
  {
    o[i]=mean(pars$o[,i])
    d[i]=mean(pars$d[,i])
    s[i]=mean(o[i]+d[i])  
  }
  if (ordering=="rat") {
    ord=order(-s)    
  } else {
    ord=order(z$names)
  }
  dd=data.frame(who=z$names,off=o,def=d,sum=s,diff=o-d)
  dds=dd[ord,]
  list(r=dds,h=mean(pars$h))
}

#' Show all the matches in a competition involving input team
#' 
#' @param comp matches data frame
#' @param name of team to search for
#' 
#' @return data frame containing just matches for that team
#' 
#' @export
show_matches=function(comp,name) {
  comp %>% filter(t1==name | t2==name)
}
