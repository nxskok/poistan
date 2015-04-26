#' show mean ratings
#' 
#' offensive, defensive and home field
#' 
#' @param z output from sampling (list: `z` of class `stanfit`, `names` vector of team names)
#' 
#' @return list containing `r`: table of ratings, `h` home field advantage

show_rating=function(z) {
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
  s
  ord=order(-s)
  ord
  dd=data.frame(who=z$names,off=o,def=d,sum=s,diff=o-d)
  list(r=dd[ord,],h=mean(pars$h))
}

