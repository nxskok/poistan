#' Create input to sampler from data frame
#' 
#' @param res Data frame of results with columns team1, team2, score1, score2

makexy=function(res)
{
  f=factor(c(res[,1],res[,2]))
  ll=split_half(f)
  x=cbind(as.numeric(ll[[1]]),as.numeric(ll[[2]]))
  y=cbind(res[,3],res[,4]) 
  list(X=x,Y=y,names=levels(f))
}

split_half=function(x)
{
  sf=ceiling(2*seq_along(x)/length(x))
  split(x,sf)
}
