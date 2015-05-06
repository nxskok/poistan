#' Create input to sampler from data frame
#' 
#' @param res Data frame of results with columns team1, team2, score1, score2
#' 
#' @return list containing design matrix of teams (as numbers), matrix of scores, names of teams
#' 
#' @export
#' @examples makexy(m)

makexy=function(res)
{
  f=factor(c(res[,1],res[,2]))
  ll=split_half(f)
  x=cbind(as.numeric(ll[[1]]),as.numeric(ll[[2]]))
  y=cbind(res[,3],res[,4]) 
  list(X=x,Y=y,names=levels(f))
}

#' Split vector into two halves
#' 
#' @param x vector to split in half
#' 
#' @return list of length 2, each component containing half of original vector
#' 
#' @examples split_half(letters[1:6])

split_half=function(x)
{
  sf=ceiling(2*seq_along(x)/length(x))
  split(x,sf)
}
