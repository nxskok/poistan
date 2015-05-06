#' Obtain predictive distribution for teams numbers i and j from fitted model z
#' @param z fitted model from \code{sample_model}
#' @param i number (not name) of home team
#' @param j number (not name) of away team
#' @return list of: \code{dist}, predictive probabilities of each score,
#'   \code{prob} posterior predictive probabilities of win, draw and loss for
#'   team \code{i} at home against team \code{j}.
#' @export

do_predict=function(z,i,j)
{
  pars=extract(z$z)
  l1=exp(pars$h+pars$o[,i]-pars$d[,j])
  l2=exp(pars$o[,j]-pars$d[,i])
  nr=nrow(pars$o)
  r1=rpois(nr,l1)
  r2=rpois(nr,l2)
  tb=table(paste(r1,r2,sep="-"))
  o=order(-tb)
  d=r1-r2
  dt=table(cut(d,c(-100,-0.5,0.5,100)))
  dt=dt/sum(dt)
  names(dt)=c("L","D","W")
  #  list(head(tb[o]/nr,n=12),dt[3:1])
  list(dist=tb[o]/nr,prob=dt[3:1])
}
