#' Sample from Poisson model
#' 
#' Obtain Stan sampling object
#' 
#' @param p.sc compiled Poisson object
#' @param xylist data processed (fix)
#' @param nit number of iterations of sampler
#' 
#' @return list: Stan sampling object, names of teams

sample_model=function(p.sc,xylist,nit=10000)
{
  X=xylist$X
  Y=xylist$Y
  ng=nrow(X)
  nt=length(table(X))
  list(z=sampling(p.sc,list(x=X,y=Y,ng=ng,nt=nt),iter=nit),
       names=xylist$names)
}

#' Posterior mode estimate of Poisson model
#' 
#' Obtain posterior mode estimates of ratings
#' 
#' @param p.sc compiled Poisson object
#' @param xylist data processed (fix)
#' 
#' @return Stan optimizing object


optim.model=function(p.sc,xylist)
{
  X=xylist$X
  Y=xylist$Y
  ng=nrow(X)
  nt=max(X)
  optimizing(p.sc,list(x=X,y=Y,ng=ng,nt=nt),iter=10000)  
}
