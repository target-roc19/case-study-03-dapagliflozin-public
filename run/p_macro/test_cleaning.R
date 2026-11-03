diff3 <- function(in1,in2){ ## wrap up some set calculations:
  list(both=intersect(in1,in2),only1=setdiff(in1,in2),only2=setdiff(in2,in1)) }

.getFromFile = function(fn,pos){
    ## attach a file and quietly return the environment
    e=attach(fn,warn.conflicts=FALSE,pos=pos)
    return(e)
}

ediff <- function(e1=".RData",e2=.GlobalEnv){
  ### ### ediff - wrap the real logic in a try: ###
  s0 = search()[-1]
  ret = try(.ediff(e1,e2),TRUE)
  if(inherits(ret,"try-error")){
    s1 = search()[-1]
    ngo = length(s1) - length(s0)
    if(ngo > 0){
      for(i in 1:ngo){ detach(pos=2) }
    }
    stop(ret)
  }
  return(ret)
}

.ediff <- function(e1=".RData",e2=.GlobalEnv){
  ### ### diff two of any RData files or environment. ###
  pos = 1
  # for tracking how many we need to detach at end
  if(inherits(e1,"character")){
    n1 = e1
    pos = pos + 1
    e1 = .getFromFile(e1,pos)
  }
  else{ n1 = capture.output(print(e1)) }
  in1 = ls(e1, all.names=TRUE)
  if(inherits(e2,"character")){
    n2 = e2
    pos = pos + 1
    e2 = .getFromFile(e2,pos)
  }
  else{ n2 = capture.output(print(e2)) }
  in2 = ls(e2, all.names=TRUE)
  d3 = diff3(in1,in2)
  d3$identical=character(0)
  d3$different=character(0)
  for(n in d3$both){
    o1 = get(n,e1)
    o2 = get(n,e2)
    if(all.equal(o1,o2)){ d3$identical=c(d3$identical,n) }else{ d3$different=c(d3$different,n) } }
  if(pos == 2){ detach(pos=2) }
  if(pos == 3){
    detach(pos=2)
    detach(pos=2)
  }
  d3$name1 = n1
  d3$name2 = n2
  class(d3)="ediff"
  return(d3)
}

print.ediff <- function(x,...){
  templ = " --
  Difference between %s and %s \
 --
  
  Only in %s:
  %s
  
  Only in %s:
  %s
  
  Different in %s and %s:
  %s "
  
  # Identical in %s and %s:
  #   %s
  # x$name1,x$name2,identical,
  
  identical = paste(x$identical,collapse=",")
  different = paste(x$different,collapse=",")
  only1 = paste(x$only1,collapse=",")
  only2 = paste(x$only2,collapse=",")
  s=sprintf(templ,x$name1,x$name2,x$name1,only1,x$name2,only2,x$name1,x$name2,different)
  cat(s)
} 
