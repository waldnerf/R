mcNemar <- function(c1,c2,ref){
  dummy1<-as.data.frame(c1,optional=TRUE)
  dummy2<-as.data.frame(c2,optional=TRUE)
  ref   <-as.data.frame(ref,optional=TRUE)
  correct.c1<-(dummy1[,] == ref[,1])
  correct.c2<-(dummy2[,] == ref[,1])
  
  #We will construct the contingency matrix by running the following commands:
  n00<-sum(!correct.c1 & !correct.c2)
  n10<-sum(!correct.c1 & correct.c2)
  n01<-sum(correct.c1 & !correct.c2)
  n11<-sum(correct.c1 & correct.c2)
  mc <- mcnemar.test(matrix(c(n00,n10,n01,n11),nrow=2))
  return(list(mc=mc,cm=matrix(c(n00,n10,n01,n11),nrow=2)))
}

