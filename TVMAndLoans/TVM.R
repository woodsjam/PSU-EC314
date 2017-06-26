# Class definitions

TVM<-setClass("TVM", 
              slots=c(N="numeric",Shift="numeric"),
              prototype=list(N=0,Shift=0))


Constant<-setClass("Constant", 
                   slots=c(A="numeric"),
                   prototype=list(A=0),
                   contains="TVM")

Linear<-setClass("Linear",
                 slots=c(G="numeric"),
                 prototype=list(G=0),
                 contains="TVM")

Geometric<-setClass("Geometric",
                    slots=c(A1="numeric",g="numeric"),
                    prototype=list(A1=0,g=0),
                    contains="TVM")

Singleton<-setClass("Singleton",
                   slots=c(A="numeric"),
                   prototype=list(A=0),
                   contains="TVM")


# Constructors

Constant<-function(A=0,N=0,Shift=0){new("Constant",N=N,Shift=Shift,A=A )}
Linear<-function(G=0,N=0,Shift=0){new("Linear",N=N,Shift=Shift,G=G )}
Geometric<-function(A1=0,g=0,N=0,Shift=0){new("Geometric",N=N,Shift=Shift,A1=A1,g=g )}
Singleton<-function(A=0,N=0){new("Singleton",N=N,Shift=0,A=A )}


# For printing out the factor notation representation

setGeneric(name='FactorNotation',
           def=function(theObject)
           {
             standardGeneric("FactorNotation")
           }
)

setMethod(f="FactorNotation",
          signature="Constant",
          definition=function(theObject)
          {
            paste("\\\\frac{",theObject@A,"(P|A, i ,", theObject@N,")}{(1+i)^{",theObject@Shift,"}}",sep="")
            
          }
)

setMethod(f="FactorNotation",
          signature="Linear",
          definition=function(theObject)
          {
            paste("\\\\frac{",theObject@G,"(P|G, i ,", theObject@N,")}{(1+i)^{",theObject@Shift,"}}",sep="")
            
          }
)



setMethod(f="FactorNotation",
          signature="Geometric",
          definition=function(theObject)
          {
            paste("\\\\frac{",theObject@A1,"(P|A_1, i ,","g=",theObject@g*100,"\\\\%,", theObject@N,")}{(1+i)^{",theObject@Shift,"}}",sep="")
            
          }
)

setMethod(f="FactorNotation",
          signature="Singleton",
          definition=function(theObject)
            {
              paste("\\\\frac{",theObject@A,"}{(1+i)^{",theObject@N,"}}",sep="")
            }
)

setMethod(f="length",
          signature='TVM',
          definition=function(theObject)
          {
            theObject@Shift+theObject@N+1
          }
)


#A function to give the cash flow as a c().  Note that there is a way to make it be a specific length.  No checking parameters and no defaults.  Length includes the zero.  It is not the last period.

setGeneric(name="CashFlow",
           def=function(theObject,len)
           {
             standardGeneric("CashFlow")
           }
)


setMethod(f="CashFlow",
          signature="Constant",
          definition=function(theObject,len=0)
          {
            base<-c(0,rep(theObject@A,theObject@N))
            if(theObject@Shift==-1) base<-base[-1] else base<-c(rep(0,theObject@Shift),base)
            length(base)<-len
            base[is.na(base)]<-0
            base
          }
)

setMethod(f="CashFlow",
          signature="Linear",
          definition=function(theObject,len=0)
          {
            base<-c(0,seq(from=0,by=theObject@G,length.out=theObject@N))
            if(theObject@Shift==-1) base<-base[-1] else base<-c(rep(0,theObject@Shift),base)
            length(base)<-len
            base[is.na(base)]<-0
            base
          }
)


setMethod(f="CashFlow",
          signature="Geometric",
          definition=function(theObject,len=0)
          {
            base<-c(0,rep(theObject@A1,theObject@N)*(1+theObject@g)^seq(0,theObject@N-1))
            if(theObject@Shift==-1) base<-base[-1] else base<-c(rep(0,theObject@Shift),base)
            length(base)<-len
            base[is.na(base)]<-0
            base
          }
)


setMethod(f="CashFlow",
          signature="Singleton",
          definition=function(theObject,len=0)
            {
            base<-rep(0,theObject@N+1)
            base[theObject@N+1]<-theObject@A
            length(base)<-len
            base[is.na(base)]<-0
            base
            }
)

CombinedCashFlow<-function(ListofTVM){
  longest<-max(unlist(lapply(ListofTVM,length)))
  rowSums(simplify2array(lapply(ListofTVM,FUN=function(obj)CashFlow(obj,longest))))    
}


##
Singleton(10,5)
CombinedCashFlow(list(Singleton(A=10,N=5),Singleton(A=10,N=6)))

