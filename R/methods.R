# # TODO
# # # pima is a class which inherit fom list. each element of the list is a list with 2 elements: Tspace (mandatory) and summary_table (optional). further elements are allowed.
# https://www.datamentor.io/r-programming/s3-class
# #
# # test1 <- setClass("test1", slots = c(Tspace="matrix", summary_table="data.frame"))
# # ## an object from the class
# # t1 <- test1(Tspace = matrix(1:10))
# # 
# # ## A class extending the previous, adding one more slot
# # pima <- setClass("pima",
# #                        slots = c("list"))
# # 
# # pima(mods)
# # https://stackoverflow.com/questions/41520472/r-define-class-as-list-of-classes
# setClass("onetest", slots = c(Tspace="matrix", summary_table="data.frame"),
#          prototype = prototype(Tspace= matrix(NA), summary_table=data.frame()))
# # setClass('onetest',
# # #         representation = representation(obj = "character", amount = "numeric"),
# #          prototype = prototype(obj = 'hi', amount = 0))
# 
# getClass('onetest')
# test1=new("onetest")
# 
# # init multiset#
# # setMethod(f = 'initialize', 
# #           signature = 'multiset', 
# #           definition =  function(.Object, ..., amount = numeric(0)){
# #             print('you just initialized the class - multiset')
# #             callNextMethod(.Object, ..., amount = amount)
# #           })
# # new('multiset', amount = 2)
# # # [1] "you just initialized the class - multiset"
# # # An object of class "multiset"
# # # Slot "obj":
# # #   [1] "hi"
# # # 
# # # Slot "amount":
# # #   [1] 2
# # 
# 
# setMethod('pima', 
#          representation = representation(objects = 'list'),
#           prototype = prototype(objects = list(test1)))
# 
# jt=new('pima', objects = list(test1,test1))
# str(jt)
# jt[1]

#' @title Methods for pima objects
#'
#' @description Methods for \code{pima} objects. 
#' The following are methods to extract and manipulate relevant information from
#' a \code{pima} object.
#' 
#' @name pima-method
#' @docType methods

NULL
# 
#' non funziona e non funziona neppure summary se lo uso: 
#' #' print.pima print method for a pima object.
#' #' @rdname pima-method
#' #' @param object an object of class \code{pima}.
#' #' @param ... additional arguments to be passed
#' #' @method print pima
#' #' @docType methods
#' #' @export
#' print.pima <- function (object, ...) {
#'   #summary.pima(object, ...)
#' }

# 

#' summary.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param ... additional arguments to be passed
#' @method  summary pima
#' @docType methods
#' @export

summary.pima <- function (object, ...) 
{
  object$summary_table
 # do.call(rbind,lapply(object, function(ob) ob$summary_table))
}

.get_summary_table_from_flipscores <- function(object){
  tab = as.data.frame(summary(object)$coefficients)
  tab = tab[!is.na(tab[, "Score"]), ]
  colnames(tab)[ncol(tab)]="p"
  tab = cbind( Coeff = rownames(tab), tab)
}
 

is_signif=NULL
###########################
#' as.pima method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param names_obj a vector of names, its length must be equal to the length of \code{object}
#' @param ... additional arguments to be passed
#' @method as pima
#' @docType methods
#' @export

as.pima <- function (object, names_obj=NULL, ...)
{
  # TODO: calcolare summary_table in ogni elemento di object. se flipscores usa
  #     .get_summary_table_from_flipscores()
  if(!is.null(names_obj)) names(object)=names_obj
  if (is.null(names(object))) names(object)=paste0("mod",1:length(object))
  class(object) <- unique(c("pima", class(object)))
  object
}
#############################################
#' plot.pima summary method for a pima object.
#' @rdname pima-method
#' @param object an object of class \code{pima}.
#' @param p.values  use "raw" or "adjusted" (i.e. corrected for selective inference) 
#' @param alpha a value between 0 and 1. The plot will mark the p-values smaller than \code{alpha} (0.05 by default). If equal to 0 or 1 nothing will be marked. 
#' @param ... additional arguments to be passed
#' @method  plot pima
#' @docType methods
#' @export
#' @import ggplot2

plot.pima <- function(object,
                    p.values=c("raw","adjusted"),
                    alpha=.05){
  
  p.values=p.values[1]
  y="-log10(p.vals)"
  x="Estimate"
  group="Coeff"
  D=summary.pima(object)
  if(p.values =="raw"){
    if(!is.null(D$`Pr(>|z|)`)) D$p.vals=D$`Pr(>|z|)` else
      D$p.vals=D$`Pr(>|t|)`
    if(is.null(D$p.vals)) D$p.vals=D$p
  } else
    if(p.values=="adjusted")
      D$p.vals=D$p.adj
  D$is_signif=(D$p.vals<=alpha)
  
  if(p.values=="raw") title="(Raw) p-values" else title="Adjusted p-values"
  
  p <- ggplot(D, aes_string(y=y, 
                            x=x, 
                            group=group,color=group)) +
    geom_point(aes(shape=is_signif),size=2)+ 
    ggtitle(title) + theme_minimal() 
  if(!(alpha%in%c(0,1))){
    p <- p +
      scale_shape_manual(values=c(3,19),name = paste(p.values,
                                                     "p-value"), 
                         labels = paste0(c("p >  ", "p <= "),
                                         alpha))
  }
  #RIRR and RP indices 
  IRR<-exp(object$summary_table$Estimate)
  RIRR<-as.numeric(quantile(IRR,c(0.99))/quantile(IRR,c(0.01)))
  #Relative pvalues
  RP_raw<-as.numeric(diff(-log10(quantile(object$summary_table$p,c(0.99,0.01)))))
  RP_adjusted<-as.numeric(diff(-log10(quantile(object$summary_table$p.adj,c(0.99,0.01)))))
  if(p.values=="raw") {
    indices<-paste("RIRR=",round(RIRR,3),"\nRP=",round(RP_raw,3),sep="")
    p + geom_text(x=quantile(D$Estimate,0.05), y=quantile(-log10(D$p),0.95), label=indices)
  }
    if(p.values=="adjusted"){
      indices<-paste("RIRR=",round(RIRR,3),"\nRP=",round(RP_adjusted,3),sep="")
      p + geom_text(x=quantile(D$Estimate,0.05), y=quantile(-log10(D$p),0.95), label=indices)
    }
  p
}
