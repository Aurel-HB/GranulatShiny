#' puissance_maker2 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

puissance_maker2<-function(model, fixeff, n, campagne){
  
  df<-as.data.frame(setNames(replicate(4,numeric(0), simplify = F),c("p","conf_moins","conf_plus", "campagne") ))
  for (i in 1:campagne){
    mod_extend<-simr::extend(model, along = "campagne", n=n+i)
    power_sim<-powerSim(mod_extend, test=simr::fixed(fixeff))
    pval<-ifelse(power_sim[["pval"]]>0.05,1,0)
    proba<-stats::prop.test(table(pval))
    df[i,]<-cbind(proba[["estimate"]][["p"]], proba[["conf.int"]][1], proba[["conf.int"]][2], n+i)
  }
  plot_puissance<-ggplot(data=df, mapping = aes(x = campagne, y=p))+geom_point()+geom_line()
  plot_puissance
}

