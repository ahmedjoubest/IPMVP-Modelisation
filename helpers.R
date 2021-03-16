model <- function(db,linear, predict.name, Explic.1.name, Explic.2.name=NULL, resid,Explic.3.name) {
  df = db
  if(sum(is.na(df$Periode.de.Reference)) == 0 & sum(is.na(df$Periode.de.suivi)) == 0){
    max_new = nrow(df)
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.suivi)) != 0){
    max_new = which(is.na(df$Periode.de.suivi))[1] - 1
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.Reference)) != 0){
    max_new = nrow(df)
    max_ref = which(is.na(df$Periode.de.Reference))[1] - 1
  }
  # Formatage de la table
  db$Periode.de.Reference = as.Date(db$Periode.de.Reference, tryFormats = c("%d/%m/%Y"))
  if(linear == 1){
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1)
    } else{
      modele = lm(Predict  ~ Explic.1- 1)
    }
    # coef du modele
    coef = coef(modele)
    # Extraction du coefficient R 
    R2 = summary(modele)$r.squared
    RMSE = sqrt(mean(modele$residuals^2))
    # statistique T
    if(resid){
      a0 = round(summary(modele)[[4]][1,3],2)
      a1 = round(summary(modele)[[4]][2,3],2)
    } else{
      a1 = round(summary(modele)[[4]][1,3],2)
    }
    # output table
    if(resid){
      Equation = paste(predict.name ,"= ",round(coef[1],2)," + ",round(coef[2],2)," x",Explic.1.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ", "  .A0 = ",a0,"--- .A1 =",a1)
    } else{
      Equation = paste(predict.name ,"= ",round(coef[1],2)," x",Explic.1.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ","  .A1 =",a1)
    }
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Conso.lm = modele$fitted.values 
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time , Modele = Conso.lm)
    visuals = rbind(visual1,visual2)
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Réalité",length(Time)),rep("Modèle",length(Time)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes))+ geom_line(size = 1) + geom_point()+
      ggtitle(Equation) +
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))
    
    
  } else if (linear == 2) {
    
    
    
    
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 - 1)
    }
    # coef du modele
    coef = coef(modele)
    # Extraction du coefficient R 
    R2 = summary(modele)$r.squared
    RMSE = sqrt(mean(modele$residuals^2))
    # statistique T
    if(resid){
      a0 = round(summary(modele)[[4]][1,3],2)
      a1 = round(summary(modele)[[4]][2,3],2)
      a2 = round(summary(modele)[[4]][3,3],2)
      
    } else{
      a1 = round(summary(modele)[[4]][1,3],2)
      a2 = round(summary(modele)[[4]][2,3],2)    }
    # output table
    if(resid){
      Equation = paste(predict.name ,"= ",round(coef[1],2)," + ",round(coef[2],2)," x",Explic.1.name," + ",round(coef[3],2)," x",Explic.2.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ", "  .A0 = ",a0,"--- .A1 =",a1,"--- .A2 =", a2)
    } else{
      Equation = paste(predict.name ,"= ",round(coef[1],2)," x",Explic.1.name," + ",round(coef[2],2)," x",Explic.2.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ","  .A1 =",a1,"--- .A2 =", a2)
    }
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Conso.lm = modele$fitted.values 
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time , Modele = Conso.lm)
    visuals = rbind(visual1,visual2)
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Réalité",length(Time)),rep("Modèle",length(Time)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes))+ geom_line(size = 1) + geom_point()+
      ggtitle(Equation) +
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))
      
  } else if (linear == 3){
    
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    Explic.3 = db[,Explic.3.name][1:max_ref]
    
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3- 1)
    }
    # coef du modele
    coef = coef(modele)
    # Extraction du coefficient R 
    R2 = summary(modele)$r.squared
    RMSE = sqrt(mean(modele$residuals^2))
    # statistique T
    if(resid){
      a0 = round(summary(modele)[[4]][1,3],2)
      a1 = round(summary(modele)[[4]][2,3],2)
      a2 = round(summary(modele)[[4]][3,3],2)
      a3 = round(summary(modele)[[4]][4,3],2)
      
      
    } else{
      a1 = round(summary(modele)[[4]][1,3],2)
      a2 = round(summary(modele)[[4]][2,3],2)
      a3 = round(summary(modele)[[4]][3,3],2)
    }
    # output table
    if(resid){
      Equation = paste(predict.name ,"= ",round(coef[1],2)," + ",round(coef[2],2)," x",Explic.1.name," + ",round(coef[3],2)," x",Explic.2.name," + ",round(coef[4],2)," x",Explic.3.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ", "  .A0 = ",a0,"--- .A1 =",a1,"--- .A2 =", a2,"--- .A3 =", a3)
    } else{
      Equation = paste(predict.name ,"= ",round(coef[1],2)," x",Explic.1.name," + ",round(coef[2],2)," x",Explic.2.name," + ",round(coef[3],2)," x",Explic.3.name,"\n","R2 = ",round(R2,3),"\n","RMSE = ",RMSE = round(RMSE,2),
                       "\n","Stat t : ","  .A1 =",a1,"--- .A2 =", a2,"--- .A3 =", a3)
    }
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Conso.lm = modele$fitted.values 
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time , Modele = Conso.lm)
    visuals = rbind(visual1,visual2)
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Réalité",length(Time)),rep("Modèle",length(Time)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes))+ geom_line(size = 1) + geom_point()+
      ggtitle(Equation) +
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))
    
  }
}












prev <- function(db,linear, predict.name, Explic.1.name, Explic.2.name=NULL, resid,new.explic.1, new.explic.2, comparaison.n,i.debut,i.fin,Explic.3.name,new.explic.3) {
  df = db
  if(sum(is.na(df$Periode.de.Reference)) == 0 & sum(is.na(df$Periode.de.suivi)) == 0){
    max_new = nrow(df)
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.suivi)) != 0){
    max_new = which(is.na(df$Periode.de.suivi))[1] - 1
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.Reference)) != 0){
    max_new = nrow(df)
    max_ref = which(is.na(df$Periode.de.Reference))[1] - 1
  }
  # Formatage dPeriode.de.Referencee la table
  db$Periode.de.Reference = as.Date(db$Periode.de.Reference, tryFormats = c("%d/%m/%Y"))
  db$Periode.de.suivi = as.Date(db$Periode.de.suivi, tryFormats = c("%d/%m/%Y"))
  if(linear == 1){
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    new1 = db[,new.explic.1][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1)
    } else{
      modele = lm(Predict  ~ Explic.1- 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2]
    } else{
      Conso.lm = new1*coef[1]
    }
    conso.real = comparaison
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time.new , Modele = Conso.lm)
    visual3 = data.frame(Time = Time.new , Modele = conso.real)
    
    visuals = rbind(visual1,visual2)
    visuals = rbind(visuals,visual3)
    
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Consomamtion de référence",length(Time)),rep("Consommation estimée sans APE",length(Time.new)),rep("Consommation réelle après APE",length(Time.new)))
    
    debut = db[i.debut,"Periode.de.suivi"]
    fin = db[i.fin,"Periode.de.suivi"]
    
    
    
    
    ### Montrer la surface d'économie rouge verte : 
    
    #Calcul par interpolation
    days = seq(Time.new[1],Time.new[length(Time.new)],by = "day")
    resultat_modele =approx(Time.new, visual2$Modele, xout = days, method="linear", ties="ordered")$y
    resultat_reel =approx(Time.new, visual3$Modele, xout = days , method="linear", ties="ordered")$y
    bdd=data.frame(days,resultat_modele,resultat_reel)
    # différence des couleurs
    bdd_red = bdd
    red = c(bdd_red$resultat_modele <= bdd$resultat_reel)
    bdd_red = bdd_red[red,]
    #
    bdd_green = bdd
    green = c(bdd_green$resultat_modele > bdd$resultat_reel)
    bdd_green = bdd_green[green,]
    for(i in 1:(length(bdd$days)-length(visuals[,1]))){
      a = data.frame(Time = visuals[1,1], Modele = visuals[1,2], Graphes = visuals[1,3])
      names(a)[2] = predict.name
      visuals = rbind(a,visuals)
    }
    # extremité segment
    x1_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y1_red = c(bdd_red$resultat_modele,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    x2_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y2_red = c(bdd_red$resultat_reel,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    #
    x1_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y1_green = c(bdd_green$resultat_modele,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    x2_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y2_green = c(bdd_green$resultat_reel,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes,linetype = Graphes))+ geom_line(size = 1.6) + geom_point(size = 1)+
      scale_color_manual(values = c("grey","red","green"))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(plot.background = element_rect(fill = "#EEEBEB"))+
      geom_vline(xintercept = seq(Time[length(Time)],Time.new[1], by="day"),linetype = "dotdash", colour = "grey")+
      geom_vline(xintercept = debut-15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = fin+15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = seq(debut-15,fin+15, by="day"),linetype = "dotted", colour = "yellow")+      theme(legend.position="top")+
      scale_linetype_manual(values=c("dashed", "dashed","solid"))+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))+
      geom_segment(mapping=aes(x=x1_red,
                               y=y1_red,
                               xend=x2_red,
                               yend=y2_red),
                   color="red",linetype = "dotted",size = 0.7)+
      geom_segment(mapping=aes(x=x1_green,
                               y=y1_green,
                               xend=x2_green,
                               yend=y2_green),
                   color="#00FF00",linetype = "dotted",size = 0.7)
    
    
    
  } else if (linear == 2) {
    
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    new1 = db[,new.explic.1][1:max_new]
    new2 = db[,new.explic.2][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 - 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2] + new2*coef[3]
    } else{
      Conso.lm = new1*coef[1] + new2*coef[2]
    }
    conso.real = comparaison
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time.new , Modele = Conso.lm)
    visual3 = data.frame(Time = Time.new , Modele = conso.real)
    
    visuals = rbind(visual1,visual2)
    visuals = rbind(visuals,visual3)
    
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Consomamtion de référence",length(Time)),rep("Consommation estimée sans APE",length(Time.new)),rep("Consommation réelle après APE",length(Time.new)))
    
    debut = db[i.debut,"Periode.de.suivi"]
    fin = db[i.fin,"Periode.de.suivi"]
    
    
    ### Montrer la surface d'économie rouge verte : 
    
    #Calcul par interpolation
    days = seq(Time.new[1],Time.new[length(Time.new)],by = "day")
    resultat_modele =approx(Time.new, visual2$Modele, xout = days, method="linear", ties="ordered")$y
    resultat_reel =approx(Time.new, visual3$Modele, xout = days , method="linear", ties="ordered")$y
    bdd=data.frame(days,resultat_modele,resultat_reel)
    # différence des couleurs
    bdd_red = bdd
    red = c(bdd_red$resultat_modele <= bdd$resultat_reel)
    bdd_red = bdd_red[red,]
    #
    bdd_green = bdd
    green = c(bdd_green$resultat_modele > bdd$resultat_reel)
    bdd_green = bdd_green[green,]
    for(i in 1:(length(bdd$days)-length(visuals[,1]))){
      a = data.frame(Time = visuals[1,1], Modele = visuals[1,2], Graphes = visuals[1,3])
      names(a)[2] = predict.name
      visuals = rbind(a,visuals)
    }
    # extremité segment
    x1_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y1_red = c(bdd_red$resultat_modele,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    x2_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y2_red = c(bdd_red$resultat_reel,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    #
    x1_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y1_green = c(bdd_green$resultat_modele,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    x2_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y2_green = c(bdd_green$resultat_reel,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes,linetype = Graphes))+ geom_line(size = 1.6) + geom_point(size = 1)+
      scale_color_manual(values = c("grey","red","green"))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(plot.background = element_rect(fill = "#EEEBEB"))+
      geom_vline(xintercept = seq(Time[length(Time)],Time.new[1], by="day"),linetype = "dotdash", colour = "grey")+
      geom_vline(xintercept = debut-15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = fin+15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = seq(debut-15,fin+15, by="day"),linetype = "dotted", colour = "yellow")+      theme(legend.position="top")+
      scale_linetype_manual(values=c("dashed", "dashed","solid"))+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))+
      geom_segment(mapping=aes(x=x1_red,
                               y=y1_red,
                               xend=x2_red,
                               yend=y2_red),
                   color="red",linetype = "dotted",size = 0.7)+
      geom_segment(mapping=aes(x=x1_green,
                               y=y1_green,
                               xend=x2_green,
                               yend=y2_green),
                   color="#00FF00",linetype = "dotted",size = 0.7)
      
    
  } else if(linear == 3){
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    Explic.3 = db[,Explic.3.name][1:max_ref]
    
    new1 = db[,new.explic.1][1:max_new]
    new2 = db[,new.explic.2][1:max_new]
    new3 = db[,new.explic.3][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3 - 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2] + new2*coef[3] + new3*coef[4]
    } else{
      Conso.lm = new1*coef[1] + new2*coef[2] + new3*coef[3]
    }
    conso.real = comparaison
    
    visual1 = data.frame(Time = Time , Modele = Conso)
    visual2 = data.frame(Time = Time.new , Modele = Conso.lm)
    visual3 = data.frame(Time = Time.new , Modele = conso.real)
    
    visuals = rbind(visual1,visual2)
    visuals = rbind(visuals,visual3)
    
    names(visuals) = c("Time",predict.name)
    visuals$Graphes=c(rep("Consomamtion de référence",length(Time)),rep("Consommation estimée sans APE",length(Time.new)),rep("Consommation réelle après APE",length(Time.new)))
    
    debut = db[i.debut,"Periode.de.suivi"]
    fin = db[i.fin,"Periode.de.suivi"]
    
    
    ### Montrer la surface d'économie rouge verte : 
    
    #Calcul par interpolation
    days = seq(Time.new[1],Time.new[length(Time.new)],by = "day")
    resultat_modele =approx(Time.new, visual2$Modele, xout = days, method="linear", ties="ordered")$y
    resultat_reel =approx(Time.new, visual3$Modele, xout = days , method="linear", ties="ordered")$y
    bdd=data.frame(days,resultat_modele,resultat_reel)
    # différence des couleurs
    bdd_red = bdd
    red = c(bdd_red$resultat_modele <= bdd$resultat_reel)
    bdd_red = bdd_red[red,]
    #
    bdd_green = bdd
    green = c(bdd_green$resultat_modele > bdd$resultat_reel)
    bdd_green = bdd_green[green,]
    for(i in 1:(length(bdd$days)-length(visuals[,1]))){
      a = data.frame(Time = visuals[1,1], Modele = visuals[1,2], Graphes = visuals[1,3])
      names(a)[2] = predict.name
      visuals = rbind(a,visuals)
    }
    # extremité segment
    x1_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y1_red = c(bdd_red$resultat_modele,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    x2_red = c(bdd_red$days,rep(bdd_red$days[1],nrow(bdd) - nrow(bdd_red)))
    y2_red = c(bdd_red$resultat_reel,rep(bdd_red$resultat_modele[1],nrow(bdd) - nrow(bdd_red)))
    #
    x1_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y1_green = c(bdd_green$resultat_modele,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    x2_green = c(bdd_green$days,rep(bdd_green$days[1],nrow(bdd) - nrow(bdd_green)))
    y2_green = c(bdd_green$resultat_reel,rep(bdd_green$resultat_modele[1],nrow(bdd) - nrow(bdd_green)))
    
    ggplot(visuals, aes(visuals[,1],visuals[,2],col=Graphes,linetype = Graphes))+ geom_line(size = 1.6) + geom_point(size = 1)+
      scale_color_manual(values = c("grey","red","green"))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylab(predict.name)+
      xlab("Temps")+
      theme(plot.background = element_rect(fill = "#EEEBEB"))+
      geom_vline(xintercept = seq(Time[length(Time)],Time.new[1], by="day"),linetype = "dotdash", colour = "grey")+
      geom_vline(xintercept = debut-15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = fin+15, colour = "black",linetype = "solid")+
      geom_vline(xintercept = seq(debut-15,fin+15, by="day"),linetype = "dotted", colour = "yellow")+      theme(legend.position="top")+
      scale_linetype_manual(values=c("dashed", "dashed","solid"))+
      theme(
        plot.background = element_rect(fill = "#ffffff"),
        panel.background = element_rect(fill = "#ffffff", colour = "#ffffff",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"),
        legend.key = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill="#ffffff",
                                         size=0.5, linetype="solid", 
                                         colour ="#ffffff"))+
      geom_segment(mapping=aes(x=x1_red,
                               y=y1_red,
                               xend=x2_red,
                               yend=y2_red),
                   color="red",linetype = "dotted",size = 0.7)+
      geom_segment(mapping=aes(x=x1_green,
                               y=y1_green,
                               xend=x2_green,
                               yend=y2_green),
                   color="#00FF00",linetype = "dotted",size = 0.7)
  }
}


economie <- function(db,linear, predict.name, Explic.1.name, Explic.2.name=NULL, resid,new.explic.1, new.explic.2, comparaison.n,i.debut,i.fin, Explic.3.name,new.explic.3) {
  
  df = db
  if(sum(is.na(df$Periode.de.Reference)) == 0 & sum(is.na(df$Periode.de.suivi)) == 0){
    max_new = nrow(df)
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.suivi)) != 0){
    max_new = which(is.na(df$Periode.de.suivi))[1] - 1
    max_ref = nrow(df)
  } else if (sum(is.na(df$Periode.de.Reference)) != 0){
    max_new = nrow(df)
    max_ref = which(is.na(df$Periode.de.Reference))[1] - 1
  }
  
  if(linear == 1){
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    new1 = db[,new.explic.1][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1)
    } else{
      modele = lm(Predict  ~ Explic.1- 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2]
    } else{
      Conso.lm = new1*coef[1]
    }
    
    
    Conso.lm = Conso.lm[i.debut:i.fin]
    comparaison = comparaison[i.debut:i.fin]
    n.Periode.de.Reference = length(i.debut:i.fin)
    conso = round(sum(Conso.lm - comparaison))
    moyenne = conso/n.Periode.de.Reference
    return(list(conso = conso, n.Periode.de.Reference = n.Periode.de.Reference,moyenne=moyenne))
    
  } else if (linear == 2) {
    
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    new1 = db[,new.explic.1][1:max_new]
    new2 = db[,new.explic.2][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 - 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2] + new2*coef[3]
    } else{
      Conso.lm = new1*coef[1] + new2*coef[2]
    }
    
    Conso.lm = Conso.lm[i.debut:i.fin]
    comparaison = comparaison[i.debut:i.fin]
    n.Periode.de.Reference = length(i.debut:i.fin)
    conso = round(sum(Conso.lm - comparaison))
    moyenne = conso/n.Periode.de.Reference
    return(list(conso = conso, n.Periode.de.Reference = n.Periode.de.Reference,moyenne=moyenne))
  } else if (linear == 3){
    # Lecture
    Predict = db[,predict.name][1:max_ref]
    Explic.1 = db[,Explic.1.name][1:max_ref]
    Explic.2 = db[,Explic.2.name][1:max_ref]
    Explic.3 = db[,Explic.3.name][1:max_ref]
    new1 = db[,new.explic.1][1:max_new]
    new2 = db[,new.explic.2][1:max_new]
    new3 = db[,new.explic.3][1:max_new]
    
    comparaison = db[,comparaison.n][1:max_new]
    # Modele
    if(resid){
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3)
    } else{
      modele = lm(Predict  ~ Explic.1 + Explic.2 + Explic.3 - 1)
    }
    # coef du modele
    coef = coef(modele)
    # plot
    Time = db$Periode.de.Reference[1:max_ref]
    Conso = db[,predict.name][1:max_ref]
    Time.new = db$Periode.de.suivi[1:max_new]
    if(resid){
      Conso.lm =  coef[1] + new1*coef[2] + new2*coef[3] + new3*coef[4]
    } else{
      Conso.lm = new1*coef[1] + new2*coef[2] + new3*coef[3]
    }
    
    Conso.lm = Conso.lm[i.debut:i.fin]
    comparaison = comparaison[i.debut:i.fin]
    n.Periode.de.Reference = length(i.debut:i.fin)
    conso = round(sum(Conso.lm - comparaison))
    moyenne = conso/n.Periode.de.Reference
    return(list(conso = conso, n.Periode.de.Reference = n.Periode.de.Reference,moyenne=moyenne))
  }
}



