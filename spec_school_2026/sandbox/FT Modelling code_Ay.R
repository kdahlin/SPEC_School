library(caret)
library(pls)
library(randomForest)

ctrl<-trainControl(method="LOOCV",savePredictions="final")

run_model<-function(df,y,method){
  
  data<-data.frame(AGB_Mg_ha=y,df)
  
  if(method=="LM"){
    fit<-train(AGB_Mg_ha~.,data=data,method="lm",trControl=ctrl)
  }
  
  if(method=="PLS"){
    fit<-train(AGB_Mg_ha~.,data=data,method="pls",preProcess=c("center","scale"),tuneLength=5,trControl=ctrl)
  }
  
  if(method=="RF"){
    fit<-train(AGB_Mg_ha~.,data=data,method="rf",ntree=500,trControl=ctrl)
  }
  
  pred<-fit$pred$pred
  obs<-fit$pred$obs
  
  data.frame(
    Model=method,
    R2=cor(obs,pred,use="complete.obs")^2,
    RMSE=sqrt(mean((obs-pred)^2))
  )
  
}

lidar_results<-rbind(
  run_model(lidar,y,"LM"),
  run_model(lidar,y,"PLS"),
  run_model(lidar,y,"RF")
)

hs_results<-rbind(
  run_model(hs,y,"LM"),
  run_model(hs,y,"PLS"),
  run_model(hs,y,"RF")
)

combined_results<-rbind(
  run_model(combined,y,"LM"),
  run_model(combined,y,"PLS"),
  run_model(combined,y,"RF")
)

final_results<-rbind(
  cbind(Sensor="LiDAR",lidar_results),
  cbind(Sensor="Hyperspectral",hs_results),
  cbind(Sensor="Combined",combined_results)
)

final_results

df_cor <- combined
df_cor$AGB_Mg_ha <- y

cors <- cor(df_cor, use="complete.obs")[,"AGB_Mg_ha"]

cors <- cors[names(cors)!="AGB_Mg_ha"]

corr_df <- data.frame(
  Variable=names(cors),
  Correlation=cors
)

corr_df <- corr_df[order(abs(corr_df$Correlation), decreasing=TRUE),]

ggplot(corr_df, aes(x=reorder(Variable,Correlation), y=Correlation))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  xlab("Predictor")+
  ylab("Correlation with Biomass")+
  ggtitle("Predictor–Biomass Relationships (Combined Dataset)")
