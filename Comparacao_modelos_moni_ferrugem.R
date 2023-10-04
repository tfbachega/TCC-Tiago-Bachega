#Analise Comparativa de Modelo Linear vs 
#Modelos Log Logisticos de 2, 3 e 4 fatores para dados de Bioensaios de
#Monitoramento de Ferrugem da soja a diferentes produtos fungicidas

#Inatalação dos pacotes:

pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl","car",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","drc",
             "caret","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Leitura do bacnco de dados

a<-read_xlsx("C:/Users/bachegti/OneDrive - BASF/Documents/1-MBA/1-TCC/PHAKPA.xlsx")

#Transposição do Banco de dados

b<-a%>%dplyr::select(Produto,`Dosagem (log ppm)`,`inibição (%)`,Rep)%>%
  pivot_wider(names_from = Produto,
                   values_from = c(`inibição (%)`))%>%
  rename(ppm1=`Dosagem (log ppm)`,
         P1="1",
         P2="2",
         P3="3",
         P4="4",
         P5="5",
         P6="6")%>%mutate(ppm=10^ppm1)

#modelos lineares
linear1<-lm(P1~ppm1,data=b)
linear2<-lm(P2~ppm1,data=b)
linear3<-lm(P3~ppm1,data=b)
linear4<-lm(P4~ppm1,data=b)
linear5<-lm(P5~ppm1,data=b)
linear6<-lm(P6~ppm1,data=b)

#Modelos sigmoides 2 fatores
ll2m1<-drm(P1/100~ppm,data=b,fct = LL.2())
ll2m2<-drm(P2/100~ppm,data=b,fct = LL.2())
ll2m3<-drm(P3/100~ppm,data=b,fct = LL.2())
ll2m4<-drm(P4/100~ppm,data=b,fct = LL.2())
ll2m5<-drm(P5/100~ppm,data=b,fct = LL.2())
ll2m6<-drm(P6/100~ppm,data=b,fct = LL.2())

#Modelos sigmoides 3 fatores
ll3m1<-drm(P1/100~ppm,data=b,fct = LL.3())
ll3m2<-drm(P2/100~ppm,data=b,fct = LL.3())
ll3m3<-drm(P3/100~ppm,data=b,fct = LL.3())
ll3m4<-drm(P4/100~ppm,data=b,fct = LL.3())
ll3m5<-drm(P5/100~ppm,data=b,fct = LL.3())
ll3m6<-drm(P6/100~ppm,data=b,fct = LL.3())

#Modelos sigmoides 4 fatores
ll4m1<-drm(P1/100~ppm,data=b,fct = LL.4())
ll4m2<-drm(P2/100~ppm,data=b,fct = LL.4())
ll4m3<-drm(P3/100~ppm,data=b,fct = LL.4())
ll4m4<-drm(P4/100~ppm,data=b,fct = LL.4())
ll4m5<-drm(P5/100~ppm,data=b,fct = LL.4())
ll4m6<-drm(P6/100~ppm,data=b,fct = LL.4())


########
#Analises Por Produto vs Modelo
#Produto 1 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m1,newdata = newdata)*100
b$pred2.1<-predict(linear1,b)

b%>%group_by(ppm1)%>%
  summarise(P1=mean(P1),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P1,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P1,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
    scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P1")+
  labs(x="Log ppm",y="% Inibição")

#Produto 1 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m1,newdata = newdata)*100
b$pred2.1<-predict(linear1,b)

b%>%group_by(ppm1)%>%
  summarise(P1=mean(P1),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P1,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P1,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P1")+
  labs(x="Log ppm",y="% Inibição")

#Produto 1 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m1,newdata = newdata)*100
b$pred2.1<-predict(linear1,b)

b%>%group_by(ppm1)%>%
  summarise(P1=mean(P1),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P1,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P1,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P1")+
  labs(x="Log ppm",y="% Inibição")

#Produto 2 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m2,newdata = newdata)*100
b$pred2.1<-predict(linear2,b)

b%>%group_by(ppm1)%>%
  summarise(P2=mean(P2),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P2,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P2,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P2")+
  labs(x="Log ppm",y="% Inibição")

#Produto 2 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m2,newdata = newdata)*100
b$pred2.1<-predict(linear2,b)

b%>%group_by(ppm1)%>%
  summarise(P2=mean(P2),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P2,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P2,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P2")+
  labs(x="Log ppm",y="% Inibição")

#Produto 2 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m2,newdata = newdata)*100
b$pred2.1<-predict(linear2,b)

b%>%group_by(ppm1)%>%
  summarise(P2=mean(P2),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P2,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P2,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P2")+
  labs(x="Log ppm",y="% Inibição")

#Produto 3 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m3,newdata = newdata)*100
b$pred2.1<-predict(linear3,b)

b%>%group_by(ppm1)%>%
  summarise(P3=mean(P3),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P3,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P3,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P3")+
  labs(x="Log ppm",y="% Inibição")

#Produto 3 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m3,newdata = newdata)*100
b$pred2.1<-predict(linear3,b)

b%>%group_by(ppm1)%>%
  summarise(P3=mean(P3),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P3,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P3,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P3")+
  labs(x="Log ppm",y="% Inibição")

#Produto 3 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m3,newdata = newdata)*100
b$pred2.1<-predict(linear3,b)

b%>%group_by(ppm1)%>%
  summarise(P3=mean(P3),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P3,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P3,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P3")+
  labs(x="Log ppm",y="% Inibição")

#Produto 4 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m4,newdata = newdata)*100
b$pred2.1<-predict(linear4,b)

b%>%group_by(ppm1)%>%
  summarise(P4=mean(P4),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P4,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P4,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P4")+
  labs(x="Log ppm",y="% Inibição")

#Produto 4 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m4,newdata = newdata)*100
b$pred2.1<-predict(linear4,b)

b%>%group_by(ppm1)%>%
  summarise(P4=mean(P4),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P4,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P4,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P4")+
  labs(x="Log ppm",y="% Inibição")

#Produto 4 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m4,newdata = newdata)*100
b$pred2.1<-predict(linear4,b)

b%>%group_by(ppm1)%>%
  summarise(P4=mean(P4),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P4,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P4,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P4")+
  labs(x="Log ppm",y="% Inibição")

#Produto 5 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m5,newdata = newdata)*100
b$pred2.1<-predict(linear5,b)

b%>%group_by(ppm1)%>%
  summarise(P5=mean(P5),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P5,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P5,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P5")+
  labs(x="Log ppm",y="% Inibição")

#Produto 5 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m5,newdata = newdata)*100
b$pred2.1<-predict(linear5,b)

b%>%group_by(ppm1)%>%
  summarise(P5=mean(P5),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P5,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P5,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P5")+
  labs(x="Log ppm",y="% Inibição")

#Produto 5 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m5,newdata = newdata)*100
b$pred2.1<-predict(linear5,b)

b%>%group_by(ppm1)%>%
  summarise(P5=mean(P5),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P5,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P5,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P5")+
  labs(x="Log ppm",y="% Inibição")

#Produto 6 - Linear vs LL2
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll2m6,newdata = newdata)*100
b$pred2.1<-predict(linear6,b)

b%>%group_by(ppm1)%>%
  summarise(P6=mean(P6),
            ll2=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P6,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll2,color="modelo LL.2"))+
  geom_smooth(aes(x=ppm1,y=P6,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll2,color="modelo LL.2"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P6")+
  labs(x="Log ppm",y="% Inibição")

#Produto 6 - Linear vs LL3
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll3m6,newdata = newdata)*100
b$pred2.1<-predict(linear6,b)

b%>%group_by(ppm1)%>%
  summarise(P6=mean(P6),
            ll3=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P6,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll3,color="modelo LL.3"))+
  geom_smooth(aes(x=ppm1,y=P6,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll3,color="modelo LL.3"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P6")+
  labs(x="Log ppm",y="% Inibição")

#Produto 6 - Linear vs LL4
newdata <- expand.grid(b$ppm)
b$pred1.1<-predict(ll4m6,newdata = newdata)*100
b$pred2.1<-predict(linear6,b)

b%>%group_by(ppm1)%>%
  summarise(P6=mean(P6),
            ll4=mean(pred1.1),
            Linear=mean(pred2.1))%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=P6,color="Valor Medido"))+
  geom_point(aes(x=ppm1,y=Linear,color="Modelo Linear"))+
  geom_point(aes(x=ppm1,y=ll4,color="modelo LL.4"))+
  geom_smooth(aes(x=ppm1,y=P6,color="Valor Medido"),se=F)+
  geom_smooth(aes(x=ppm1,y=Linear,color="Modelo Linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=ll4,color="modelo LL.4"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000"))+
  ggtitle("Comparação entre modelos e valor medido - P6")+
  labs(x="Log ppm",y="% Inibição")

#função comparativa de modelos
comparar<-function(a,b,c,d){
  data.frame(Linear = logLik(a),
             Sig_2Fatores = logLik(b),
             Sig_3Fatores = logLik(c),
             Sig_4Fatores = logLik(d)) %>%
    rename(Linear = 1,
           Sig_2Fatores = 2,
           Sig_3Fatores = 3,
           Sig_4Fatores = 4) %>%
    melt() %>%
    ggplot(aes(x = variable, y = value, fill = (variable))) +
    geom_bar(stat = "identity") +
    geom_label(aes(label = (round(value,3))), hjust = 0.5, color = "white", size = 3) +
    labs(title = "Comparando Log-Likelihood", 
         y = "LogLik", 
         x = "Modelo Proposto") +
    #coord_flip() +
    scale_fill_manual("Legenda:",
                      values = c("grey25","grey45","darkorchid","bisque4")) +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.line = element_line())
  
  
}
#P1
p1<-comparar(linear1,ll2m1,ll3m1,ll4m1)+ggtitle("comparando para P1")
plot(p1)

#P2
p2<-comparar(linear2,ll2m2,ll3m2,ll4m2)+ggtitle("comparando para P2")
plot(p2)

#P3
p3<-comparar(linear3,ll2m3,ll3m3,ll4m3)+ggtitle("comparando para P3")
plot(p3)

#P4
p4<-comparar(linear4,ll2m4,ll3m4,ll4m4)+ggtitle("comparando para P4")
plot(p4)

#P5
p5<-comparar(linear5,ll2m5,ll3m5,ll4m5)+ggtitle("comparando para P5")
plot(p5)

#P6
p6<-comparar(linear6,ll2m6,ll3m6,ll4m6)+ggtitle("comparando para P6")
plot(p6)

gridExtra::grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3)

gridExtra::grid.arrange(p1,p2,p3,nrow=3)

gridExtra::grid.arrange(p4,p5,p6,nrow=3)

#tabela EC50 e EC90

EC50p1<-drc::ED(ll2m1,50)[1]
EC50p2<-drc::ED(ll2m2,50)[1]
EC50p3<-drc::ED(ll2m3,50)[1]
EC50p4<-drc::ED(ll2m4,50)[1]
EC50p5<-drc::ED(ll2m5,50)[1]
EC50p6<-drc::ED(ll2m6,50)[1]

EC90p1<-drc::ED(ll2m1,90)[1]
EC90p2<-drc::ED(ll2m2,90)[1]
EC90p3<-drc::ED(ll2m3,90)[1]
EC90p4<-drc::ED(ll2m4,90)[1]
EC90p5<-drc::ED(ll2m5,90)[1]
EC90p6<-drc::ED(ll2m6,90)[1]


EC50p1<-drc::ED(ll2m1,50)[1]
EC50p2<-drc::ED(ll2m2,50)[1]
EC50p3<-drc::ED(ll2m3,50)[1]
EC50p4<-drc::ED(ll2m4,50)[1]
EC50p5<-drc::ED(ll2m5,50)[1]
EC50p6<-drc::ED(ll2m6,50)[1]

EC90p1<-drc::ED(ll2m1,90)[1]
EC90p2<-drc::ED(ll2m2,90)[1]
EC90p3<-drc::ED(ll2m3,90)[1]
EC90p4<-drc::ED(ll2m4,90)[1]
EC90p5<-drc::ED(ll2m5,90)[1]
EC90p6<-drc::ED(ll2m6,90)[1]

EC50<-as.data.frame(rbind(EC50p1,EC50p2,EC50p3,EC50p4,EC50p5,EC50p6))
EC90<-as.data.frame(rbind(EC90p1,EC90p2,EC90p3,EC90p4,EC90p5,EC90p6))
ECtable<-cbind(EC50,EC90)
colnames(ECtable)<-c("EC50","EC90")
rownames(ECtable)<-c("P1","P2","P3","P4","P5","P6")
write.csv(ECtable,"ec.csv")

#encontrnado os valores de EC para os modelos Lineares

ll2m1$coefficients[1]
linear1$coefficientss[1]
linear1$coefficients[1]

findx<-function(x,y){
  int<-x$coefficients[1]
  slope<-x$coefficients[2]
  ec=(y-int)/slope
  ecf=10^(ec)
  print(ecf)
  
}

findx(linear6,90)


demo<-expand.grid(runif(100,min = 0,max = 50))
demo2<-expand.grid(runif(100,min = 0,max = 0.1))
demo2.1<-expand.grid(runif(100,min = 0.1,max = 1))
demo3<-rbind(demo,demo2,demo2.1)%>%rename(ppm=Var1)
demo3$predLL2<-predict(ll2m2,demo3)*100
demo3$predLL3<-predict(ll3m2,demo3)*100
demo3$predLL4<-predict(ll4m2,demo3)*100
demo3$ppm1<-log(demo3$ppm,base = 10)
demo3$predlin<-predict(linear2,demo3)

demo3%>%
  ggplot()+
  geom_point(aes(x=ppm1,y=predLL2,color="Modelo LL2F"))+
  geom_point(aes(x=ppm1,y=predlin,color="Modelo linear"))+
  geom_point(aes(x=ppm1,y=predLL3,color="Modelo LL3F"))+
  geom_point(aes(x=ppm1,y=predLL4,color="Modelo LL4F"))+
  geom_smooth(aes(x=ppm1,y=predLL2,color="Modelo LL2F"),se=F)+
  geom_smooth(aes(x=ppm1,y=predlin,color="Modelo linear"),se=F)+
  geom_smooth(aes(x=ppm1,y=predLL3,color="Modelo LL3F"),se=F)+
  geom_smooth(aes(x=ppm1,y=predLL4,color="Modelo LL4F"),se=F)+
  scale_color_manual("Modelos:",values = c("#0000FF","#00FF00","#FF0000","#D95F02"))+
  ggtitle("Comparação entre modelos e valor medido - P3")+
  labs(x="Log ppm",y="% Inibição")
  labs(x="log ppm",y="% inibição previsto")+
  ggtitle("Simulação comparativa entre modelos - P3")
