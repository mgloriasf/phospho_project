## Detecció de pèptids fosforil.lats expressats de forma diferencial en dos subtipus de tumors

## Creació de un contenidor similar al que al que obtindríam amb SummarizedExperiment.


#Es llegeixen les dades a analitzar

library(readxl)
fosfo_db<-read_xlsx("~/dadesomiques/TIO2+PTYR-human-MSS+MSIvsPD.XLSX")

# S'identifiquen les columnes de la taula i s'obté un dataframe de la taula.
head(fosfo_db)
fosfo_db_df<-as.data.frame(fosfo_db)

#Per cambiar columnes per files, s'obtenen llistes de cada columna i s'uneixen en files:

list<-as.list(fosfo_db_df)
str(list)
se_container<-as.data.frame(do.call(rbind,list))
dim(se_container)



## Anàlisi de dades

You can also embed plots, for example:

# Exploració de les dades

str(fosfo_db_df)
summary(fosfo_db_df)
fosfo_samples<-as.matrix(fosfo_db_df[,5:16])
colnames(fosfo_samples)<-c("M1_1_MSS","M1_2_MSS","M5_1_MSS ","M5_2_MSS ","T49_1_MSS","T49_2_MSS","M42_1_PD","M42_2_PD","M43_1_PD","M43_2_PD","M64_1_PD","M64_2_PD")
fosfo_samples<-as.matrix(fosfo_db_df[,5:16])
str(fosfo_samples)
summary(fosfo_samples)
dim(fosfo_samples)

# Representació de boxplot de cadascuna de les mostres.

peptide<-c(1:1438)
z<-cbind(fosfo_samples,peptide)
boxplot(fosfo_samples)
stripchart(fosfo_samples,method="jitter", pch=19,add=TRUE,col="blue")

# diferència entre duplicats

x1<-fosfo_samples[,1]
x2<-fosfo_samples[,2]
l<-as.matrix(x1,x2)
t.test(x=x1,y=x2,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)

x3<-fosfo_samples[,3]
x4<-fosfo_samples[,4]
t.test(x=x3,y=x4,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)


x5<-fosfo_samples[,5]
x6<-fosfo_samples[,6]
t.test(x=x5,y=x6,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)


x7<-fosfo_samples[,7]
x8<-fosfo_samples[,8]
t.test(x=x7,y=x8,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)


x9<-fosfo_samples[,9]
x10<-fosfo_samples[,10]
t.test(x=x9,y=x10,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)


x11<-fosfo_samples[,11]
x12<-fosfo_samples[,12]
t.test(x=x11,y=x12,alternative="two.sided",mu=0, paired=TRUE,conf.level=0.95)



#Normalitat de les dades

library(nortest) 
lillie.test(x1)
lillie.test(x2)
lillie.test(x3)
lillie.test(x4)
lillie.test(x5)
lillie.test(x6)
lillie.test(x7)
lillie.test(x8)
lillie.test(x9)
lillie.test(x10)
lillie.test(x11)
lillie.test(x12)







