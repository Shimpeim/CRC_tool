fileWBC <- '//Ibegpfsv/03_cl_ibegp/治験実施管理/治験/K11P1_レトロゾール（共和）/検査関連/データ入力/WBC/K11p1_WBC.txt'
fileNUT <- '//Ibegpfsv/03_cl_ibegp/治験実施管理/治験/K11P1_レトロゾール（共和）/検査関連/データ入力/Nut/K11p1_Nut.txt'

HEADER = TRUE

matWBC      <- read.csv(fileWBC
                        ,header=HEADER
                        ,sep='\t')
dataWBC     <- na.omit(data.frame(matWBC))

matNUT      <- read.csv(fileNUT
                        ,header=HEADER
                        ,sep='\t')
dataNUT     <- na.omit(data.frame(matNUT))

dimnames(dataWBC)[[1]] <- dataWBC[,1]
colnames(dataWBC)[1]   <- 'id'
dimnames(dataNUT)[[1]] <- dataNUT[,1]
colnames(dataNUT)[1]   <- 'id'

countNUT <-  dataNUT[,2:length(dataNUT)]*dataWBC[,2:length(dataWBC)]/100

dataWBC
countNUT

write.csv(countNUT,'//Ibegpfsv/03_cl_ibegp/治験実施管理/治験/K11P1_レトロゾール（共和）/検査関連/データ入力/Nut/K11p1_cNut.csv')
boxplot(dataWBC[,2:length(dataWBC)],main='count WBC')
boxplot(dataNUT[,2:length(dataNUT)],main='% Nuet')
boxplot(countNUT,main='count Neut')


require(ggplot2)
require(reshape2)

mWBC <- reshape2::melt(dataWBC,id.var=c('id'))
  p   <- ggplot(mWBC,aes('count','WBC',group=id))
  exp <- p + geom_line(aes(x      = variable,
                           y      = value,
                           colour = id
                           )
                       )+
  scale_x_discrete   ('time_point') +
  scale_y_continuous('count of WBC',limits=c(min(mWBC$value),max(mWBC$value)))
  
plot(exp)  

countNUT$id <- dimnames(countNUT)[[1]]
mcountNUT <- reshape2::melt(countNUT,id.var=c('id'))
p   <- ggplot(mcountNUT,aes('count','Nuet',group=id))
exp <- p + geom_line(aes(x      = variable,
                         y      = value,
                         colour = id
)
)+
  scale_x_discrete   ('time_point') +
  scale_y_continuous('count of Nuet',limits=c(min(mcountNUT$value),max(mcountNUT$value)))

plot(exp)  

