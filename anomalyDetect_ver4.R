## ２つのグループのデータを結合して、pre-postの比較を行う。

fileRef1 <- '//XXXX/03_cl_ibegp/治験実施管理/治験/XXXX/検査関連/データ入力/GROUP1/K11p1_g1_I_pre.txt'
fileRel1 <- '//XXXX/03_cl_ibegp/治験実施管理/治験/XXXX/検査関連/データ入力/GROUP1/K11p1_g1_I_24ｈ.txt'
fileRef2 <- '//XXXX/03_cl_ibegp/治験実施管理/治験/XXXX/検査関連/データ入力/GROUP2/K11p1_g2_I_pre.txt'
fileRel2 <- '//XXXX/03_cl_ibegp/治験実施管理/治験/XXXX/検査関連/データ入力/GROUP2/K11p1_g2_I_24h.txt'

HEADER = FALSE

## describe all items

  koumoku<- c(
    'id','RBC' ,'HGB' ,  'HCT', 'PLT' ,'WBC' ,'Neut', 'Eos','Baso', 'Lym', 'Mono','GLU','AST','ALT','LD'  ,'ALP','r_GT','T_Bil' , 'D_Bil' , 'TP'   ,  'ALB','UN','Cre','UA','Na','K','Cl','CK','T_cho','TG','AMY')

## classify the items by the criterion of the anomaly

  koumoku_a <- c('AST','ALT','LD','ALP','r_GT','T_Bil' , 'D_Bil' ,'UN','CK','TG')
  koumoku_b <- c('RBC','HGB','HCT','PLT','WBC','GLU','TP','ALB','Cre','UA','Na','K','Cl','T_cho','AMY')
  koumoku_c <- c('Neut', 'Eos','Baso', 'Lym', 'Mono')

## describe the upper and lower bound of normality 

standard  <- data.frame(matrix(c(
    NA  , 430 , 14.0 ,  40.0 , 13.0  , 40 , 40.0 ,  2.0 ,  0.0 ,  30.0 ,  3.0  ,  80 , 13 ,  6 , 119 , 115 ,	10 , 0.30 , NA   ,  6.70 ,  4.00 ,  8.0  ,  0.60  , 3.60 , 138  ,  3.6 ,	99 ,	62 , 128 ,	30 ,	42 ,
    NA  , 570 , 18.0 ,	52.0 , 36.0  , 90 ,	70.0 , 	4.0 ,	 1.0 ,	43.0 ,	6.0  , 109 , 33 ,	30 , 229 , 359 ,	47 , 1.20 ,	0.60 ,	8.30 ,	5.00 , 22.0  ,  1.10  ,	7.00 , 146  ,	 4.9 , 109 , 287 , 219 ,	149 ,	132 
                                    ),nrow=2,byrow=TRUE
                                 )
                          )
  # and makes the table
  dimnames(standard)[[1]] <- c('lower','upper')
  dimnames(standard)[[2]] <-    koumoku

  standard

  koumoku_suu    <- length(koumoku)

## read datae from the place set in the head of this PG

  matRef1      <- read.csv(fileRef1
                           ,header=HEADER
                           ,sep='\t')

  matRel1      <- read.csv(fileRel1
                           ,header=HEADER
                           ,sep='\t')
  matRef2      <- read.csv(fileRef2
                        ,header=HEADER
                        ,sep='\t')

  matRel2      <- read.csv(fileRel2
                        ,header=HEADER
                        ,sep='\t')


  dataRef1     <- data.frame(matRef1)
  dataRel1     <- data.frame(matRel1)
  dataRef2     <- data.frame(matRef2)
  dataRel2     <- data.frame(matRel2)

 dataRef <- rbind(dataRef1,dataRef2)
 dataRel <- rbind(dataRel1,dataRel2)

  dimnames(dataRef)[[2]][1:koumoku_suu] <- koumoku
  dimnames(dataRel)[[2]][1:koumoku_suu] <- koumoku

## add column of the count datae of the fractions of WBC

  dataRef$countNeut <- dataRef$WBC * dataRef$Neut
  dataRef$countEos  <- dataRef$WBC * dataRef$Eos
  dataRef$countBaso <- dataRef$WBC * dataRef$Baso
  dataRef$countLym  <- dataRef$WBC * dataRef$Lym
  dataRef$countMono <- dataRef$WBC * dataRef$Mono

  dataRel$countNeut <- dataRel$WBC * dataRel$Neut
  dataRel$countEos  <- dataRel$WBC * dataRel$Eos
  dataRel$countBaso <- dataRel$WBC * dataRel$Baso
  dataRel$countLym  <- dataRel$WBC * dataRel$Lym
  dataRel$countMono <- dataRel$WBC * dataRel$Mono



  ObsRef            <- nrow(dataRef)
  ObsRel            <- nrow(dataRel)  

#Definitions of anomalee
OPS <- options() 
 def_b <- function(x,y,lower,upper){
    if(        x < lower
            && y < lower
            && y < x * 1/2){
        return("zohaku_LOW")
    }
    if(     x > upper
            && y > upper
            && y > x * 2  ){
        return("zohaku_HIGH")
    }
    if(     x >lower
            && x<upper
            && (y>upper)
            &&  (y - x) > x*0.2){
        return(paste('+',(y-x)/x*100,'%',sep=''))
    }
    if(     x >lower
            && x<upper
            && (y<lower)
            &&  (x - y) > x*0.2){
      return(paste('-',print((x-y)/x*100,digits=3),'%',sep=''))
    }
    
    else{
      return('OK')
    }
  }

    def_a <- function(x,y,lower,upper){
      if(     x < upper
              && x > lower
              && (y < lower || y > upper)
              &&  y > upper*1.2){
        return(paste('+',(y-x)/x*100,'%',sep=''))
      }
      if(     (x < lower || x > upper)
              && y > upper
              && y > x * 2  ){
        return("zouhaku_HIGH")
      }
      else{
        return('OK')
    }
   }


    # def_d <- function(引数1,引数2){
    #  if(  条件；ORなら || , ANDなら && ){
    #   return( 条件がTRUEのときに返すもの )
    #  }
    # }

selector <- function(koumoku){
  if (is.element(koumoku,koumoku_a)){
    return( mapply(def_a 
                   ,dataRef[,koumoku]
                   ,dataRel[,koumoku]
                   ,standard['lower',koumoku],standard['upper',koumoku])
            )
  }
  if (is.element(koumoku,koumoku_b)){
     return( mapply(def_b
                   ,dataRef[,koumoku]
                   ,dataRel[,koumoku]
                   ,standard['lower',koumoku],standard['upper',koumoku])
             )
  }
}
              
## detect anormaly

anomaly_detect <- data.frame(matrix(nrow=nrow(dataRef)))
anomaly_detect[,koumoku] <- mapply(selector,koumoku)
anomaly_detect$id  <- dataRef$id


dimnames(anomaly_detect)[[1]] <- anomaly_detect$id
anomaly_detect[,-1]


## 入力データのチェック
dimnames(dataRef)[[1]] <- dataRef[,1]
dimnames(dataRel)[[1]] <- dataRel[,1]

dataRef
dataRel

##　経時的にデータを表示

for ( i in dimnames(dataRef)[[2]] ){
  Ref            <- dataRef[,i]
  Rel            <- dataRel[,i]
  boxplot(cbind(Ref,Rel),main=i)
  print(cbind(Ref,Rel))
}
