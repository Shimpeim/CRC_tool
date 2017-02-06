

file_name <- 'C:/R/k11test_2.csv'
HEADER = FALSE

#DATA = Obs by koumoku

  koumoku        <- c('id','RBC_pre','HGB_pre','HCT_pre','PLT_pre','WBC_pre','Neut_pre','Eos_pre','Baso_pre','Lym_pre','Mono_pre','GLU_pre','AST_pre','ALT_pre','LD_pre','ALP_pre','r_GT_pre','T_Bil_pre','D_Bil_pre','TP_pre','ALB_pre','UN_pre','Cre_pre','UA_pre','Na_pre','K_pre','Cl_pre','CK_pre','T_cho_pre','TG_pre','AMY_pre',
                      'RBC_post','HGB_post','HCT_post','PLT_post','WBC_post','Neut_post','Eos_post','Baso_post','Lym_post','Mono_post','GLU_post','AST_post','ALT_post','LD_post','ALP_post','r_GT_post','T_Bil_post','D_Bil_post','TP_post','ALB_post','UN_post','Cre_post','UA_post','Na_post','K_post','Cl_post','CK_post','T_cho_post','TG_post','AMY_post'
                      )
  koumoku_suu    <- length(koumoku)
  dat_mat        <- read.csv(file_name,header=HEADER,sep='\t')
  data           <- data.frame(dat_mat)
  dimnames(data)[[2]][1:koumoku_suu] <- koumoku
  Obs                                <- nrow(data)

#Definitions of anomalee

  anomalee <- data.frame()
  def_b <- function(x,y,lower,upper){
    if(     x < lower
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
            && (y>upper|| y<lower)
            &&  (y - x) > x*0.2){
        return('+120%')
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
        return("+120%")
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
              
              #detect anormalee
anomaly_detect <- data.frame(matrix(nrow=nrow(data)))
dimnames(anomaly_detect)[[2]] <- c('id','RBC','HGB','HCT','PLT','WBC','Neut','Eos','Baso','Lym','Mono','GLU','AST','ALT','LD','ALP','r_GT','T_Bil','D_Bil','TP','ALB','UN','Cre','UA','Na','K','Cl','CK','T_cho','TG','AMY')
attach(data)

anomaly_detect$id  <- id
anomaly_detect$RBC <- mapply(def_b , RBC_pre, RBC_post,   430, 570  )
anomaly_detect$HGB <- mapply(def_b , HGB_pre, HGB_post,  14.0,  18.0)
anomaly_detect$HCT <- mapply(def_b , HCT_pre, HCT_post,  40.0,  52.0)
anomaly_detect$PLT <- mapply(def_b , PLT_pre, PLT_post,  13.0,  36.0)
anomaly_detect$WBC <- mapply(def_b , WBC_pre, WBC_post,    40,  90  )

anomaly_detect$Neut <- mapply(def_b , Neut_pre, Neut_post,  40.0 , 70.0  )
anomaly_detect$Eos  <- mapply(def_b ,  Eos_pre,  Eos_post,   2.0 ,  4.0  )
anomaly_detect$Baso <- mapply(def_b , Baso_pre, Baso_post,   0.0 ,  1.0  )
anomaly_detect$Lym  <- mapply(def_b ,  Lym_pre,  Lym_post,   30  ,  43   )
anomaly_detect$Mono <- mapply(def_b , Mono_pre, Mono_post,    3.0,   6.0 )

anomaly_detect$GLU <- mapply(def_b ,  GLU_pre,   GLU_post,     80 , 109   )

anomaly_detect$AST <- mapply(def_a ,  AST_pre,   AST_post,     13 ,  33   )
anomaly_detect$ALT <- mapply(def_a ,  ALT_pre,   ALT_post,      6 ,  30   )
anomaly_detect$LD  <- mapply(def_a ,   LD_pre,    LD_post,    119 , 229   )
anomaly_detect$ALP <- mapply(def_a ,  ALP_pre,   ALP_post,    115 , 359   )

anomaly_detect$r_GT  <- mapply(def_a,   r_GT_pre,  r_GT_post,  10 , 47   )
anomaly_detect$T_Bil <- mapply(def_a,  T_Bil_pre, T_Bil_post, 0.30,1.20  )
anomaly_detect$D_Bil <- mapply(def_a,  D_Bil_pre, D_Bil_post,    0,0.60  )
anomaly_detect$TP    <- mapply(def_b,     TP_pre,    TP_post, 6.70,8.30  )
anomaly_detect$ALB   <- mapply(def_b,    ALB_pre,   ALB_post, 4.00,5.00  )

anomaly_detect$UN  <- mapply(  def_a,     UN_pre,  UN_post,  8.0, 22.0  )
anomaly_detect$Cre <- mapply(  def_b,    Cre_pre, Cre_post,  0.60, 1.10 )
anomaly_detect$UA  <- mapply(  def_b,     UA_pre,  UA_post,  3.60, 7.00 )
anomaly_detect$Na  <- mapply(  def_b,     Na_pre,  Na_post, 138  ,146   )
anomaly_detect$K   <- mapply(  def_b,      K_pre,   K_post,   3.6,  4.9 )

anomaly_detect$Cl    <- mapply(  def_b,     Cl_pre,     Cl_post,  99,  109)
anomaly_detect$CK    <- mapply(  def_a,     CK_pre,     CK_post,  62,  287)
anomaly_detect$T_cho <- mapply(  def_b,  T_cho_pre,  T_cho_post, 128,  219)
anomaly_detect$TG    <- mapply(  def_a,     TG_pre,     TG_post,  30,  149)
anomaly_detect$AMY   <- mapply(  def_b,    AMY_pre,    AMY_post,  42,  132)

dimnames(anomaly_detect)[[1]] <- anomaly_detect[,2]
anomaly_detect[,-1]

dimnames(data)[[1]] <- data[,1]
