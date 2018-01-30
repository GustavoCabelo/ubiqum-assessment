require(pacman)
pacman::p_load(readr, magrittr, caret,DMwR, tidyverse) 


#################### Business Question #################
# The goal of this assessment is to predict the dependent variable placed in the column #5 (Y)


#################### Load #######################
# Set Directory
setwd("D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Assessment")

# Load CSV
    df <- read.csv(paste0(getwd(),"/assesment_Gustavo.csv"), stringsAsFactors = F)

  # Add "Index" column
    df <- tibble::rowid_to_column(df, "Index")

################### Data Exploration ##################
    View(df)
    
    str(df)

      # data.frame':	10000 obs. of  5 variables:
      #  $ X1        : int  5 -5 2 -4 0 1 1 0 -3 4 ...
      # $ X2        : int  3 1 -5 2 -5 -2 -2 5 -3 0 ...
      # $ X3        : int  2 0 2 -2 1 -2 4 -3 0 0 ...
      # $ X4        : int  1 3 3 2 2 2 2 1 1 2 ...
      # $ Y: int  11 -1 -2 -4 -2 2 -3 -15 -3 2 ...

    summary(df)

      # X1                 X2                 X3                X4            Y      
      # Min.   : -5.0000   Min.   :-5.00000   Min.   :-3.0000   Min.   :-50.000   Min.   :-24.0000  
      # 1st Qu.: -3.0000   1st Qu.:-3.00000   1st Qu.:-2.0000   1st Qu.:  1.000   1st Qu.: -2.0000  
      # Median :  0.0000   Median : 0.00000   Median : 0.0000   Median :  2.000   Median :  0.0000  
      # Mean   :  0.3059   Mean   : 0.01192   Mean   : 0.4715   Mean   :  1.912   Mean   : -0.0277  
      # 3rd Qu.:  3.0000   3rd Qu.: 3.00000   3rd Qu.: 2.0000   3rd Qu.:  3.000   3rd Qu.:  2.0000  
      # Max.   :318.0000   Max.   : 5.00000   Max.   : 4.0000   Max.   :  3.000   Max.   : 25.0000  
      # NA's   :108        NA's   :100        NA's   :99        NA's   :121                 

  # Explore NAs
    {
    NAsx1 <- which(is.na(df$X1))
    NAsx2 <- which(is.na(df$X2))
    NAsx3 <- which(is.na(df$X3))
    NAsx4 <- which(is.na(df$X4))
    NasIndex <- unique(c(NAsx1,NAsx2,NAsx3,NAsx4))
    rm(list = "NAsx1","NAsx2","NAsx3","NAsx4")
    }

    NAs <- df[NasIndex,] #<--- 424 unique obs. (4,2%)
    df <- na.omit(df)

# View Attributes distribution

    table(df$X1)
    table(df$X2)
    table(df$X3)
    table(df$X4)
    table(df$Y)
    
    # $X1
    # -5  -4  -3  -2  -1   0   1   2   3   4   5 308 310 311 315 316 317 318 
    # 908 956 866 898 863 899 915 876 892 876 933   2   1   1   1   3   1   1 
    
      ## 1st Qu.: -3.0000
      ## 3rd Qu.:  3.0000

    # $X2
    # -5  -4  -3  -2  -1   0   1   2   3   4   5 
    # 858 898 948 895 889 888 913 916 909 879 907 

      ## 1st Qu.:-3.00000
      ## 3rd Qu.: 3.00000 
    
    # $X3
    # -3   -2   -1    0    1    2    3    4 
    # 1245 1273 1204 1308 1219 1239 1196 1217 
    
      ## 1st Qu.:-2.0000
      ## 3rd Qu.: 2.0000

    # $X4
    # -50    1    2    3 
    # 17 3271 3302 3289 
    
      ## 1st Qu.:  1.000 
      ## 3rd Qu.:  3.000 
    
    
    # $Y
    # -24  -23  -21  -20  -19  -18  -17  -16  -15  -14  -13  -12  -11  -10   -9   -8   -7   -6   -5   -4   -3   -2 
    # 4    4    9    9    8   15   26   21   37   41   44   50   62   73   93  131  188  244  375  426  560  823 
    
    # -1    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
    # 950 1674 1001  760  596  403  346  201  179  130   95   89   61   51   51   35   26   29   25   16   13   10 
    
    # 21   22   23   25 
    # 3    6    1    6
    
# Histogram
 
    hist(df$X1, breaks = 200, xlim = c(-10,318), main = '')
    hist(df$X2, breaks = 200, xlim = c(-10,10), main = '')
    hist(df$X3, breaks = 200, xlim = c(-10,10), main = '')
    hist(df$X4, breaks = 200, xlim = c(-50,10), main = '')
    hist(df$Y, breaks = 200, xlim = c(-30,30), main = '')
    
# PLOT DF
    plotdf <- plot(df)
  # Exploring "$Y" distribution below "1st Qu = -2" and above "3rd Qu = 2"
    
    Y <- df[ which( df$Y > 2 | df$Y < -2) , ]

      # Findings
        # $x2 ~ Y: Seems to have the stronger correlation, as the dependent variable variation is highly affected by $x2 values 
        # $x3 ~ Y: the second highest correlation
        # $x1 ~ Y: the third highest correlation

    # Understading $x4 role in "Y"
    
    x4 <- df[ which(df$X4 == -50),]
    
      # Findings  
        # $x4 ~ .: when $x4 = -50, seems to neutralize the weigth from other variables   

# Remove Outliers
    
    df <- df %>% 
      filter(df$X4 !=-50) %>% 
      filter(df$X1 <=5) 
    
#################################### CORRELATION MATRIX ###############################
    
    corrplot <- cor(df) #<--- calculating correlation (excluded NAs)
 
    #             X1           X2           X3           X4           Y
    # X1  1.000000000  0.009918285 -0.001000009 -0.002284554 0.358780078
    # X2  0.009918285  1.000000000 -0.006440958 -0.013693972 0.162990497
    # X3 -0.001000009 -0.006440958  1.000000000  0.008716574 0.012091572
    # X4 -0.002284554 -0.013693972  0.008716574  1.000000000 0.003424017
    # Y   0.358780078  0.162990497  0.012091572  0.003424017 1.000000000
    
    
    
############################ Modeling ##############################
####================ Regression Problem =======================####
#------------->  Algorithm Selection: -------------------------------------------------
  # For this regression problem, I've chosen KNN, SVM, GBM, RF and LM to model.
    # No pre-processing
    # Resampling: Cross-Validated (3 fold, repeated 1 times) 
    
#-------------> Create Training/Testing Partitions -------------------------------------------------
    {
    set.seed(123)
      inTrain <- createDataPartition(df$Y, p = .33, list = F)
      training <- df[inTrain,]
      testing <- df[-inTrain,]
    }

#---------------------------> Training -------------------------------------------------
    # Set TrainControl validation 
    CrossValidation <- trainControl(method = "repeatedcv", number = 3)
    
    #KNN
      {
      set.seed(123)
      a <- proc.time()      #<----- staring time stamp
        KNNFit <- train(Y ~ ., 
                        data = training,
                        method = "knn",
                        trControl = CrossValidation)
        
      KNN_timelapse <- proc.time() - a
      KNN_timelapse
      # elapsed 1.72 

    # Check metrics
      KNNFit
      
      # k  RMSE      Rsquared   MAE      
      # 5  1.208709  0.9556950  0.7450895
      # 7  1.275244  0.9532795  0.7789867
      # 9  1.354994  0.9487287  0.8134834
    }
    
    #GBM
      {
      set.seed(123)
      a <- proc.time()      #<----- staring time stamp
      GBMFit <- train(Y ~ ., 
                      data = training,
                      method = "gbm",
                      trControl = CrossValidation)
      
      GBM_timelapse <- proc.time() - a
      GBM_timelapse
      # elapsed 2.14 
      
      # Check metrics
      GBMFit
      
      # interaction.depth  n.trees  RMSE      Rsquared   MAE 
      # 3                   50      3.124013  0.7396923  1.8483526
      # 3                  100      1.731125  0.9184478  1.1294847
      # 3                  150      1.212541  0.9553087  0.8745497
      
      
      }
    
    #RF
      {
      set.seed(123)
      a <- proc.time()      #<----- staring time stamp
      RFFit <- train(Y ~ ., 
                     data = training,
                     method = "rf",
                     trControl = CrossValidation,
                     ntree = 50)
      
      RF_timelapse <- proc.time() - a
      RF_timelapse
      # elapsed 5.12
      
      # Check metrics
      RFFit
      
      # mtry  RMSE       Rsquared   MAE      
      # 2     1.2866878  0.9678517  0.7121974
      # 3     0.8176097  0.9829629  0.4675501
      # 4     0.6706925  0.9870298  0.4108465
      }
    
    #SVM
      {
      set.seed(123)
      a <- proc.time()      #<----- staring time stamp
      SVMFit <- train(Y ~ ., 
                      data = df,
                      method = "svmLinear2",
                      trControl = CrossValidation)
      
      SVM_timelapse <- proc.time() - a
      SVM_timelapse
      # elapsed 27.17 
      
      # Check metrics
      SVMFit
      
      # cost  RMSE      Rsquared   MAE     
      # 0.25  4.843430  0.1530427  3.288448
      # 0.50  4.843544  0.1530519  3.288419
      # 1.00  4.843482  0.1530323  3.288474
      
      
      # RMSE was used to select the optimal model using the smallest value.
      # The final value used for the model was cost = 0.25.
      }
    
    #LM
    {
      set.seed(123)
      a <- proc.time()      #<----- staring time stamp
      LMFit <- train(Y ~ ., 
                     data = training,
                     method = "lm",
                     trControl = CrossValidation)
      
      LM_timelapse <- proc.time() - a
      LM_timelapse
      # elapsed 0.64
      
      # Check metrics
      LMFit
      
      # RMSE      Rsquared   MAE     
      # 4.939869  0.1544126  3.405934
    }
    
#---------------------------> Testing -------------------------------------------------    
    
    #KNN
    {
      KNNPred <- predict(KNNFit, testing)
      postResample(KNNPred, testing$Y)
    
      # RMSE  Rsquared       MAE 
      # 0.9299043 0.9696381 0.6034920 
      
    }
    
    #SVM prediction
    {
      SVMPred <- predict(SVMFit, testing)
      postResample(SVMPred, testing$Y)
      
      #       RMSE   Rsquared        MAE 
      # 5.54396257 0.02775308 3.36538795 
      
      #       RMSE  Rsquared       MAE 
      # 4.7840767 0.1538328 3.2531569
    }
    
    #GBM prediction
    {
      GBMPred <- predict(GBMFit, testing)
      postResample(GBMPred, testing$Y)
      
      #     RMSE  Rsquared       MAE 
      # 1.1511451 0.9558092 0.8310627 
    }
    
    #RF prediction
    {
      RFPred <- predict(RFFit, testing)
      postResample(RFPred, testing$Y)
      
      # RMSE  Rsquared       MAE 
      # 0.4928726 0.9915952 0.3097492 
    }
    
    #LM prediction
    {
      LMPred <- predict(LMFit, testing)
      postResample(LMPred, testing$Y)
      
      # RMSE  Rsquared       MAE 
      # 4.7618325 0.1539219 3.2857970 
    }
    
#---------------------------> Plotting -------------------------------------------------    
    
  # Build DF with predictions ####
    testing$YKNN <- KNNPred
    testing$YSVM <- SVMPred
    testing$YGBM <- GBMPred
    testing$YRF <- RFPred
    testing$YLM <- LMPred
    
  # Plots ####
    
    #KNN
    {
      KNNPlot <- ggplot(testing) +
        geom_line(aes(x = testing$Y, y = testing$Y), size = 1.5) +
        geom_point(aes(x = testing$YKNN, y = testing$Y), color = "red", alpha = 0.3, size = 1) +
        labs(title = "KNN FIT") +
        theme(legend.position = "none",
              axis.title = element_blank())
      KNNPlot
    }
    
    # SVM
    {
      SVMPlot <- ggplot(testing) +
        geom_line(aes(x = testing$Y, y = testing$Y), size = 1.5) +
        geom_point(aes(x = testing$YSVM, y = testing$Y), color = "blue", alpha = 0.3, size = 1) +
        labs(title = "SVM FIT") +
        theme(legend.position = "none",
              axis.title = element_blank())
      SVMPlot
    }
    
    # GBM
    {
      GBMPlot <- ggplot(testing) +
        geom_line(aes(x = testing$Y, y = testing$Y), size = 1.5) +
        geom_point(aes(x = testing$YGBM, y = testing$Y), color = "purple", alpha = 0.3, size = 1) +
        labs(title = "GBM FIT") +
        theme(legend.position = "none",
              axis.title = element_blank())
      GBMPlot
    }
    
    # RF
    {
      RFPlot <- ggplot(testing) +
        geom_line(aes(x = testing$Y, y = testing$Y), size = 1.5) +
        geom_point(aes(x = testing$YRF, y = testing$Y), color = "green", alpha = 0.3, size = 1) +
        labs(title = "RF FIT") +
        theme(legend.position = "none",
              axis.title = element_blank())
      RFPlot
    }
    
    #LM
    {
      LMPlot <- ggplot(testing) +
        geom_line(aes(x = testing$Y, y = testing$Y), size = 1.5) +
        geom_point(aes(x = testing$YLM, y = testing$Y), color = "dark green", alpha = 0.3, size = 1) +
        labs(title = "LM FIT") +
        theme(legend.position = "none",
              axis.title = element_blank())
      LMPlot
    }
    
    ############################ Conclusion ##############################
    
    # Being a regression problem, these algorithms were chosen.
    # The Linear models, couldn´t predict well as the independent variable values had their values concentrated in a range too centered (-5 to 5) and couldn't read well the tail.
    # As the problem had multivariables, Trees and clusterization algorithms helped to predict better the "Y".
    # Forests are intrinsically suited for multiclass problems, while SVM is intrinsically two-class. 
    # KNN also worked well as it build clusters and reffer to the nearest neighbor to  predict the value. 