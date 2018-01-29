library(pacman)
pacman::p_load(readr, magrittr, caret) 


#################### Business Question #################
# The goal of this assessment is to predict the dependent variable placed in the column #5 (predictive)


#################### Load #######################
# Set Directory
setwd("D:/Profissional/Ubiqum/Data Analytics - Understanding customers/Programa 2017.02/Assessment")

# Load CSV
assessment_Gustavo <- read.csv(paste0(getwd(),"/assesment_Gustavo.csv"), stringsAsFactors = F)


################### Data Exploration ##################
    View(assessment_Gustavo)
    
    str(assessment_Gustavo)

# data.frame':	10000 obs. of  5 variables:
#  $ X1        : int  5 -5 2 -4 0 1 1 0 -3 4 ...
# $ X2        : int  3 1 -5 2 -5 -2 -2 5 -3 0 ...
# $ X3        : int  2 0 2 -2 1 -2 4 -3 0 0 ...
# $ X4        : int  1 3 3 2 2 2 2 1 1 2 ...
# $ predictive: int  11 -1 -2 -4 -2 2 -3 -15 -3 2 ...

    summary(assessment_Gustavo)

# X1                 X2                 X3                X4            predictive      
# Min.   : -5.0000   Min.   :-5.00000   Min.   :-3.0000   Min.   :-50.000   Min.   :-24.0000  
# 1st Qu.: -3.0000   1st Qu.:-3.00000   1st Qu.:-2.0000   1st Qu.:  1.000   1st Qu.: -2.0000  
# Median :  0.0000   Median : 0.00000   Median : 0.0000   Median :  2.000   Median :  0.0000  
# Mean   :  0.3059   Mean   : 0.01192   Mean   : 0.4715   Mean   :  1.912   Mean   : -0.0277  
# 3rd Qu.:  3.0000   3rd Qu.: 3.00000   3rd Qu.: 2.0000   3rd Qu.:  3.000   3rd Qu.:  2.0000  
# Max.   :318.0000   Max.   : 5.00000   Max.   : 4.0000   Max.   :  3.000   Max.   : 25.0000  
# NA's   :108        NA's   :100        NA's   :99        NA's   :121                 

  # Explore NAs
    # NAsx1 <- which(is.na(assessment_Gustavo$X1))
    # NAsx2 <- which(is.na(assessment_Gustavo$X2))
    # NAsx3 <- which(is.na(assessment_Gustavo$X3))
    # NAsx4 <- which(is.na(assessment_Gustavo$X4))
    
    NAs <- assessment_Gustavo[unique(c(NAsx1,NAsx2,NAsx3,NAsx4)),] #<--- 424 obs. (4,2%)
    noNAs <- assessment_Gustavo[-unique(c(NAsx1,NAsx2,NAsx3,NAsx4)),]

    
#View Columns

    table(assessment_Gustavo$X1)
    table(assessment_Gustavo$X2)
    table(assessment_Gustavo$X3)
    table(assessment_Gustavo$X4)
    table(assessment_Gustavo$predictive)
    
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
    
    
    # $predictive
    # -24  -23  -21  -20  -19  -18  -17  -16  -15  -14  -13  -12  -11  -10   -9   -8   -7   -6   -5   -4   -3   -2 
    # 4    4    9    9    8   15   26   21   37   41   44   50   62   73   93  131  188  244  375  426  560  823 
    
    # -1    0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
    # 950 1674 1001  760  596  403  346  201  179  130   95   89   61   51   51   35   26   29   25   16   13   10 
    
    # 21   22   23   25 
    # 3    6    1    6
    
# Histogram
 
    hist(assessment_Gustavo$X1, breaks = 200, xlim = c(-10,318), main = '')
    hist(assessment_Gustavo$X2, breaks = 200, xlim = c(-10,10), main = '')
    hist(assessment_Gustavo$X3, breaks = 200, xlim = c(-10,10), main = '')
    hist(assessment_Gustavo$X4, breaks = 200, xlim = c(-50,10), main = '')
    hist(assessment_Gustavo$predictive, breaks = 200, xlim = c(-30,30), main = '')
    
# Understading "predictive" distribution below "1st Qu" and above "3rd Qu"
    
    predictive <- assessment_Gustavo[ which( assessment_Gustavo$predictive > 2 | assessment_Gustavo$predictive < -2) , ]

    
    # $x2 ~ predictive: Seems to have the stronger correlation, as the dependent variable variation is highly affected by $x2 values 
    # $x3 ~ predictive: the second highest correlation
    # $x1 ~ predictive: the third highest correlation

    
# Understading $x4 role in "predictive"
    
    x4 <- assessment_Gustavo[ which(assessment_Gustavo$X4 == -50),]

    # $x4 ~ .: when $x4 = -50, seems to neutralize the weigth from other variables   
    
######## CORRELATION MATRIX ############
    
    corrplot <- cor(noNAs) #<--- calculating correlation (excluded NAs)
 
      #                     X1           X2           X3           X4   predictive
      # X1         1.000000000  0.011266120  0.007601582  0.001430316  0.115606694
      # X2         0.011266120  1.000000000 -0.006216659 -0.017421402  0.162655700
      # X3         0.007601582 -0.006216659  1.000000000  0.008949837  0.012684914
      # X4         0.001430316 -0.017421402  0.008949837  1.000000000 -0.002667038
      # predictive 0.115606694  0.162655700  0.012684914 -0.002667038  1.000000000
    
    
    
    model.lm <- lm(data=noNAs, predictive ~ X1 + X2 + X3 + X4)
    summary(model.lm)
    
        # Residuals:
        #   Min       1Q   Median       3Q      Max 
        # -23.7878  -2.2678   0.0081   2.2326  23.3092 
        # 
        # Coefficients:
        #               Estimate Std. Error t value Pr(>|t|)    
        # (Intercept) -0.0624111  0.0694132  -0.899    0.369    
        # X1           0.0588527  0.0051861  11.348   <2e-16 ***
        # X2           0.2683491  0.0166538  16.113   <2e-16 ***
        # X3           0.0293696  0.0229411   1.280    0.200    
        # X4          -0.0003031  0.0230539  -0.013    0.990    
        # ---
        #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
        # 
        # Residual standard error: 5.131 on 9571 degrees of freedom
        # Multiple R-squared:  0.03957,	Adjusted R-squared:  0.03917 
        # F-statistic: 98.58 on 4 and 9571 DF,  p-value: < 2.2e-16
    