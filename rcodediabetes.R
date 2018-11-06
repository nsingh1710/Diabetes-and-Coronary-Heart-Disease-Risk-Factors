> View(`diabetes.(1)`)
> Diabetes <- na.omit(`diabetes.(1)`)
> Diabetes$Diabetes_Positive <- ifelse(Diabetes$glyhb>7,"Yes","No")
> Diabetes$location <- NULL
> Diabetes$gender <- NULL
> Diabetes$frame <- NULL
> install.packages("MASS")
> library(MASS)
> Diabetes$time.ppn<- NULL
> qda.Diabetes <- qda(Diabetes$Diabetes_Positive~Diabetes$chol+Diabetes$stab.glu+Diabetes$hdl+Diabetes$ratio+Diabetes$age+Diabetes$height+Diabetes$weight+Diabetes$bp.1s+Diabetes$bp.1d+Diabetes$bp.2s+Diabetes$bp.2d+Diabetes$waist+Diabetes$hip+(Diabetes$waist / Diabetes$hip))
> qbar.no <- sum(qda.Diabetes$scaling*qda.Diabetes$means[1,])
> qbar.yes <- sum(qda.Diabetes$scaling*qda.Diabetes$means[2,])
> Qda_Cutoff <- (qbar.no+qbar.yes)/2
> Qda_Cutoff
[1] 91.72856
> z <- predict(qda.Diabetes,Diabetes[-5])
> table(Diabetes$Diabetes_Positive,z$class)
     
       No Yes
  No  102   3
  Yes   6  25
> lda.Diabetes <- lda(Diabetes$Diabetes_Positive~Diabetes$chol+Diabetes$stab.glu+Diabetes$hdl+Diabetes$ratio+Diabetes$age+Diabetes$height+Diabetes$weight+Diabetes$bp.1s+Diabetes$bp.1d+Diabetes$bp.2s+Diabetes$bp.2d+Diabetes$waist+Diabetes$hip+(Diabetes$waist / Diabetes$hip))
> lbar.no <- sum(lda.Diabetes$scaling*lda.Diabetes$means[1,])
> lbar.yes <- sum(lda.Diabetes$scaling*lda.Diabetes$means[2,])
> lda_Cutoff <- (lbar.no+lbar.yes)/2
> lda_Cutoff
[1] -2.796499
> z <- predict(lda.Diabetes,Diabetes[-5])
> table(Diabetes$Diabetes_Positive,z$class)
     
       No Yes
  No  104   1
  Yes  11  20
