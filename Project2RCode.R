getwd()
P2 <- read.csv('FA20_P2_33301.csv', header=TRUE)
M_E <- lm(Y ~ E1+E2+E3+E4, data=P2)

View(P2)
summary(M_E)
summary(M_E)$adj.r.squared
##[1] 0.5079178
M_raw <- lm( Y ~ (E1+E2+E3+E4+G1+G2+G3+G4+G5+G6+G7+G8+G9+G10+G11+G12+G13+G14+G15+G16+G17+G18+G19+G20)^2, data=P2)
plot(resid(M_raw) ~ fitted(M_raw), main='Residual Plot')
boxcox(M_raw)
M_trans <- lm( I(sqrt(Y)) ~ (.)^2, data=P2)
summary(M_raw)$adj.r.square;
##[1] 0.5209232
summary(M_trans)$adj.r.square
##[1] 0.5661488
plot(resid(M_trans) ~ fitted(M_trans), main='New Residual Plot')

install.packages("leaps")
library(leaps)
M <- regsubsets( model.matrix(M_trans)[,-1], I(sqrt(P2$Y)),
             	nbest = 1 , nvmax=4,
             	method = 'forward', intercept = TRUE )
temp <- summary(M)
Var <- colnames(model.matrix(M_trans))
M_select <- apply(temp$which, 1,
              	function(x) paste0(Var[x], collapse='+'))
kable(data.frame(cbind( model = M_select, adjR2 = temp$adjr2, BIC = temp$bic)),
  	caption='Model Summary')
M_main <- lm( I(sqrt(Y)) ~ ., data=P2)
temp <- summary(M_main)
kable(temp$coefficients[ abs(temp$coefficients[,4]) <= 0.001, ], caption='Sig Coefficients')

M_2stage <- lm( I(sqrt(Y)) ~ (E4+G4)^2, data=P2)
temp <- summary(M_2stage)
temp$coefficients[ abs(temp$coefficients[,3]) >= 4, ]
citation()
Footer
© 2023 GitHub, Inc.
Footer navigation
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
