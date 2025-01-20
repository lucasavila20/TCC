abdom_SN1 <- 
gamlss(y ~ ., 
sigma.formula = ~., 
nu.formula = ~., 
tau.formula = ~.,
data = abdom,
family = SN1,
control = gamlss.control(n.cyc = 500)
)

summary(abdom_SN1)



abdom_ST2 <- 
gamlss(y ~ ., 
sigma.formula = ~., 
nu.formula = ~., 
tau.formula = ~.,
data = abdom,
family = ST2,
control = gamlss.control(n.cyc = 500)
)

summary(abdom_ST2)


abdom_ST2 <- 
  gamlss(y ~ ., 
         sigma.formula = ~., 
         nu.formula = ~., 
         tau.formula = ~ 1,
         data = abdom,
         family = ST2,
         control = gamlss.control(n.cyc = 500)
  )

summary(abdom_ST2)


abdom_NO <- 
gamlss(y ~ ., 
sigma.formula = ~., 
nu.formula = ~., 
tau.formula = ~.,
data = abdom,
family = NO,
control = gamlss.control(n.cyc = 500)
)

summary(abdom_NO)


abdom_TF <- 
gamlss(y ~ ., 
sigma.formula = ~., 
nu.formula = ~., 
tau.formula = ~.,
data = abdom,
family = TF,
control = gamlss.control(n.cyc = 500)
)

summary(abdom_TF)


abdom_TF <- 
  gamlss(y ~ ., 
         sigma.formula = ~., 
         nu.formula = ~ 1, 
         tau.formula = ~.,
         data = abdom,
         family = TF,
         control = gamlss.control(n.cyc = 500)
  )

summary(abdom_TF)

#por aic
gamlss::GAIC(abdom_SN1, abdom_ST2,
abdom_NO, abdom_TF,
k=2, c = FALSE
)

#ST2

#por bic
gamlss::GAIC(abdom_SN1, abdom_ST2,
abdom_NO, abdom_TF,
k=log(nrow(abdom)), c = FALSE
)


#SN1

plotSimpleGamlss(y, x,
                 data = abdom,
                 model = abdom_ST2,
                 x.val = seq(min(abdom$x),max(abdom$x),
                             length.out = 5),
                 xlim=c(min(abdom$x),max(abdom$x)),
                 cols = 'white',
                 csize = 1,
                 family = ST2)

lines(y = ST2()$mean(mu = abdom_ST2$mu.fv, 
                     sigma = abdom_ST2$sigma.fv,
                     nu = abdom_ST2$nu.fv,
                     tau = abdom_ST2$tau.fv)
      [order(martin$CRSP)],
      x = martin$CRSP[order(martin$CRSP)], col = "red", lwd = 2)

plotar = 0
for(i in 1:nrow(abdom)){
  plotar[i] = ST2()$mean(
             mu = abdom_ST2$mu.fv[i], 
             sigma = abdom_ST2$sigma.fv[i],
             nu = abdom_ST2$nu.fv[i],
             tau = abdom_ST2$tau.fv[i])
}

lines(y = plotar[order(abdom$x)],
      x = abdom$x[order(abdom$x)], col = "red", lwd = 2)

# Gráfico das densidades estimadas

plotSimpleGamlss(y, x,
                 data = abdom,
                 model = abdom_SN1,
                 x.val = seq(min(abdom$x),max(abdom$x),
                             length.out = 5),
                 xlim=c(min(abdom$x),max(abdom$x)),
                 cols = 'white',
                 csize = 1,
                 family = SN1)

plotar = 0
for(i in 1:nrow(abdom)){
  plotar[i] = SN1()$mean(mu = abdom_SN1$mu.fv[i], 
                         sigma = abdom_SN1$sigma.fv[i],
                         nu = abdom_SN1$nu.fv[i])
}

lines(y = plotar[order(abdom$x)],
      x = abdom$x[order(abdom$x)], col = "red", lwd = 2)


plot(abdom_SN1)
plot(abdom_ST2)

wp(abdom_SN1)
wp(abdom_ST2, ylim.all = 1.5)

#ST2 é melhor

       
summary(abdom_SN1)
summary(abdom_ST2)

plot(abdom_SN1)
plot(abdom_ST2)

abdom %>% 
  ggplot2::ggplot(aes(x = y)) +
  geom_histogram(col = "black", fill = "blue", bins = 15) +
  xlab("y") +
  ylab("Frequência")
