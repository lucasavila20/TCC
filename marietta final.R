# Martin Marietta

library(tidyverse)
library(hett)
library(conflicted)
conflict_prefer("select", "dplyr")
data(mm)

martin <- mm

## SN1 

library(tidyverse); library(gamlss)

gamm.mariettaSN1 <- gamlss(m.marietta ~ CRSP,
                           sigma.formula = ~ CRSP,
                           nu.formula = ~ CRSP,
                           family = SN1,
                           data = martin,
                           control = gamlss.control(n.cyc = 100, 
                                                    trace = FALSE),
                           trace = FALSE)


summary(gamm.mariettaSN1)


gamm.mariettaSN1 <- gamlss(m.marietta ~ 1,
                           sigma.formula = ~ CRSP,
                           nu.formula = ~ CRSP,
                           family = SN1,
                           data = martin,
                           control = gamlss.control(n.cyc = 100, 
                                                    trace = FALSE),
                           trace = FALSE)


summary(gamm.mariettaSN1)

## ST2

gamm.mariettaST2 <- gamlss(m.marietta ~ CRSP,
                            sigma.formula = ~ CRSP,
                            nu.formula = ~ CRSP,
                            tau.formula = ~ CRSP,
                            family = ST2,
                            data = martin,
                            control = gamlss.control(n.cyc = 100, 
                                                     trace = FALSE),
                            trace = FALSE)
summary(gamm.mariettaST2)


gamm.mariettaST2 <- gamlss(m.marietta ~ CRSP,
                           sigma.formula = ~ 1,
                           nu.formula = ~ CRSP,
                           tau.formula = ~ CRSP,
                           family = ST2,
                           data = martin,
                           control = gamlss.control(n.cyc = 100, 
                                                    trace = FALSE),
                           trace = FALSE)
summary(gamm.mariettaST2)



gamm.mariettaST2 <- gamlss(m.marietta ~ CRSP,
                           sigma.formula = ~ 1,
                           nu.formula = ~ CRSP,
                           tau.formula = ~ 1,
                           family = ST2,
                           data = martin,
                           control = gamlss.control(n.cyc = 100, 
                                                    trace = FALSE),
                           trace = FALSE)
summary(gamm.mariettaST2)

moments::skewness(lei$leite)

## NO

gamm.mariettaNO <- gamlss(m.marietta ~ CRSP,
                           sigma.formula = ~ CRSP,
                           nu.formula = ~ CRSP,
                           tau.formula = ~ CRSP,
                           family = NO,
                           data = martin,
                           control = gamlss.control(n.cyc = 100, 
                                                    trace = FALSE),
                           trace = FALSE)
summary(gamm.mariettaNO)


## TF

gamm.mariettaTF <- gamlss(m.marietta ~ CRSP,
                          sigma.formula = ~ CRSP,
                          nu.formula = ~ CRSP,
                          tau.formula = ~ CRSP,
                          family = TF,
                          data = martin,
                          control = gamlss.control(n.cyc = 100, 
                                                   trace = FALSE),
                          trace = FALSE)
summary(gamm.mariettaTF)


gamm.mariettaTF <- gamlss(m.marietta ~ CRSP,
                          sigma.formula = ~ 1,
                          nu.formula = ~ CRSP,
                          tau.formula = ~ CRSP,
                          family = TF,
                          data = martin,
                          control = gamlss.control(n.cyc = 100, 
                                                   trace = FALSE),
                          trace = FALSE)
summary(gamm.mariettaTF)


gamm.mariettaTF <- gamlss(m.marietta ~ CRSP,
                          sigma.formula = ~ 1,
                          nu.formula = ~ 1,
                          tau.formula = ~ CRSP,
                          family = TF,
                          data = martin,
                          control = gamlss.control(n.cyc = 100, 
                                                   trace = FALSE),
                          trace = FALSE)
summary(gamm.mariettaTF)


gamlss::GAIC(gamm.mariettaSN1, gamm.mariettaST2,
             gamm.mariettaNO, gamm.mariettaTF,
             k = 2, c = FALSE) %>% kable()

#SN1

gamlss::GAIC(gamm.mariettaSN1, gamm.mariettaST2,
             gamm.mariettaNO, gamm.mariettaTF,
             k = log(nrow(martin)), c = FALSE)  

#SN1

summary(gamm.mariettaSN1)

plot(y = residuals(gamm.mariettaSN1),
     x = rep(-0.05617106,60))


plot(y = residuals(gamm.mariettaSN1),
     x = gamm.mariettaSN1$sigma.fv)

plot(y = residuals(gamm.mariettaSN1),
     x = gamm.mariettaSN1$nu.fv)

plot(gamm.mariettaSN1)

plotSimpleGamlss(m.marietta, CRSP,
                 data = martin,
                 model = gamm.mariettaSN1,
                 x.val = seq(-.05,0.1176,
                             length.out = 5),
                 xlim=c(-.1,.12),
                 val = .003,
                 cols = 'white',
                 csize = 1)


lines(y = SN1()$mean(mu = gamm.mariettaSN1$mu.fv, 
                     sigma = gamm.mariettaSN1$sigma.fv,
                     nu = gamm.mariettaSN1$nu.fv)
      [order(martin$CRSP)],
      x = martin$CRSP[order(martin$CRSP)], col = "red", lwd = 2)



editada <- edit(plotSimpleGamlss)
body(plotSimpleGamlss) <- body(editada)


martin %>% 
ggplot2::ggplot(aes(x = m.marietta)) +
geom_histogram(col = "black", fill = "blue", bins = 15) +
xlab("Martin") +
ylab("Frequência")


martin %>% 
  ggplot2::ggplot(aes(y = m.marietta)) +
  geom_boxplot(col = "black", fill = "blue") +
  ylab("Martin") +
  theme(axis.text.x = element_blank(),  # Remove os textos do eixo x
        axis.ticks.x = element_blank())  # Remove os ticks do eixo x


curve(dSN1(x, 0, sigma = 1, 1), from = -10, to = 10)
curve(dSN1(x, 0, sigma = 2, 1), add = TRUE, col = "red")
curve(dSN1(x, 0, sigma = 3, 1), add = TRUE, col = "blue")
curve(dSN1(x, 5, sigma = 1, 1), add = TRUE, col = "green")




