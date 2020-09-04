# Gompertz Analysis

#Gompertz model: y = a e^(-b e^(-c t))
#First derivative: dy/dt = a b c e^(b (-e^(-c t)) - c t)
#Second derivative: d2y/dt2 = -a b c^2 e^(-b e^(-c t) - 2 c t) (-b + e^(c t))

N0 <- 1000
Ninf <- 100000

a <- Ninf # Asymptote
b <- log(Ninf/N0)     # Displacement in time
c <- .2    # Growth rate
e <- exp(1)
t <- 0:150
y <- a * e^(-b * e^(-c * t))
rate <- a * b * c * e^(b * (-e^(-c * t)) - c * t)
accel <- -a * b * c^2 * e^(b * (-e^(-c * t)) - 2 * c * t) * (e^(c * t) - b)
DATA <- data.frame(
  Deaths = y,
  Daily = rate,
  Accel = accel,
  Time = t
)
ggplot(DATA, aes(x = Time, y = Deaths)) + 
  geom_line() +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = c("1", "10","100","1,000","10,000","100,000")
    ) +
  scale_x_continuous(breaks = 0:5*30)

ggplot(DATA, aes(x = Time, y = Daily)) + 
  geom_line() +
  scale_x_continuous(breaks = 0:5*30)

ggplot(DATA, aes(x = Time, y = Accel)) + 
  geom_line() +
  scale_x_continuous(breaks = 0:5*30)

DATA$Super <- 0
for (i in 1:150)
{
  DATA$Super[i] <- sum(DATA$Accel[1:i])  
}

ggplot(DATA, aes(x = Time, y = Super)) + 
  geom_line() +
  scale_x_continuous(breaks = 0:5*30)
