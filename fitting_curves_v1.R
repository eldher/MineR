


# linear model analysis

r <- c(8272,
  10071,
  13088,
  13576,
  11717,
  9959.45,
  6539.5725,
  4255.767175,
  3400
)
r
plot(r)
tab <- data.frame(x=seq_along(r), y=r)

# res <- lm(y ~ sin((0.41*x)^2),data = tab)
# summary(res)
# plot(y~x,data=tab)
# lines(tab$x,res$fitte,col=2)
# fit <- glm(y~x,data=tab,family=gaussian())
# summary(fit)




# Emend cap -----------------

fit <- nls(y ~ C1*exp(-(x-mean1)**2/(2 * sigma1**2))+2000, data=tab, start=list(C1=10000, mean1=3.8, sigma1=1))
summary(fit)


min = 10
max = 15

newdata = data.frame(x = c(min:max))
d <- predict(fit, newdata, type="response")
predicted_values <- data.frame(x=c(min:max),y = d)

tab <- rbind(tab,predicted_values)

plot(tab$x,tab$y)
# Emend amp -----------------


r <- c(2377,
       3728,
       6076.64,
       8279.264,
       9892.6064,
       8700)
r
plot(r)
tab <- data.frame(x=seq_along(r), y=r)

fit <- nls(y ~ C1*exp(-(x-mean1)**2/(2 * sigma1**2))+2000, data=tab, start=list(C1=10000, mean1=5, sigma1=1))
summary(fit)

min = 7
max = 12

newdata = data.frame(x = c(min:max))
d <- predict(fit, newdata, type="response")
predicted_values <- data.frame(x=c(min:max),y = d)

tab <- rbind(tab,predicted_values)


plot(tab$x,tab$y)

#This adds to the already created plot a line
# once again, first argument is x values, second is y values
#lines(tab$x,predict(fit))

# Onicit ------------------------------

r <- c(4448,
       4397,
       4555,
       6014,
       7996,
       10599.29605,
       12159.95527,
       13807.54632,
       11800)
r
plot(r)
tab <- data.frame(x=seq_along(r), y=r)


fit <- nls(y ~ C1*exp(-(x-mean1)**2/(2 * sigma1**2))+4440, data=tab, start=list(C1=10000, mean1=8, sigma1=1))
summary(fit)

min = 10
max = 15

newdata = data.frame(x = c(min:max))
d <- predict(fit, newdata, type="response")
predicted_values <- data.frame(x=c(min:max),y = d)

tab <- rbind(tab,predicted_values)
View(d)

plot(tab$x,tab$y)

#This adds to the already created plot a line
# once again, first argument is x values, second is y values
lines(tab$x,predict(fit))

# Akynzeo

r <- c(2796,
       6422,
       13564)
r
plot(r)
tab <- data.frame(x=seq_along(r), y=r)


fit <- nls(y ~ C1*exp(-(x-mean1)**2/(2 * sigma1**2))+1000, data=tab, start=list(C1=10000, mean1=4, sigma1=1))
summary(fit)

min = 10
max = 15

newdata = data.frame(x = c(min:max))
d <- predict(fit, newdata, type="response")
predicted_values <- data.frame(x=c(min:max),y = d)

tab <- rbind(tab,predicted_values)
View(d)

plot(tab$x,tab$y)

#This adds to the already created plot a line
# once again, first argument is x values, second is y values
lines(tab$x,predict(fit))




