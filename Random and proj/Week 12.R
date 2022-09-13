ggplot(data = xy, aes(x = x, y = y)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

summary(lm(y ~ x))

p_base <- ggplot(data = data.frame(resid <- residuals(fit), fitted = fitted(fit)
                ))
p1 <- p_base + 
  geom_qq(aes(sample = resid)) + 
  geom_qq_line(aes(sample = resid))

>p2 <- p_base + 
  geom_point(aes(x=fitted, y = resid)) +
  geom_hline(aes(yintercept = 0), colour = "red") +
  xlab("Fitted values")+ 
  ylab("Residuals") + 
  ggtitle("Residuals vs Fitted")

p1+p2
