---
title: "Irene Hsueh's BS 728 Homework 3"
author: "Irene Hsueh"
date: "11/23/2021"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### LQAS Functions
```{r}
#Decision Rule
decision_rule <- function(upper, lower, alpha=0.10, beta=0.10, n=seq(from=1,to=100,by=1))
  {for(i in 1:length(n)) 
    {d <- seq(from=0, to=n[i], by=1)
    temp_alpha <- pbinom(d-1, size=n[i], prob=upper) 
    temp_beta <- 1-pbinom(d-1, size=n[i], prob=lower) 
    test <- intersect(which(temp_alpha <= alpha), which(temp_beta <= beta))
    
    if(length(test)>0) break}
  d <- d[test]
  n <- n[i]
  return(list(n=n, d=d, alpha=temp_alpha[test], beta=temp_beta[test]))
}


#Operating Characteristic Curve
operating_curve <- function(n, d, alpha, beta) 
{probability <- seq(from=0, to=1, by=0.01)
  p_high <- 1-pbinom(d-1, n, prob=probability)
  plot(probability, p_high, type="l", 
       main = "Operating Characteristic Curve", 
       ylab = "Probability of Being Classified as Passing Threshold",
       xlab = "Population Prevalence")
}


#Risk Curve
risk_curve <- function(n, d, target_prob) 
{probability <- seq(from=0, to=1, by=0.01)
  risk1 <- 1-pbinom(d-1, n, prob=probability[probability < target_prob])
  risk2 <- pbinom(d-1, n, prob=probability[probability >= target_prob])
  risks <- c(risk1, risk2)
  plot(probability[probability < target_prob], risk1, xlim=c(0,1), type="l", 
       main = "Risk Curve", 
       ylab = "Probability of Misclassification",
       xlab = "Population Prevalence")
  lines(probability[probability >= target_prob], risk2)
}
```




```{r}
#Method 1
method1 <- decision_rule(upper=0.6,lower=0.3, alpha=0.1, beta=0.1)
operating_curve(n=method1$n, d=method1$d, alpha=0.1, beta=0.1)


#Method 2
method2 <- decision_rule(upper=0.6,lower=0.4, alpha=0.1, beta=0.1)
operating_curve(n=method2$n, d=method2$d, alpha=0.1, beta=0.1)


#Method 3
method3 <- decision_rule(upper=0.8,lower=0.6, alpha=0.1, beta=0.1)
operating_curve(n=method3$n, d=method3$d, alpha=0.1, beta=0.1)


#Method 4
method4 <- decision_rule(upper=0.8,lower=0.6, alpha=0.1, beta=0.05)
operating_curve(n=method4$n, d=method4$d, alpha=0.1, beta=0.1)
```




```{r}
exam_dataset <- decision_rule(upper=0.5,lower=0.2, alpha=0.1, beta=0.1)

hypothetical1 <- decision_rule(upper=0.7,lower=0.4, alpha=0.1, beta=0.1)
hypothetical2 <- decision_rule(upper=0.7,lower=0.2, alpha=0.1, beta=0.1)
```











