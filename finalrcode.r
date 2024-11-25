> # Student Spending Analysis Script
> # This script answers three statistical questions using t-tests, regression, and ANOVA.
> # Ensure you have the 'ggplot2' package installed: install.packages("ggplot2")
> 
> # Load required library
> library(ggplot2)
> 
> # Load dataset
> # Modify the file path if needed
> data <- read.csv("/Users/shriatluri/Desktop/student_spending.csv") 
> 
> # Check dataset structure
> str(data)
'data.frame':	1000 obs. of  18 variables:
 $ X                       : int  0 1 2 3 4 5 6 7 8 9 ...
 $ age                     : int  19 24 24 23 20 25 23 23 22 18 ...
 $ gender                  : chr  "Non-binary" "Female" "Non-binary" "Female" ...
 $ year_in_school          : chr  "Freshman" "Junior" "Junior" "Senior" ...
 $ major                   : chr  "Psychology" "Economics" "Economics" "Computer Science" ...
 $ monthly_income          : int  958 1006 734 617 810 523 1354 631 1402 1423 ...
 $ financial_aid           : int  270 875 928 265 522 790 69 748 248 74 ...
 $ tuition                 : int  5939 4908 3051 4935 3887 3151 4973 3966 5638 3977 ...
 $ housing                 : int  709 557 666 652 825 413 812 571 599 626 ...
 $ food                    : int  296 365 220 289 372 386 398 269 354 249 ...
 $ transportation          : int  123 85 137 114 168 122 101 92 82 117 ...
 $ books_supplies          : int  188 252 99 223 194 131 213 251 155 123 ...
 $ entertainment           : int  41 74 130 99 48 73 21 37 123 51 ...
 $ personal_care           : int  78 92 23 30 71 38 38 90 41 74 ...
 $ technology              : int  134 226 239 163 88 234 157 152 162 243 ...
 $ health_wellness         : int  127 129 112 105 71 108 117 56 172 34 ...
 $ miscellaneous           : int  72 68 133 55 104 99 48 62 194 196 ...
 $ preferred_payment_method: chr  "Credit/Debit Card" "Credit/Debit Card" "Cash" "Mobile Payment App" ...
> 
> # ---- Question 1: Does the average monthly income differ significantly between males and females? ----
> # Subset data for male and female incomes
> male_income <- data$monthly_income[data$gender == 'Male']
> female_income <- data$monthly_income[data$gender == 'Female']
> 
> # Perform two-sample t-test
> t_test_result <- t.test(male_income, female_income, var.equal = TRUE)
> print(t_test_result)  # Display test results

	Two Sample t-test

data:  male_income and female_income
t = -0.71307, df = 677, p-value = 0.476
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -60.31829  28.17880
sample estimates:
mean of x mean of y 
 1008.258  1024.328 

> 
> # Visualize monthly income by gender
> ggplot(data, aes(x = gender, y = monthly_income, fill = gender)) +
+   geom_boxplot() +
+   labs(
+     title = "Monthly Income by Gender",
+     x = "Gender",
+     y = "Monthly Income (in dollars)"
+   ) +
+   theme_minimal()
> 
> # ---- Question 2: Is there a relationship between monthly income and entertainment expenses? ----
> # Fit a simple linear regression model
> model <- lm(entertainment ~ monthly_income, data = data)
> summary(model)  # Display regression results

Call:
lm(formula = entertainment ~ monthly_income, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-66.076 -30.991   1.377  31.128  66.699 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)    81.588746   4.342994  18.786   <2e-16 ***
monthly_income  0.003160   0.004089   0.773     0.44    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 37.98 on 998 degrees of freedom
Multiple R-squared:  0.000598,	Adjusted R-squared:  -0.0004034 
F-statistic: 0.5972 on 1 and 998 DF,  p-value: 0.4398

> 
> # Visualize the relationship with regression line
> ggplot(data, aes(x = monthly_income, y = entertainment)) +
+   geom_point(color = "blue", alpha = 0.6) +
+   geom_smooth(method = "lm", color = "red", se = TRUE) +
+   labs(
+     title = "Relationship between Monthly Income and Entertainment Expenses",
+     x = "Monthly Income (in dollars)",
+     y = "Entertainment Expenses (in dollars)"
+   ) +
+   theme_minimal()
`geom_smooth()` using formula = 'y ~ x'
> 
> # ---- Question 3: Do students in different years of study spend significantly different amounts on housing? ----
> # Perform one-way ANOVA
> anova_result <- aov(housing ~ year_in_school, data = data)
> summary(anova_result)  # Display ANOVA results
                Df   Sum Sq Mean Sq F value Pr(>F)  
year_in_school   3   300031  100010   3.436 0.0165 *
Residuals      996 28986468   29103                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> # Visualize housing expenses by year of study
> ggplot(data, aes(x = year_in_school, y = housing, fill = year_in_school)) +
+   geom_boxplot() +
+   labs(
+     title = "Housing Expenses by Year of Study",
+     x = "Year of Study",
+     y = "Housing Expenses (in dollars)"
+   ) +
+   theme_minimal()
> 
