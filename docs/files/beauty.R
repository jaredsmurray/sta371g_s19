# Professor Daniel Hamermesh from UT’s economics department has been studying the 
# impact of beauty on income. Dr. Hamermesh has demonstrated the effect of beauty on 
# income in a variety of different situations. Here’s an example: In the paper 
# “Beauty in the Classroom” they showed that “...instructors who are viewed as better 
# looking receive higher instructional ratings” leading to a direct impact in the 
# salaries in the long run. By now, you should know that this is a hard effect to 
# measure. Not only do we need a way to measure “beauty” objectively but we also 
# need to “adjust for many other determinants” (gender, lower division class, 
# native language, tenure track status, etc).
# 
# We have an extract of the data and a number of questions:
# 
# 1. Is there a relationship between beauty and course evaluations?
# 2. Is that relationship *different* for men and women?
# 3. Does the relationship persist after controlling for other variables?
# 4. What other factors are predictive of course evaluations?

# These are the variables we have:
# score       	average professor evaluation score: (1) very unsatisfactory - (5) excellent.
# rank        	rank of professor: teaching, tenure track, tenured.
# ethnicity   	ethnicity of professor: not minority, minority.
# gender	      gender of professor: female, male.
# language    	language of school where professor received education: english or non-english.
# age	          age of professor.
# cls_level	    class level: lower, upper.
# bty_avg	      average beauty rating of professor.

library(readr)
path = "https://jaredsmurray.github.io/sta371g_f17/data/"
beauty = read_csv(paste0(path, 'evals.csv'))

plot(score~bty_avg, data=beauty)
fit1 = lm(score~bty_avg, data=beauty)

fit2 = lm(score~bty_avg+gender, data=beauty)

fit3 = lm(score~bty_avg*gender, data=beauty)

fit4 = lm(score~bty_avg*gender + rank + ethnicity + language + age + cls_level, data=beauty)

fit5 = lm(score~bty_avg+gender + rank + ethnicity + language + age + cls_level, data=beauty)


