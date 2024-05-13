##########  Comparing Group Means    ##############
######### by Andrew Piper #########################
######### CC By 4.0 License #######################
library(effsize)

#Compare the mean / median value of a variable of interest
#For tips on how to understand testing means of independent samples:
#http://statistics-help-for-students.com/How_do_I_report_independent_samples_T_test_data_in_APA_style.htm#.Ye7Doi_72v5
#https://statistics.laerd.com/spss-tutorials/independent-t-test-using-spss-statistics.php

#In order to compare the means of two samples those samples need to be NORMALLY distributed

### Step 1: Visualize the distribution of your feature using a histogram
#Here we will add a curve of the normal distribution so you can see how well your histogram fits the curve

#Dummy example using artificial data
hist(rnorm(100, mean = 5, sd = 3), prob=T)
curve(dnorm(x, mean=5, sd=3), add=TRUE)

#now load your own data
df<-NULL

#Same thing using your data
hist(feature1, prob=T, main="Histogram of Feature in Group 1")
curve(dnorm(x, mean=mean(feature1), sd=sd(feature1)), add=TRUE)
hist(feature2, prob=T, main="Histogram of Feature in Group 2")
curve(dnorm(x, mean=mean(feature2), sd=sd(feature2)), add=TRUE)

#### Step 2: Use a statistical test for normality
#Here we use the "Shapiro Test" to see if we can assume our data is normally distributed
# when p < 0.05 it means your data is NOT normally distributed
shapiro.test(feature1) 
shapiro.test(feature2)

#### Step 3: IF YES, there is a normal distribution, then run a Welch's two sample t-test
#Here we can get the value for the Mean of Corpus A and Mean of Corpus B for our Feature
#We can also get a t-statistic to tell us how strong this difference is
#And we can get a p-value
t.test(feature1, feature2)

#when you report your results you report the means of both samples along with the standard deviation
mean(feature1)
mean(feature2)
sd(feature1)
sd(feature2)

#### EXAMPLE OF WRITING YOUR RESULTS #####
#"An independent-samples t-test was conducted to compare the rate of determiners
#in detective fiction by Sherlock Holmes and literary short fiction. 
#There was a significantly higher rate of determiners in detective fiction (M=0.077, SD=0.0069) 
#than literary fiction (M=0.068, SD=0.014); t(69.7)=4.15, p = 9.315e-05."

#to observe how similar your distributions are, plot the two distributions on the same graph
#this uses a density plot
#the dark line = Corpus A
#the dotted line = Corpus B
plot(density(feature1), main = "Feature Distributions for\nCorpus A (black line) and Corpus B (dotted line)")
lines(density(feature2), lty=2)
#abline(v=mean(feature1))
#abline(v=mean(feature2), lty=2)

#you can also use a boxplot
#the dark line = median
boxplot(feature1, feature2)

#finally you can estimate what is known as the "effect size", a way of estimating just how strong this difference is
#this is like asking what percent of Group A observations are below/above the mean of Group B
#rule of thumb .2 or less = small, .5 or less = medium, .8 and above = large
#see https://www.simplypsychology.org/effect-size.html
cohen.d(feature1, feature2)

#### Step 4: IF NO, your data is NOT normally distributed, then run a Wilcoxon Rank Sum Test
wilcox.test(feature1, feature2)
#report the medians rather than the means
median(feature1)
median(feature2)
#report how much higher/lower median1 is to median2 as a ratio
median(feature1)/median(feature2)
#report how much higher/lower median1 is to median2 as a raw difference
median(feature1) - median(feature2)
#report how much this difference translates into per page counts
#we use 500 words as an idealized page size
(median(feature1) - median(feature2))*500

####   EXAMPLE OF WRITING YOUR RESULTS: #####
#"According to a Wilcoxon rank-sum test with continuity correction, 
#detective fiction uses significantly more determiners than short fiction 
#in our sample of short stories (p = 2.463e-05). 
#We found that the median value for detective fiction (0.07684468) 
#was 19% higher than the median value for short fiction (0.06463036) 
#resulting in an overall increase of 0.0122 or roughly 6 determiners per page."
