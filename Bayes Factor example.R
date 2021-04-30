# Bayes Factor presentation using George Margetis experiment 1 preliminary data
# Daniel Nettle April 2021

# Libraries
library(tidyverse)
# Make the graphs look tolerable
theme_set(theme_classic())
library(psych)

# For the Bayes Factors
library(BayesFactor)
# A helper package for getting a nice summary of parameter estimates
library(parameters)

# Get data
d=read.csv("data.long.csv", stringsAsFactors = TRUE)
# d contains repeated measures (each person does three puzzles)
# For a simpler example we restrict to just one puzzle per participant
d1=subset(d, Puzzle==1)

#### A simple t-test##### 
# Examine the 'power' variable by condition (this was just a manipulation check)
ggplot(d1, aes(x=Condition, y=Power))  + 
  geom_violin() + 
  stat_summary(fun.data="mean_cl_boot", colour="red")
# Looks like there could be a modest difference

# Traditional t-test
t.test(Power~Condition, data=d1)

# Bayes factor version t-test
# BF for the alternative
ttestBF(formula=Power~Condition, data=d1)
# BF for the null
1/ttestBF(formula=Power~Condition, data=d1)
# What if we want the 95% credible interval for the difference?
# This function comes from the 'parameters' package
model_parameters(ttestBF(formula=Power~Condition, data=d1), ci=0.95)
# This gives lots of useful stats, as well as the CI
# The % in ROPE is particularly useful 
# (% of the posterior density that is as close to zero as makes no difference)

# Also note a t-test is just a special case of a linear model
# We can see this using the classical approach
summary(lm(Power~Condition, data=d1))

# lmBF() is the equivalent function to lm() in classical world
lmBF(Power~Condition, data=d1)
# Get the model parameters this way
model_parameters(lmBF(Power~Condition, data=d1), ci=0.95)

##### Multiple predictive factors: ANOVA #####
# Say we want to consider both condition and gender
# Classical approach
anova(lm(Power~Condition*Gender, data=d1))
# Only condition is significant, not gender

# BayesFactor equivalent is AnovaBF() function
# Note that this requires all the predictor variables to be of class 'factor'
anovaBF(Power~Condition*Gender, data=d1)
# What this gives us is the BFs against the null of all possible sub-hypotheses
# You can get a handy plot of this
plot(anovaBF(Power~Condition*Gender, data=d1))

# But you might want to be testing Gender + Condition against Condition only
# Rather than each against the null
# Where two BFs share the same denominator, they can simply be divided
# In the AnovaBF model, you can divide one row by another
my.anova = anovaBF(Power~Condition*Gender, data=d1)
my.anova['Gender + Condition']/my.anova['Condition']
# Or the other way around
my.anova['Condition']/my.anova['Gender + Condition']
# Note that there is a natural penalty against the more complex model unless it gives extra explanatory power

###A linear mixed model and a more complex null #####
# Here each participant is responding to three puzzles
# So we need a random effect of participant
# And also which puzzle it is should be in the null
# Our experimental hypothesis:
mixed1 = lmBF(Confidence ~ Condition + Puzzle + participant.code, 
          whichRandom = "participant.code", 
          data=d)
# BF of this model against the null
mixed1
# This is very large. But intercept only is not the relevant null

# The relevant null should still contain which puzzle and participant it is
# So we set up the following
mixed1reduced = lmBF(Confidence ~ Puzzle + participant.code, 
          whichRandom = "participant.code", 
          data=d)
mixed1reduced

# Now the BF for our critical test
mixed1/mixed1reduced
# So BF favouring the null (not very strongly). 
# Express it as the BF in favour of the null instead
mixed1reduced/mixed1
# So the evidence is tending to the null

###Examining the evolution of the BF with sample size#####
# Show how BF evolves with more data for the simple t-test example
# This chunk calculates the BF sequentially as the data come in
ns=NULL
bfs=NULL
counter=1
for (i in 4:length(d1$Power)) {
  current.d=d1[1:i, ]
  ns[counter]=i
  bfs[counter]=extractBF(ttestBF(formula=Power~Condition, data=current.d))$bf
  counter=counter+1
}

# Now plot the evolution of the BF
f=ggplot(data.frame(ns, bfs), aes(x=ns, y=bfs)) + 
  geom_point() + 
  geom_line() + 
  ylab("Bayes Factor") + 
  xlab("Sample size") + 
  geom_hline(yintercept=1, linetype="dotted") +
  geom_hline(yintercept=3, linetype="dashed") +
  geom_hline(yintercept=1/3, linetype="dashed") + 
  scale_y_continuous(trans='log10')
f

# Repeat the same exercise but for something that is null
ns=NULL
bfs=NULL
counter=1
for (i in 5:length(d1$Gender)) {
  current.d=d1[1:i, ]
  ns[counter]=i
  bfs[counter]=extractBF(ttestBF(formula=Power~Gender, data=current.d))$bf
  counter=counter+1
}

# Now plot the evolution of the BF
f2=ggplot(data.frame(ns, bfs), aes(x=ns, y=bfs)) + 
  geom_point() + 
  geom_line() + 
  ylab("Bayes Factor") + 
  xlab("Sample size") + 
  geom_hline(yintercept=1, linetype="dotted") +
  geom_hline(yintercept=3, linetype="dashed") +
  geom_hline(yintercept=1/3, linetype="dashed") + 
  scale_y_continuous(trans='log10')
f2
