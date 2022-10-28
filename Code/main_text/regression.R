library('tidyverse')
library('lmerTest')

## Experiment 1
dat <- read.csv('./../../Data/exp1.csv', header = T, stringsAsFactors = T)
dat$agent <- factor(dat$agent, labels = c('A', 'B'))
dat$round <- as.factor(dat$round)
dat$scenario <- factor(dat$scenario, levels = c('F,F;F,F','F,F;F,L','F,L;F,L','F,F;L,L','F,L;L,L','L,L;L,L'), ordered = T)
dat$prob[dat$round==3 & dat$agent=='B'] <- NaN # so that the probabilities are only counted once for joint lifting
dat <- arrange(dat,subject,scenario,round,agent)

# Effort
dat$subject <- as.factor(dat$subject)
fit_lmm1 <- lmer(effort ~ round + agent + (1|subject) + (round+0|subject) + (agent+0|subject),
                 data = dat %>% filter(round != 1 & scenario != 'F,F;F,F' & scenario != 'F,F;F,L' & scenario != 'F,L;F,L'))
summary(fit_lmm1)

# Lift probability
t.test(dat$prob[dat$round == 3 & dat$scenario == 'F,F;F,F' & dat$agent == 'A'], mu = 0, alternative = 'two.sided')
# filter one agent so that the probability is only counted once. Round 3 is a joint lift.

## Experiment 2
dat2 <- read.csv('./../../Data/exp2.csv', header = T, stringsAsFactors = T)
dat2$subject <- as.factor(dat2$subject)
fit_lmm2 <- lmer(incentive ~ weak_strength + strong_strength + (1|subject) + (weak_strength+0|subject) + (strong_strength+0|subject), data = dat2)
summary(fit_lmm2)

## Experiment 3
dat3 <- read.csv('./../../Data/exp3.csv', header = T, stringsAsFactors = T)
dat3$subject <- as.factor(dat3$subject)
dat3$select <- as.factor(dat3$select)

# incentive change with weight
fit_lmm3 <- lmer(total ~ weight + (1|subject) + (weight+0|subject), data = dat3 %>% filter(contestant == 'A'))
# filter one agent so that the total incentive is only counted once
summary(fit_lmm3)

fit_lmm3a <- lmer(incentive ~ weight + (1|subject) + (weight+0|subject), data = dat3 %>% filter(contestant == 'A'))
summary(fit_lmm3a)

fit_lmm3b <- lmer(incentive ~ weight + (1|subject) + (weight+0|subject), data = dat3 %>% filter(contestant == 'B'))
summary(fit_lmm3b)

fit_lmm3c <- lmer(incentive ~ weight + (1|subject) + (weight+0|subject), data = dat3 %>% filter(contestant == 'C'))
summary(fit_lmm3c)

# incentive change with whether the contestant is selected
fit_lmm <- lmer(incentive ~ select + (1|subject) + (select+0|subject), data = dat3 %>% filter(contestant == 'A'))
summary(fit_lmm)

fit_lmm <- lmer(incentive ~ select + (1|subject) + (select+0|subject), data = dat3 %>% filter(contestant == 'B'))
summary(fit_lmm)

fit_lmm <- lmer(incentive ~ select + (1|subject) + (select+0|subject), data = dat3 %>% filter(contestant == 'C'))
summary(fit_lmm)
