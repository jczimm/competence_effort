library('tidyverse')
library('patchwork')
library('Rmisc')
source('plt.R')

## Experiment 1: Predicting strength, effort, probability of success
# load behavioral data
dat <- read.csv('./../Data/index1.csv', header = T, stringsAsFactors = T)
dat$agent <- factor(dat$agent, labels = c('A', 'B'))
dat$round <- as.factor(dat$round)
dat$scenario <- factor(dat$scenario, levels = c('F,F;F,F','F,F;F,L','F,L;F,L','F,F;L,L','F,L;L,L','L,L;L,L'), ordered = T)
dat$prob[dat$round==3 & dat$agent=='B'] <- NaN # so that the probabilities are only counted once for joint lifting
dat <- arrange(dat,subject,scenario,round,agent)

# load simulation results
inference <- read.csv('./simulation/inference.csv', header = T, stringsAsFactors = T)
inference$round <- as.factor(inference$round)
inference$model <- factor(inference$model, levels = c('joint','solitary','compensatory','maximum'), ordered = T)
inference$scenario <- factor(inference$scenario, levels = c('F,F;F,F','F,F;F,L','F,L;F,L','F,F;L,L','F,L;L,L','L,L;L,L'), ordered = T)
inference$effort[inference$outcome==0] <- NaN
inference$prob[inference$round==3 & inference$agent=='B'] <- NaN

# Figure 3a: Model-predicted strength vs. human judgments
pdf('./fig3a.pdf', onefile=T, width = 6, height = 8)

dat$model <- 'data'

fig3a <- plt(dat, inference, 'round', 'strength', pt_size = 1) +
  coord_cartesian(ylim = c(1, 10)) +
  facet_grid(scenario ~ agent)

fig3a + labs(x = 'Round', y='Strength')

dev.off()

pdf('./fig3b.pdf', onefile=T, width = 6, height = 5)

inference_subset <- inference %>% filter(scenario != 'F,F;F,F' & scenario != 'F,F;F,L' & scenario != 'F,L;F,L')
dat_subset <- dat %>% filter(scenario != 'F,F;F,F' & scenario != 'F,F;F,L' & scenario != 'F,L;F,L')

fig3b <- plt(dat_subset, inference_subset, 'round', 'effort') +
  coord_cartesian(ylim = c(0, 100)) +
  facet_grid(scenario ~ agent)

fig3b + labs(x = 'Round', y='Effort (%)')

dev.off()

pdf('./fig3c.pdf', onefile=T, width = 6, height = 3)

inference_subset <- inference %>% filter(round == 3 & agent == 'A')
dat_subset <- dat %>% filter(round == 3 & agent == 'A')

fig3c <- plt(dat_subset, inference_subset, 'scenario', 'prob') +
  coord_cartesian(ylim = c(0, 100)) 

fig3c + labs(x='Round 1 and Round 2 outcome', y='Lift probability in Round 3 (%)', color=NULL)

dev.off()

# pearson correlation
dat_summary <- dat %>%
  group_by(scenario,round,agent) %>%
  dplyr::summarize(avg_effort = mean(effort), 
                   uci_effort = CI(effort)[1], 
                   lci_effort = CI(effort)[3],
                   avg_strength = mean(strength), 
                   uci_strength = CI(strength)[1], 
                   lci_strength = CI(strength)[3],
                   avg_prob = mean(prob), 
                   uci_prob = CI(prob)[1], 
                   lci_prob = CI(prob)[3])
inference <- merge(inference, dat_summary, sort = F)

prob_cor <- NULL
for (i in inference$model %>% unique()) {
  prob_cor <- c(prob_cor, cor(inference$avg_prob[inference$model==i], inference$prob[inference$model==i], method = 'pearson', use = 'complete.obs'))
}
prob_cor <- paste0('r = ',round(prob_cor,2))
prob.text.labels <- data.frame(model = inference$model %>% unique(),
                               correlation = prob_cor,
                               x = rep(75,4),
                               y = rep(10,4))

effort_cor <- NULL
for (i in inference$model %>% unique()) {
  effort_cor <- c(effort_cor, cor(inference$avg_effort[inference$model==i], inference$effort[inference$model==i], method = 'pearson', use = 'complete.obs'))
}
effort_cor <- paste0('r = ',round(effort_cor,2))
effort.text.labels <- data.frame(model = inference$model %>% unique(),
                                 correlation = effort_cor,
                                 x = rep(75,4),
                                 y = rep(10,4))

strength_cor <- NULL
for (i in inference$model %>% unique()) {
  strength_cor <- c(strength_cor, cor(inference$avg_strength[inference$model==i], inference$strength[inference$model==i], method = 'pearson', use = 'complete.obs'))
}
strength_cor <- paste0('r = ',round(strength_cor,2))
strength.text.labels <- data.frame(model = inference$model %>% unique(),
                                   correlation = strength_cor,
                                   x = rep(7.5,4),
                                   y = rep(2,4))

pdf('./fig4.pdf', onefile=T, width = 8, height = 8)

fig4L <- cor_plt(inference, 'prob', 'avg_prob', 'lci_prob', 'uci_prob', prob.text.labels) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  coord_fixed()

fig4M <- cor_plt(inference, 'effort', 'avg_effort', 'lci_effort', 'uci_effort', effort.text.labels) +
  scale_x_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100)) +
  scale_y_continuous(limits = c(0,100), breaks = c(0,25,50,75,100)) +
  coord_fixed()

fig4R <- cor_plt(inference, 'strength', 'avg_strength', 'lci_strength', 'uci_strength', strength.text.labels) +
  scale_x_continuous(limits = c(1, 10), breaks = c(1,2.5,5,7.5,10)) +
  scale_y_continuous(limits = c(1,10), breaks = c(1,2.5,5,7.5,10)) +
  coord_fixed()

fig4L + labs(x = NULL, y='Data', title='Probability (%)') + 
  theme(legend.position='none') +
  fig4M + labs(x = 'Model', y=NULL, title='Effort (%)') + 
  theme(legend.position='none') +
  fig4R + labs(x = NULL, y=NULL, title='Strength')
  
dev.off()


## Experiment 2: Incentive selection
# load data
dat2 <- read.csv('./../Data/index2.csv', header = T, stringsAsFactors = T)
dat2$strong_strength <- factor(dat2$strong_strength, levels = c(6.0, 6.5, 7.5, 8.0, 9.5), ordered = T)
dat2$weak_strength <- factor(dat2$weak_strength, levels = c(5.0, 5.5, 6.0, 6.5, 7.5, 8.0, 9.5), ordered = T)

# load simulation
df <- read.csv('./simulation/incentive.csv', header = T, stringsAsFactors = T)
df$strong_strength <- factor(df$strong_strength, levels = c(6.0, 6.5, 7.5, 8.0, 9.5), ordered = T)
df$weak_strength <- factor(df$weak_strength, levels = c(5.0, 5.5, 6.0, 6.5, 7.5, 8.0, 9.5), ordered = T)
df$model <- factor(df$model, levels = c('joint','solitary','compensatory','maximum'), ordered = T)

pdf('./fig6a.pdf', onefile=T, width = 10, height = 4)

dat2$model <- 'data'

fig6a <- plt(dat2, df, 'weak_strength', 'incentive') +
  coord_cartesian(ylim = c(0, 115)) +
  facet_grid(~strong_strength, scales = 'free')

fig6a + labs(x='Weak contestant\'s strength', y='Incentive', color=NULL)

dev.off()

# pearson correlation
dat_summary <- dat2 %>%
  group_by(strong_strength, weak_strength) %>%
  dplyr::summarize(avg_incentive = mean(incentive), 
                   uci_incentive = CI(incentive)[1], 
                   lci_incentive = CI(incentive)[3])
df2 <- merge(df, dat_summary, sort = F)

incentive_cor <- NULL
for (i in df2$model %>% unique()) {
  incentive_cor <- c(incentive_cor, cor(df2$avg_incentive[df2$model==i], df2$incentive[df2$model==i], method = 'pearson', use = 'complete.obs'))
}
incentive_cor <- paste0('r = ',round(incentive_cor,2))
incentive.text.labels <- data.frame(model = df2$model %>% unique(),
                                    correlation = incentive_cor,
                                    x = rep(75,4),
                                    y =rep(10,4))

pdf('./fig6b.pdf', onefile=T, width = 10, height = 3)

fig6b <- cor_plt2(df2, 'incentive', 'avg_incentive', 'lci_incentive', 'uci_incentive')

fig6b + labs(x = 'Model', y='Data', title='Incentive')

dev.off()


## Experiment 3: Team selection
# load data
dat3 <- read.csv('./../Data/index3.csv', header = T, stringsAsFactors = T)
num_subject <- dat3 %>% select(subject) %>% unique %>% dim()
num_subject <- num_subject[1]
dat3$subject <- as.factor(dat3$subject)
dat3$weight <- as.factor(dat3$weight)
counts <- table(dat3$contestant[dat3$select==1], dat3$weight[dat3$select==1])
p <- counts/num_subject
ci <- sqrt(p*(1-p)/num_subject)*1.96
m <- data.frame(p) %>% rename(contestant = Var1, weight = Var2)

# load simulation
t <- read.csv('./simulation/team.csv', header = T, stringsAsFactors = T)
t$weight <- as.factor(t$weight)
t$model <- factor(t$model, levels = c('joint','solitary','compensatory','maximum'), ordered = T)

pdf('./fig7b.pdf', onefile=T, width = 9, height = 4)

m$model <- 'data'

fig7b <- plt2(m, t, 'contestant', 'Freq') +
  geom_errorbar(data = m, aes(ymin=p-ci, ymax=p+ci), width=.1,
                position=position_dodge(.9)) +
  scale_y_continuous(limits = c(0,1)) +
  facet_grid(~weight)

fig7b + labs(x='Contestant', y='Proportion')

dev.off()

pdf('./fig7c.pdf', onefile=T, width = 7, height = 4)

dat3$model <- 'data'
dat3_subset <- dat3 %>% filter(contestant == 'A')
t_subset <- t %>% filter(contestant == 'A')

fig7c <- plt(dat3_subset, t_subset, 'weight', 'total') +
  coord_cartesian(ylim = c(0, 50))

fig7c + labs(x='Box weight', y='Total incentive allocated')

dev.off()

pdf('./fig7d.pdf', onefile=T, width = 9, height = 4)

fig7d <- plt(dat3, t, 'weight', 'incentive') +
  coord_cartesian(ylim = c(0, 50)) +
  facet_grid(~contestant)

fig7d + labs(x='Box weight', y='Incentive')

dev.off()

