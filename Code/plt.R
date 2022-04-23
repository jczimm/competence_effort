plt <- function(dat, simulation, x, y, pt_size = 1.5) {
  
  return (simulation %>%
            ggplot(aes_string(x, y, group = 'model', color = 'model')) +
            geom_line(data = simulation %>% filter(model=='maximum')) +
            geom_line(data = simulation %>% filter(model=='compensatory')) +
            geom_line(data = simulation %>% filter(model=='solitary')) +
            geom_line(data = simulation %>% filter(model=='joint')) +
            geom_point(data = simulation %>% filter(model=='maximum'), size = .8) +
            geom_point(data = simulation %>% filter(model=='compensatory'), size = .8) +
            geom_point(data = simulation %>% filter(model=='solitary'), size = .8) +
            geom_point(data = simulation %>% filter(model=='joint'), size = .8) +
            stat_summary(data = dat, fun.data = 'mean_cl_boot', geom = 'errorbar', width = .1) +
            stat_summary(data = dat, fun = 'mean', geom = 'point', size = pt_size) +
            scale_color_manual(name = NULL, labels = c('Data','Joint effort model','Solitary effort model','Compensatory effort model','Maximum effort model'),
                         values = c('#000000','#e35d5e','#1d3557','#457b9d','#9ecccc'),
                         limits = c('data','joint','solitary','compensatory','maximum')) +
            theme_bw()
  )
}

plt2 <- function(dat, simulation, x, y) {
  
  return (simulation %>%
            ggplot(aes_string(x, y, group = 'model', color = 'model')) +
            geom_line(data = simulation %>% filter(model=='maximum')) +
            geom_line(data = simulation %>% filter(model=='compensatory')) +
            geom_line(data = simulation %>% filter(model=='solitary')) +
            geom_line(data = simulation %>% filter(model=='joint')) +
            geom_point(data = simulation %>% filter(model=='maximum'), size = .8) +
            geom_point(data = simulation %>% filter(model=='compensatory'), size = .8) +
            geom_point(data = simulation %>% filter(model=='solitary'), size = .8) +
            geom_point(data = simulation %>% filter(model=='joint'), size = .8) +
            stat_summary(data = dat, fun = 'mean', geom = 'point') +
            scale_color_manual(name = NULL, labels = c('Data','Joint effort model','Solitary effort model','Compensatory effort model','Maximum effort model'),
                               values = c('#000000','#e35d5e','#1d3557','#457b9d','#9ecccc'),
                               limits = c('data','joint','solitary','compensatory','maximum')) +
            theme_bw()
  )
}

cor_plt <- function(data, x, y, ymin, ymax, text_data) {
  
  return (ggplot(data, aes_string(x, y, color = 'round')) +
            geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
            geom_point() +
            geom_abline(intercept = 0, slope = 1, lty = 3) +
            scale_color_manual(name = NULL, labels = c('Round 1','Round 2','Round 3'),
                               values = c('#ea4700','#0073c3','#f8b81c')) +
            facet_wrap(~model, nrow = 4, labeller = as_labeller(c(`joint`='Joint', `solitary`='Solitary', `compensatory`='Compensatory', `maximum`='Maximum'))) +
            theme_bw() + 
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_text(data = text_data,
                      mapping = aes(label = correlation, x = x , y = y),
                      inherit.aes = F,
                      fontface='italic')
  )
}

cor_plt2 <- function(data, x, y, ymin, ymax) {
  
  return (ggplot(data, aes_string(x, y)) +
            geom_errorbar(aes_string(ymin = ymin, ymax = ymax, width = 0)) +
            geom_point() +
            geom_abline(intercept = 0, slope = 1, lty = 3) +
            scale_x_continuous(limits = c(0, 100), breaks = c(0,20,40,60,80,100)) +
            scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
            coord_fixed() +
            facet_grid(~model, labeller = as_labeller(c(`joint`='Joint', `solitary`='Solitary', `compensatory`='Compensatory', `maximum`='Maximum'))) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            geom_text(data = incentive.text.labels,
                      mapping = aes(label = correlation, x = x , y = y),
                      inherit.aes = F,
                      fontface='italic') 
  )
}