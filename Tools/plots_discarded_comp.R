# MISSING DATA

data_ <- data_[complete.cases(data_), ]

all_data <- matching_data_preprocess(year, month)
all_data[['discarded']] = FALSE 
all_data[which(!(all_data$assistance_no %in% data_$assistance_no)), 'discarded'] <- TRUE
all_data[['eligible']]=ifelse(all_data$eligible==1,TRUE,FALSE)
all_data[which(all_data$num_children>10), 'num_children'] <- 10
all_data[which(all_data$AG_1>10), 'num_children'] <- 10
all_data[which(all_data$AG_2>10), 'num_children'] <- 10
all_data[which(all_data$AG_3>10), 'num_children'] <- 10
all_data[which(all_data$AG_4>10), 'num_children'] <- 10
all_data[which(all_data$AG_5>10), 'num_children'] <- 10

dir.create(paste(outputs_matching,'discarded_plots', sep='/'))

p <- ggplot(data=all_data, aes(x=AC_1, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AC_1.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AC_6, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AC_6.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AG_1, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AG_1.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AG_2, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AG_2.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AG_3, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AG_3.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AG_4, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AG_4.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=AG_5, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/AG_5.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=eligible, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/eligible.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=months_since_application, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/months_since_application.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=months_since_application, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/months_since_application.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=nat_country, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/nat_country.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=num_children, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/num_children.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')

p <- ggplot(data=all_data, aes(x=Reg, fill=discarded)) + 
  scale_fill_manual('', values=c('grey','black'), labels=c('Kept', 'Discarded'))+
  geom_bar(position=position_dodge()) +theme_light()
ggsave(filename = paste(outputs_matching, '/discarded_plots/Reg.png', sep=''), p,
       width = 5, height = 5, dpi = 300, units = 'in', device='png')




