# Plot results for acoustic features for strict quality (labelled as good),
# relaxed quality (labelled as good or medium), and automatic quality (labelled 
# as good by SNR system) and comparison with the acoustic features of the
# ManyBabies recordings.
# Author: María Andrea Cruz Blandón
# Date: 09.12.2022

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(reshape2)
library(extrafont)

options(dplyr.summarise.inform = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


preprocess_data <- function(path_acoustic_feats){
  df = read.csv(path_acoustic_feats)
  df <- df[complete.cases(df),]
  
  data_strict <- df  %>% filter(quality==1)
  data_relaxed <- df %>% filter(quality==5 | quality==1)
  data_snr <- df %>% filter(quality_snr==1)
  data_all <- df
  
  df_mb <- read.csv('../results/mb_acoustic_features.csv')
  df_mb <- df_mb[complete.cases(df_mb),]
  
  data_strict['dataset'] = 'Strict'
  data_relaxed['dataset'] = 'Relaxed'
  data_snr['dataset']= 'SNR'
  df_mb['dataset']='ManyBabies'
  
  data_strict <- data_strict %>% select(-quality, -quality_snr)
  data_relaxed <- data_relaxed %>% select(-quality, -quality_snr)
  data_snr <- data_snr %>% select(-quality, -quality_snr)
  
  final_df <- rbind(data_strict, data_relaxed, data_snr, df_mb)
  return(final_df)
}


format_data <- function(path_acoustic_feats){
  final_df <- preprocess_data(path_acoustic_feats)
  final_df <- melt(final_df, id=c('filename', 'type', 'dataset'))
  final_df$dataset = factor(final_df$dataset, 
                            levels=c('Strict', 'Relaxed', 'SNR', 'ManyBabies'))
  return(final_df)
}


get_statistics <- function(data_df, metric){
  data_df = data_df[complete.cases(data_df),]
  result <- data_df %>% 
    filter(variable==metric) %>%
    group_by(type, dataset) %>%
    summarise(mean=mean(value),
              std=sd(value),
              count=n())
  t_test <- data_df %>%
    filter(variable==metric) %>%
    group_by(dataset) %>%
    summarise(t=t.test(value~type, data=cur_data())$statistic,
              p_value=t.test(value~type, data=cur_data())$p.value)
  
  tmp <- melt(result, id=c('dataset', 'type'))
  
  final <- pivot_wider(tmp, names_from = c(type, variable),
                       names_sep = '_', values_from = value)
  final <- final %>% 
    mutate(d=abs(IDS_mean - ADS_mean) / sqrt((IDS_std^2 + ADS_std^2)/2)
    )
  final = merge(t_test, final, by='dataset')
  return(final)
}

plot_both_distributions <- function(data){
  data <- data %>% group_by(corpus, dataset, type) %>% summarise(count=n())
  
  ggplot(data, aes(x=dataset, y=count, fill=type)) + 
    geom_col(width = 0.5, colour='black', position='dodge') + 
    labs(
      #title='Distribution of Utterances', 
         y='Total segments', x='Subset Quality') + 
    facet_grid(corpus ~ ., scales='free_y') + 
    theme(legend.position = c(0.15, 0.85),
          axis.line = element_line(color = 'black', size = 1), 
          plot.title = element_text(hjust = 0.5), 
          legend.title = element_blank(),
          legend.text = element_text(family = "LM Roman 10", size = 16),
          text = element_text(family = "LM Roman 10", size = 20))
}


data_ber <- format_data('../results/bergelson_acoustic_features.csv')
data_lyon <- format_data('../results/lyon_acoustic_features.csv')
data_ber <- data_ber %>% select(-filename)
data_lyon <- data_lyon %>% select(-filename)

data_ber1 <- data_ber %>% mutate(corpus='SEEDLings')
data_lyon1 <- data_lyon %>% mutate(corpus='Lyon')
all_data <- rbind(data_ber1, data_lyon1)
all_data <- all_data %>% filter(variable=='duration', dataset !='ManyBabies')

# Plot distribution of IDS/ADS per corpus and quality 
plot_both_distributions(all_data)
ggsave('../plots/distributions.pdf', width = 6, height = 5, units = 'in')

# Get the statistics for the different acoustic metrics per quality
get_statistics(data_ber, 'mean_pitch')
get_statistics(data_lyon, 'mean_pitch')

get_statistics(data_ber, 'std_pitch')
get_statistics(data_lyon, 'std_pitch')

get_statistics(data_ber, 'mean_tilt')
get_statistics(data_lyon, 'mean_tilt')

get_statistics(data_ber, 'duration')
get_statistics(data_lyon, 'duration')


