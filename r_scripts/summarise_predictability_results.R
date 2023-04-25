# Plot accuracy bloxplots for different arrangements of the data
# Author: María Andrea Cruz Blandón
# Date: 23.11.2022

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

options(dplyr.summarise.inform = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
get_data <- function(results_path, quality, quality_snr){
    df <- read_csv(results_path)
    if(quality_snr){
        df <- df %>% filter(quality_snr == 1)
    } else {
        if(quality=='strict'){
            df <- df %>% filter(quality == 1)
        } else if(quality=='relaxed'){
            df <- df %>% filter(quality == 1 | quality == 5)
        }
    }
    return(df)
}

# Plots

plot_acc_per_speech_style <- function(data, title, subtitle) {
  data <- data %>%
    mutate(model = as.factor(model))
  corpus.labs <- c('French: Lyon', 'English: SEEDLings', 'ManyBabies')
  names(corpus.labs) <- c('French', 'English', 'ManyBabies')
  lang.labs <- c('French model', 'English model')
  names(lang.labs) <- c('French', 'English')
  
  
  ggplot(data, aes(x = model, y = score, fill=type)) +
    geom_boxplot(
      outlier.color = "gray",
      outlier.shape = 16,
      outlier.size = 1,
      notch = FALSE
    ) +
    facet_grid(corpus ~ lang, labeller = labeller(corpus = corpus.labs,
                                                  lang = lang.labs)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Model training data (h)',
      y = 'Accuracy'
    ) +
    stat_compare_means(method = 't.test',
                       label = 'p.signif',
                       label.y = 0.05) +
    theme(
      legend.position = c(0.08, 0.93),
      legend.title = element_blank(),
      text = element_text(family = "LM Roman 10", size = 20),
      axis.line = element_line(color = 'black', size = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
}

plot_acc_per_corpus <- function(data, title, subtitle) {
  data <- data %>%
    mutate(model = as.factor(model))
  lang.labs <- c('French model', 'English model')
  names(lang.labs) <- c('French', 'English')
  
  ggplot(data, aes(x = model, y = score, fill = corpus)) +
    scale_fill_brewer(palette = "PRGn") +
    geom_boxplot(
      outlier.colour = 'gray',
      outlier.shape = 16,
      outlier.size = 1,
      notch = FALSE
    ) +
    facet_grid(lang ~ .,
               labeller = labeller(lang = lang.labs)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Model training data (h)',
      y = 'Accuracy',
      fill = 'Test corpus'
    ) +
    stat_compare_means(method = 't.test',
                       label = "p.signif",
                       label.y = 0.05) +
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "LM Roman 10", size = 16),
      text = element_text(family = "LM Roman 10", size = 18),
      axis.line = element_line(color = 'black', size = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
}

plot_acc_per_model <- function(data, title, subtitle) {
  data <- data %>%
    mutate(model = as.factor(model))
  
  corpus.labs <- c('French: Lyon', 'English: SEEDLings')
  names(corpus.labs) <- c('French', 'English')
  
  ggplot(data, aes(x = model, y = score, fill = lang)) +
    scale_fill_brewer(palette = "BuPu") +
    geom_boxplot(
      outlier.colour = 'gray',
      outlier.shape = 16,
      outlier.size = 2,
      notch = FALSE
    ) +
    facet_grid(corpus ~ .,
               labeller = labeller(corpus = corpus.labs)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = 'Model training data (h)',
      y = 'Accuracy',
      fill = 'Model'
    ) +
    stat_compare_means(method = 't.test',
                       label = "p.signif",
                       label.y = 0.05) +
    
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "LM Roman 10", size = 16),
      text = element_text(family = "LM Roman 10", size = 18),
      axis.line = element_line(color = 'black', size = 1),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
}

plot_acc_counterbalanced_scenario <- function(data, title, subtitle, grouping=FALSE){
  data <- data %>% 
    mutate(native=ifelse(lang==corpus, 'Native', 'Non-native'),
           model=as.factor(model))
  
  p <- ggplot(data, aes(x=model, y=score, fill=native)) + 
    scale_fill_brewer(palette="Reds") +
    geom_boxplot(outlier.colour = 'gray', outlier.shape = 16, 
                 outlier.size = 2, notch = FALSE) + 
    labs(title=title, subtitle=subtitle,
         x='Model training data (h)', y='Accuracy', fill='Nativeness') + 
    stat_compare_means(method = 't.test', label="p.signif", label.y = 0.05) + 
    theme(legend.position="bottom", 
          legend.text=element_text(family="LM Roman 10", size=16),
          text = element_text(family="LM Roman 10", size=18), 
          axis.line = element_line(color='black', size=1), 
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 12))
  
  if(grouping){
    p <- p +
      facet_grid(type ~ .)
  }
  p
}


relaxed_df = get_data('../results/combined_attentional_preference_scores.csv', 'relaxed', FALSE)
strict_df = get_data('../results/combined_attentional_preference_scores.csv', 'strict', FALSE)
strict_df_mb = get_data('../results/combined_attentional_preference_scores_all.csv', 'strict', FALSE)
snr_df = get_data('../results/combined_attentional_preference_scores.csv', FALSE, TRUE)

plot_acc_per_speech_style(strict_df_mb, 'IDS/ADS predictability: Strict quality', '')
ggsave('../plots/predictability_strict.pdf', width = 12, height = 10, units = 'in')
plot_acc_per_speech_style(relaxed_df, 'IDS/ADS predictability: Relaxed quality', '')
ggsave('../plots/predictability_relaxed.pdf', width = 12, height = 10, units = 'in')
plot_acc_per_speech_style(snr_df, 'IDS/ADS predictability: SNR quality', '')
ggsave('../plots/predictability_snr.pdf', width = 12, height = 10, units = 'in')

plot_acc_per_corpus(strict_df, 'Model competence', 'Strict quality')
plot_acc_per_corpus(relaxed_df, 'Model competence', 'Relaxed quality')
plot_acc_per_corpus(snr_df, 'Model competence', 'SNR automatic quality')

plot_acc_per_model(strict_df, 'Difficulty test corpus', 'Strict quality')
plot_acc_per_model(relaxed_df, 'Difficulty test corpus', 'Relaxed quality')
plot_acc_per_model(snr_df, 'Difficulty test corpus', 'SNR automatic quality')

plot_acc_counterbalanced_scenario(strict_df, 'Counterbalanced scenario', 'Strict quality')
plot_acc_counterbalanced_scenario(relaxed_df, 'Counterbalanced scenario', 'Relaxed quality')
plot_acc_counterbalanced_scenario(snr_df, 'Counterbalanced scenario', 'SNR automatic quality')

plot_acc_counterbalanced_scenario(strict_df, 'Counterbalanced scenario', 'Strict quality', TRUE)
plot_acc_counterbalanced_scenario(relaxed_df, 'Counterbalanced scenario', 'Relaxed quality', TRUE)
plot_acc_counterbalanced_scenario(snr_df, 'Counterbalanced scenario', 'SNR automatic quality', TRUE)
