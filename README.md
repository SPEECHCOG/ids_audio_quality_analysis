# Analysing the Impact of Audio Quality on the Use of Naturalistic Long-Form Recordings for Infant-Directed Speech Research

Modelling of early language acquisition aims to understand how infants bootstrap their language skills. The modelling encompasses properties of the input data used for training the models, the cognitive hypotheses and their algorithmic implementations being tested, and the evaluation methodologies to compare models to human data. Recent developments have enabled the use of more naturalistic training data for computational models. This also motivates development of more naturalistic tests of model behaviour. A crucial step towards such an aim is to develop representative speech datasets consisting of speech heard by infants in their natural environments. However, a major drawback of such recordings is that they are typically noisy, and it is currently unclear how the sound quality could affect analyses and modelling experiments conducted on such data. In this paper, we explore this aspect for the case of infant-directed speech (IDS) and adult-directed speech (ADS) analysis. First, we manually and automatically annotated audio quality of utterances extracted from two corpora of child-centred long-form recordings (in English and French). We then compared acoustic features of IDS and ADS in an in-lab dataset and across different audio quality subsets of naturalistic data. Finally, we assessed how the audio quality and recording environment may change the conclusions of a modelling analysis using a recent self-supervised learning model. Our results show that the use of modest and high audio quality naturalistic speech data result in largely similar conclusions on IDS and ADS in terms of acoustic analyses and modelling experiments. We also found that an automatic sound quality assessment tool can be used to screen out useful parts of long-form recordings for a closer analysis with comparable results to that of manual quality annotation. 

## R scripts
This repository contains the acoustic and predictability metrics extracted for the the analyses and r scripts used for the reported results. We used R version 4.2.0

* `r_scripts`: This folder contains `summarise_acoustic_results` and `summarise_preference_results` the first one produces the statistical analysis of the acoustic features and the second one produces the plots for the predictability of IDS and ADS recordings for the different qualities.
* `results`: This folder contains the csv files with the metrics for each recording in the two corpora.
* `plots`: This folder contains the plots reported in the paper

## Citing this work
"Analysing the Impact of Audio Quality on the Use of Naturalistic Long-Form Recordings for Infant-Directed Speech Research". María Andrea Cruz Blandón, Alejandrina Cristia, Okko Räsänen. Accepted for publication in Proceedings of the 44th Annual Meeting of the Cognitive Science Society (CogSci 2023). 2023 


## Contact
If you find any issue please reporte it on the [issues section](https://github.com/SPEECHCOG/ids_audio_quality_analysis/issues) in this repository. Further comments can be sent to `maria <dot> cruzblandon <at> tuni <dot> fi`
