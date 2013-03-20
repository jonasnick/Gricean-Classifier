Gricean-Classifier
==================

Attempt to predict closed questions on stack overflow using R. 
An article about specifically the influence of repuation and a relation to (Grice's Maxims)[http://en.wikipedia.org/wiki/Cooperative_principle#Grice.27s_Maxims] can be found in the folder writtenReport/writtenReport.pdf.
The challenge is (on kaggle)[https://www.kaggle.com/c/predict-closed-questions-on-stack-overflow].
Download emph{train_sample.csv} and \emph{private_leaderboard.csv} in a ./data folder. If your machine has bounded RAM you may be interested in preparing a smaller subsample. 
```shell
cd analyseR
R
```

```R
source("analyseDataset.R")
applyToLeaderboard()
```
And you will get csv files of predictions from different algorithms in your data folder. 
They can then be submitted to the challenge.


