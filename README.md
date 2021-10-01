# NMK

Outils d'analyse d'un portefeuille financier

## Environnement

* R version 3.6.3
* Packages utilisés
  * `PerformanceAnalytics` : évaluer les performances des stratégies et du portefeuille [Vignettes dispo](https://cran.r-project.org/web/packages/PerformanceAnalytics/index.html)
  * `xts` : manipuler des series temporelles [vignette](https://cran.r-project.org/web/packages/xts/vignettes/xts.pdf)
  * `tidyquant` : utiliser les outils d'analyse de portefeuille avec le tidyverse [vignettes](https://cran.r-project.org/web/packages/tidyquant/index.html) et [github](https://business-science.github.io/tidyquant/reference/tidyquant.html)
  * `quantmod` : télécharger des données depuis internet [site](https://www.quantmod.com/examples/data/) 
  * `quandl` : télécharger des données depuis internet [site](https://data.nasdaq.com/tools/r) => créer un compte pour obtenir une clé API et ensuite 50 requetes par jour
  * `FFdwnload` :  Kenneth French’s famous data library providing academia with US and international Asset Pricing factors and portfolios. https://www.sebastianstoeckl.com/post/ffdownload/
  * `portfolioAnalytics` 
  

  
## Bibliographie 

* Portfolio optimization with R/Rmetrics [livre](https://www.rmetrics.org/ebooks-portfolio) **A lire**
* Topics in Empirical Finance with R and Rmetrics [livre](https://www.rmetrics.org/ebooks-henaff) **A lire**
* Portfolio management with R [livre](http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html) **A lire**
* Backtesting, Enrico Schumann, 2018 [papier](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3374195) **A lire**

## Tutoriels

* [Portfolio Management with R, Enrico Schumann, 2021](http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html)
* [Tidy Portfoliomanagement in R, Sebastian Stöckl, 2018](https://bookdown.org/sstoeckl/Tidy_Portfoliomanagement_in_R/)
* [Backtesting Strategies with R, Tim Trice, 2016](https://timtrice.github.io/backtesting-strategies/index.html)

## Data provider

* `Nasdaq Data Link` : nouveau quandl, une partie des données sont gratuites https://data.nasdaq.com/search?filters=%5B%22Free%22%5D
* `Alphavantage` :  une partie des données sont gratuites (nécessite une clé API pour y accéder) https://www.alphavantage.co/ (67L4KQSVHW4SCT2L)

