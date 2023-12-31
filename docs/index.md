---
title: "Network Analysis: Integrating Social Network Theory, Method, and Application with R"
author: "Craig Rawlings, Jeffrey A. Smith, James Moody, and Daniel McFarland"
site: bookdown::bookdown_site
output: 
  bookdown::gitbook:
    highlight: default
    md_extensions:  -smart
    css: style.css
    config:
      toc:
        collapse: section
        scroll_highlight: true
        before: |
          <li><a href="https://inarwhal.github.io/NetworkAnalysisR-book/">
          Network Analysis with R</a></li>
        after: null
      fontsettings:
        theme: white
        family: serif
        size: 2.5
      sharing:
        facebook: false
        github: false
        twitter: false
        linkedin: false
        weibo: false
        instapaper: false
        vk: false
        whatsapp: false
        all: ['facebook', 'twitter', 'linkedin', 'weibo']
      info: false
documentclass: book
bibliography: ["references.bib"]
link-citations: no
description: "This is the website for the R tutorials associated with Network Analysis Integrating Social Network Theory, Method, and Application with R"

---

# Introduction {#ch1-intro}

<style>
p.comment {
padding: 10px;
border: 1px solid black;
margin-left: 25px;
border-radius: 5px;
}

</style>

<img src="NA_RawlingsSmithMoodyMcFarland3.png"
style="float:right; margin: 0px 0px 0px 10px; width: 30%; height: 30%;" />

Welcome to the website for *Network Analysis: Integrating Social Network Theory, Method, and Application with R*. Here you will find the R tutorials that accompany the printed manuscript, which is available through [Cambridge University Press](https://www.cambridge.org/us/universitypress/subjects/sociology/research-methods-sociology-and-criminology/network-analysis-integrating-social-network-theory-method-and-application-r). 

The printed manuscript offers substantive, theoretical and methodological discussions on how to conceptually conduct network analysis. The printed book thus offers the motivation and logic behind asking research questions from a network perspective. These tutorials serve as the practical counterpart, offering detailed examples on how to manipulate, visualize, summarize and analyze network data in R. The tutorials are motivated by substantive problems and include in-depth examples and interpretation. Many, but not all, of the examples are based on adolescents in school, as they serve as a familiar case study useful for drawing out larger, more general themes.

## How to Read the Book

The material on this website is meant to be paired with the printed manuscript. It is not an online version of the printed book. A reader would ideally read a chapter in the printed manuscript and then walk through the associated online R tutorials step-by-step.  Readers may choose to go through each R tutorial in order or opt to cover specific topics of interest, depending on the goals and experience of the reader. Each tutorial is self-contained, so that more experienced readers could choose to cover the tutorials out of order. For those readers not strictly following the published book, it is important to remember that the R tutorials are numbered to coincide exactly with the chapters in the published manuscript. 

The book covers a wide range of topics related to network analysis. There are often multiple tutorials associated with a given topic. Topics include: 

* [Data Management](#ch3-Network-Data-R)
* [Missing Data](#ch4-Missing-Network-Data-R)
* [Visualization](#ch5-Network-Visualization-R)
* [Ego Networks](#ch6-Ego-Network-Data-R)
* [Dyads and Triads](#ch7-Dyads-Triads-R)
* [Cohesion and Communities](#ch8-Network-Cohesion-Communities-R)
* [Centrality](#ch9-Network-Centrality-Hierarchy-R)
* [Positions and Roles](#ch10-Positions-Roles-R)
* [Affiliations and Duality](#ch11-Two-mode-Networks)
* [Networks and Culture](#ch12-Networks-Culture-R)
* [Statistical Network Models](#ch13-Statistical-Models-Networks-R)
* [Diffusion](#ch14-Network-Diffusion-R)
* [Social Influence](#ch15-coevolution-networks-behaviors-siena-saom-R)

## Citations and Use

You can cite the tutorials on this website as:

<p class="comment">
Rawlings, Craig M., Jeffrey A. Smith, James Moody, and Daniel A. McFarland 2023. _Network Analysis: Integrating Social Network Theory, Method, and Application with R_. New York: Cambridge University Press.</p>

The online R tutorials, like the printed manuscript, is in copyright. No reproduction of any part may take place without the written permission of Cambridge University Press & Assessment
(© Craig M. Rawlings, Jeffrey A. Smith, James Moody, and Daniel A. McFarland, 2023). The tutorials are, however, made freely available through this site.  

## Updates and Feedback

The authors are committed to keeping these chapters as up to date as possible, especially when there are major updates to key packages. The original version of the tutorials were completed and published in August 2023. It is possible that future versions of this online book will include additional tutorials on topics not currently covered. If you find errors or breaks in any of the code you can note them here:  https://github.com/JeffreyAlanSmith/Integrated_Network_Science/issues
or contact one of the authors directly. You can also find additional functions and data sets used throughout this book on the following github site: https://github.com/JeffreyAlanSmith/Integrated_Network_Science.

## Acknowledgments

There are many people we would like to thank for providing feedback and suggestions on these tutorials. Special thanks goes to Robin Gauthier, Sela Harcey and Julia McQuillan for their insightful comments and support, as well as graduate students Gabriel Varela, Tom Wolff, and Joe Quinn for reviews and beta testing. These tutorials have also been taught at various network analysis classes at Duke, Stanford and UNL. The advice and suggestions of our students have greatly strengthened the material presented here. We would also like to thank Mark Granovetter for his continued support, enthusiasm and belief in this project.

Many of the R tutorials presented in this textbook were built off prior versions developed at Stanford University by Daniel McFarland, Solomon Messing, Michael Nowak, Sean J. Westwood, and Sanne Smith. Chapter 5’s tutorial for NDTV drew on Skye Bender-deMoll’s materials; Chapter 12 on LDA/CA from Love Börjeson and Daniel McFarland; Chapter 13 concerning “ERGM” and “relevant” drew on Carter Butts’ materials; Chapter 15 on SIENA/SAOM drew on ICS materials. Finally, a great many resources from the Duke Network Analysis Center (DNAC) helped us in formulating elements in many of the tutorials. For example, Chapter 4 on missing data imputation drew on James Moody, Jeffrey A. Smith and Jon Morgan's work, as did Chapter 14 on diffusion. We are grateful to these institutions and individuals for sharing code and helping us formulate applications for each chapter’s theories.

## Author Informtion 

Craig M. Rawlings is Associate Professor of Sociology at Duke University, where he is affiliated with the Duke Network Analysis Center. His work focuses on the connections between social structures and culture, including belief systems, knowledge, meaning-making processes, and attitude change. His publications have appeared in the American Journal of Sociology, American Sociological Review, Social Forces, Sociological Science, and Poetics.

Jeffrey A. Smith is Senior Policy Analyst in Mental Health and Addictions at the Nova Scotia Health Authority. He has done methodological work on network sampling and missing data, as well as more substantive work on network processes, drug use, and health outcomes. His work has been published in the American Sociological Review, Sociological Methodology, Social Networks, and other venues.

James Moody is Professor of Sociology at Duke University and focuses on the network foundations of social cohesion and diffusion, using network analysis to help understand topics including racial segregation, disease spread, and the development of scientific disciplines. He has won the Freeman Award for contributions to network analysis and a James S. McDonnel Foundation Complexity Scholars award.

Daniel A. McFarland is Professor of Education and (by courtesy) Sociology and Organizational Behavior at Stanford University, where he founded Stanford’s Center for Computational Social Science. His past work studied social network dynamics of communication, relationships, affiliations, and knowledge structures in educational contexts. His current work integrates social network analysis and natural language processing to study the development of scientific knowledge. 

## Session Information

This version of the book was built using R version 4.3.0. See below for the session information:


```
## ─ Session info ───────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.3.0 (2023-04-21)
##  os       macOS Ventura 13.2.1
##  system   x86_64, darwin20
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       America/Halifax
##  date     2023-09-20
##  pandoc   2.11.4 @ /Applications/RStudio.app/Contents/MacOS/pandoc/ (via rmarkdown)
## 
## ─ Packages ───────────────────────────────────────────────────────────────────
##  package        * version      date (UTC) lib source
##  abind            1.4-5        2016-07-21 [1] CRAN (R 4.3.0)
##  animation        2.7          2021-10-07 [1] CRAN (R 4.3.0)
##  ape              5.7-1        2023-03-13 [1] CRAN (R 4.3.0)
##  askpass          1.1          2019-01-13 [1] CRAN (R 4.3.0)
##  backports        1.4.1        2021-12-13 [1] CRAN (R 4.3.0)
##  base64           2.0.1        2022-08-19 [1] CRAN (R 4.3.0)
##  base64enc        0.1-3        2015-07-28 [1] CRAN (R 4.3.0)
##  boot             1.3-28.1     2022-11-22 [1] CRAN (R 4.3.0)
##  brio             1.1.3        2021-11-30 [1] CRAN (R 4.3.0)
##  broom            1.0.5        2023-06-09 [1] CRAN (R 4.3.0)
##  bslib            0.5.0        2023-06-09 [1] CRAN (R 4.3.0)
##  cachem           1.0.8        2023-05-01 [1] CRAN (R 4.3.0)
##  callr            3.7.3        2022-11-02 [1] CRAN (R 4.3.0)
##  car              3.1-2        2023-03-30 [1] CRAN (R 4.3.0)
##  carData          3.0-5        2022-01-06 [1] CRAN (R 4.3.0)
##  chk              0.9.0        2023-05-27 [1] CRAN (R 4.3.0)
##  cli              3.6.1        2023-03-23 [1] CRAN (R 4.3.0)
##  coda             0.19-4       2020-09-30 [1] CRAN (R 4.3.0)
##  codetools        0.2-19       2023-02-01 [1] CRAN (R 4.3.0)
##  colorspace       2.1-0        2023-01-23 [1] CRAN (R 4.3.0)
##  cpp11            0.4.3        2022-10-12 [1] CRAN (R 4.3.0)
##  crayon           1.5.2        2022-09-29 [1] CRAN (R 4.3.0)
##  curl             5.0.1        2023-06-07 [1] CRAN (R 4.3.0)
##  DBI              1.1.3        2022-06-18 [1] CRAN (R 4.3.0)
##  DEoptimR         1.0-14       2023-06-09 [1] CRAN (R 4.3.0)
##  desc             1.4.2        2022-09-08 [1] CRAN (R 4.3.0)
##  deSolve          1.35         2023-03-12 [1] CRAN (R 4.3.0)
##  diffobj          0.3.5        2021-10-05 [1] CRAN (R 4.3.0)
##  digest           0.6.31       2022-12-11 [1] CRAN (R 4.3.0)
##  doParallel       1.0.17       2022-02-07 [1] CRAN (R 4.3.0)
##  dplyr            1.1.2        2023-04-20 [1] CRAN (R 4.3.0)
##  egor             1.23.3       2023-03-16 [1] CRAN (R 4.3.0)
##  ellipsis         0.3.2        2021-04-29 [1] CRAN (R 4.3.0)
##  EpiModel         2.4.0        2023-06-20 [1] CRAN (R 4.3.0)
##  ergm             4.5.0        2023-05-28 [1] CRAN (R 4.3.0)
##  ergm.count       4.1.1        2022-05-25 [1] CRAN (R 4.3.0)
##  ergm.ego         1.1.0        2023-05-30 [1] CRAN (R 4.3.0)
##  ergm.multi       0.2.0        2023-05-30 [1] CRAN (R 4.3.0)
##  evaluate         0.21         2023-05-05 [1] CRAN (R 4.3.0)
##  fansi            1.0.4        2023-01-22 [1] CRAN (R 4.3.0)
##  farver           2.1.1        2022-07-06 [1] CRAN (R 4.3.0)
##  fastmap          1.1.1        2023-02-24 [1] CRAN (R 4.3.0)
##  fontawesome      0.5.1        2023-04-18 [1] CRAN (R 4.3.0)
##  forcats          1.0.0        2023-01-29 [1] CRAN (R 4.3.0)
##  foreach          1.5.2        2022-02-02 [1] CRAN (R 4.3.0)
##  fs               1.6.2        2023-04-25 [1] CRAN (R 4.3.0)
##  generics         0.1.3        2022-07-05 [1] CRAN (R 4.3.0)
##  GGally           2.1.2        2021-06-21 [1] CRAN (R 4.3.0)
##  ggnetwork        0.5.12       2023-03-06 [1] CRAN (R 4.3.0)
##  ggplot2          3.4.2        2023-04-03 [1] CRAN (R 4.3.0)
##  ggrepel          0.9.3        2023-02-03 [1] CRAN (R 4.3.0)
##  glue             1.6.2        2022-02-24 [1] CRAN (R 4.3.0)
##  gtable           0.3.3        2023-03-21 [1] CRAN (R 4.3.0)
##  highr            0.10         2022-12-22 [1] CRAN (R 4.3.0)
##  hms              1.1.3        2023-03-21 [1] CRAN (R 4.3.0)
##  htmltools        0.5.5        2023-03-23 [1] CRAN (R 4.3.0)
##  htmlwidgets      1.6.2        2023-03-17 [1] CRAN (R 4.3.0)
##  igraph           1.5.0        2023-06-16 [1] CRAN (R 4.3.0)
##  intergraph       2.0-2        2016-12-05 [1] CRAN (R 4.3.0)
##  isoband          0.2.7        2022-12-20 [1] CRAN (R 4.3.0)
##  iterators        1.0.14       2022-02-05 [1] CRAN (R 4.3.0)
##  jquerylib        0.1.4        2021-04-26 [1] CRAN (R 4.3.0)
##  jsonlite         1.8.5        2023-06-05 [1] CRAN (R 4.3.0)
##  knitr            1.43         2023-05-25 [1] CRAN (R 4.3.0)
##  labeling         0.4.2        2020-10-20 [1] CRAN (R 4.3.0)
##  lattice          0.21-8       2023-04-05 [1] CRAN (R 4.3.0)
##  lazyeval         0.2.2        2019-03-15 [1] CRAN (R 4.3.0)
##  lifecycle        1.0.3        2022-10-07 [1] CRAN (R 4.3.0)
##  lme4             1.1-33       2023-04-25 [1] CRAN (R 4.3.0)
##  lpSolveAPI       5.5.2.0-17.9 2022-10-20 [1] CRAN (R 4.3.0)
##  magick           2.7.4        2023-03-09 [1] CRAN (R 4.3.0)
##  magrittr         2.0.3        2022-03-30 [1] CRAN (R 4.3.0)
##  MASS             7.3-60       2023-05-04 [1] CRAN (R 4.3.0)
##  MatchIt          4.5.4        2023-06-14 [1] CRAN (R 4.3.0)
##  Matrix           1.5-4.1      2023-05-18 [1] CRAN (R 4.3.0)
##  MatrixModels     0.5-1        2022-09-11 [1] CRAN (R 4.3.0)
##  memoise          2.0.1        2021-11-26 [1] CRAN (R 4.3.0)
##  mgcv             1.8-42       2023-03-02 [1] CRAN (R 4.3.0)
##  mime             0.12         2021-09-28 [1] CRAN (R 4.3.0)
##  minqa            1.2.5        2022-10-19 [1] CRAN (R 4.3.0)
##  mitools          2.4          2019-04-26 [1] CRAN (R 4.3.0)
##  munsell          0.5.0        2018-06-12 [1] CRAN (R 4.3.0)
##  NbClust          3.0.1        2022-05-02 [1] CRAN (R 4.3.0)
##  ndtv             0.13.3       2022-11-20 [1] CRAN (R 4.3.0)
##  netdiffuseR      1.22.5       2022-12-02 [1] CRAN (R 4.3.0)
##  network          1.18.1       2023-01-24 [1] CRAN (R 4.3.0)
##  networkD3        0.4          2017-03-18 [1] CRAN (R 4.3.0)
##  networkDynamic   0.11.3       2023-02-16 [1] CRAN (R 4.3.0)
##  networkLite      1.0.5        2023-03-10 [1] CRAN (R 4.3.0)
##  nlme             3.1-162      2023-01-31 [1] CRAN (R 4.3.0)
##  nloptr           2.0.3        2022-05-26 [1] CRAN (R 4.3.0)
##  nnet             7.3-19       2023-05-03 [1] CRAN (R 4.3.0)
##  numDeriv         2016.8-1.1   2019-06-06 [1] CRAN (R 4.3.0)
##  openssl          2.0.6        2023-03-09 [1] CRAN (R 4.3.0)
##  pbkrtest         0.5.2        2023-01-19 [1] CRAN (R 4.3.0)
##  pillar           1.9.0        2023-03-22 [1] CRAN (R 4.3.0)
##  pkgconfig        2.0.3        2019-09-22 [1] CRAN (R 4.3.0)
##  pkgload          1.3.2        2022-11-16 [1] CRAN (R 4.3.0)
##  plyr             1.8.8        2022-11-11 [1] CRAN (R 4.3.0)
##  praise           1.0.0        2015-08-11 [1] CRAN (R 4.3.0)
##  prettyunits      1.1.1        2020-01-24 [1] CRAN (R 4.3.0)
##  processx         3.8.1        2023-04-18 [1] CRAN (R 4.3.0)
##  progress         1.2.2        2019-05-16 [1] CRAN (R 4.3.0)
##  ps               1.7.5        2023-04-18 [1] CRAN (R 4.3.0)
##  purrr            1.0.1        2023-01-10 [1] CRAN (R 4.3.0)
##  quantreg         5.95         2023-04-08 [1] CRAN (R 4.3.0)
##  R6               2.5.1        2021-08-19 [1] CRAN (R 4.3.0)
##  rappdirs         0.3.3        2021-01-31 [1] CRAN (R 4.3.0)
##  rbibutils        2.2.13       2023-01-13 [1] CRAN (R 4.3.0)
##  RColorBrewer     1.1-3        2022-04-03 [1] CRAN (R 4.3.0)
##  Rcpp             1.0.10       2023-01-22 [1] CRAN (R 4.3.0)
##  RcppArmadillo    0.12.4.1.0   2023-06-19 [1] CRAN (R 4.3.0)
##  RcppEigen        0.3.3.9.3    2022-11-05 [1] CRAN (R 4.3.0)
##  RcppProgress     0.4.2        2020-02-06 [1] CRAN (R 4.3.0)
##  Rdpack           2.4          2022-07-20 [1] CRAN (R 4.3.0)
##  relevent         1.2-1        2023-01-24 [1] CRAN (R 4.3.0)
##  rematch2         2.1.2        2020-05-01 [1] CRAN (R 4.3.0)
##  reshape          0.8.9        2022-04-12 [1] CRAN (R 4.3.0)
##  rlang            1.1.1        2023-04-28 [1] CRAN (R 4.3.0)
##  rle              0.9.2        2020-09-25 [1] CRAN (R 4.3.0)
##  rmarkdown        2.22         2023-06-01 [1] CRAN (R 4.3.0)
##  robustbase       0.99-0       2023-06-16 [1] CRAN (R 4.3.0)
##  rprojroot        2.0.3        2022-04-02 [1] CRAN (R 4.3.0)
##  RSiena           1.3.14.1     2023-02-05 [1] CRAN (R 4.3.0)
##  sass             0.4.6        2023-05-03 [1] CRAN (R 4.3.0)
##  scales           1.2.1        2022-08-20 [1] CRAN (R 4.3.0)
##  sna              2.7-1        2023-01-24 [1] CRAN (R 4.3.0)
##  SparseM          1.81         2021-02-18 [1] CRAN (R 4.3.0)
##  srvyr            1.2.0        2023-02-21 [1] CRAN (R 4.3.0)
##  statnet.common   4.9.0        2023-05-24 [1] CRAN (R 4.3.0)
##  stringi          1.7.12       2023-01-11 [1] CRAN (R 4.3.0)
##  stringr          1.5.0        2022-12-02 [1] CRAN (R 4.3.0)
##  survey           4.2-1        2023-05-03 [1] CRAN (R 4.3.0)
##  survival         3.5-5        2023-03-12 [1] CRAN (R 4.3.0)
##  sys              3.4.2        2023-05-23 [1] CRAN (R 4.3.0)
##  tergm            4.2.0        2023-05-30 [1] CRAN (R 4.3.0)
##  testthat         3.1.9        2023-06-15 [1] CRAN (R 4.3.0)
##  tibble           3.2.1        2023-03-20 [1] CRAN (R 4.3.0)
##  tidygraph        1.2.3        2023-02-01 [1] CRAN (R 4.3.0)
##  tidyr            1.3.0        2023-01-24 [1] CRAN (R 4.3.0)
##  tidyselect       1.2.0        2022-10-10 [1] CRAN (R 4.3.0)
##  tinytex          0.45         2023-04-18 [1] CRAN (R 4.3.0)
##  trust            0.1-8        2020-01-10 [1] CRAN (R 4.3.0)
##  utf8             1.2.3        2023-01-31 [1] CRAN (R 4.3.0)
##  vctrs            0.6.3        2023-06-14 [1] CRAN (R 4.3.0)
##  viridisLite      0.4.2        2023-05-02 [1] CRAN (R 4.3.0)
##  waldo            0.5.1        2023-05-08 [1] CRAN (R 4.3.0)
##  withr            2.5.0        2022-03-03 [1] CRAN (R 4.3.0)
##  xfun             0.39         2023-04-20 [1] CRAN (R 4.3.0)
##  xtable           1.8-4        2019-04-21 [1] CRAN (R 4.3.0)
##  yaml             2.3.7        2023-01-23 [1] CRAN (R 4.3.0)
## 
##  [1] /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/library
## 
## ──────────────────────────────────────────────────────────────────────────────
```



