# [QCBS R Workshop series](https://wiki.qcbs.ca/r)

This series of 10 workshops walks participants through the steps required to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current series coordinators, listed [here](https://wiki.qcbs.ca/r) or open a pull request (see contributing guidelines at https://qcbsrworkshops.github.io/guidelines.html).

# [Série d'ateliers R du CSBQ](https://wiki.qcbs.ca/r)

Cette série de 10 ateliers guide les participants à travers les étapes requises afin de maîtriser le logiciel R pour une grande variété d’analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d’utilisateurs de R.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter les coordonnateurs actuels de la série, listés [ici](https://wiki.qcbs.ca/r).

# Workshop 7: Generalized linear mixed models
[![Build Status](https://img.shields.io/travis/QCBSRworkshops/workshop07/dev?style=flat-square&logo=travis)](https://travis-ci.org/QCBSRworkshops/workshop07)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop07)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=wiki&message=07&logo=wikipedia)](https://wiki.qcbs.ca/r_workshop7)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=07&color=red&logo=html5)](https://qcbsrworkshops.github.io/workshop07/workshop07-en/workshop07-en.html)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=07&color=red&logo=adobe-acrobat-reader)](https://qcbsrworkshops.github.io/workshop07/workshop07-en/workshop07-en.pdf)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=07&color=2a50b8&logo=r)](https://qcbsrworkshops.github.io/workshop07/workshop07-en/workshop07-en.R)

A significant limitation of linear models is that they cannot accommodate
response variables that do not have a normal error distribution. Most biological
data do not follow the assumption of normality. In this workshop, you will learn
how to use generalized linear models, which are important tools to overcome the
distributional assumptions of linear models. You will learn the major
distributions used depending on the nature of the response variables, the
concept of the link function, and how to verify assumptions of such models. We
will also build on the previous workshop to combine knowledge on linear mixed
models and extend it to generalized linear mixed effect models.


# Atelier 7: Modèles linéaires généralisés (mixtes)
[![Build Status](https://img.shields.io/travis/QCBSRworkshops/workshop07/dev?style=flat-square&logo=travis)](https://travis-ci.org/QCBSRworkshops/workshop07)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop07)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=wiki&message=07&logo=wikipedia)](https://wiki.qcbs.ca/r_atelier7)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=07&color=red&logo=html5)](https://qcbsrworkshops.github.io/workshop07/workshop07-fr/workshop07-fr.html)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=07&color=red&logo=adobe-acrobat-reader)](https://qcbsrworkshops.github.io/workshop07/workshop07-fr/workshop07-fr.pdf)
[![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=07&color=2a50b8&logo=r)](https://qcbsrworkshops.github.io/workshop07/workshop07-fr/workshop07-fr.R)

Les modèles linéaires généralisés sont des outils importants afin de surmonter
un problème récurrent des modèles linéaires, c.-à-d. les variables réponses
n'ayant pas une distribution normale des résidus. Dans cet atelier, vous
apprendrez les distributions principales en fonction de la nature de la variable
réponse, le concept de fonction de lien, et comment vérifier les suppositions de
base de ces modèles. Nous allons également nous baser sur ce qui a été appris
dans l'atelier 6 afin d'introduire les modèles linéaires généralisés avec
effets mixtes.