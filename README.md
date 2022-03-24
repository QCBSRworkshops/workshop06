# [QCBS R Workshop Series](https://r.qcbs.ca/)

This series of 10 workshops serves as a guide to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current coordination team, listed [here](https://r.qcbs.ca/about/) or open a pull request (see contributing guidelines at <https://r.qcbs.ca/presenter-developer-protocol/developing-en.html>).

# [Série d'ateliers R du CSBQ](https://r.qcbs.ca/fr/)

Cette série de 10 ateliers servent de guide afin de maîtriser le logiciel R pour une grande variété d'analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d'utilisateurs de `R`.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter l'équipe de coordination de la série, listé [ici](https://r.qcbs.ca/fr/about/) ou ouvrez un pull request (voir les instructions <https://r.qcbs.ca/presenter-developer-protocol/developper-fr.html>).

# Workshop 6: Generalized linear models

A significant limitation of linear models is that they cannot accommodate response variables that do not have a normal error distribution. Most biological data do not follow the assumption of normality. In this workshop, you will learn how to use generalized linear models, which are important tools to overcome the distributional assumptions of linear models. You will learn the major distributions used depending on the nature of the response variables, the concept of the link function, and how to verify assumptions of such models. 

# Atelier 6: Modèles linéaires généralisés

Les modèles linéaires généralisés sont des outils importants afin de surmonter un problème récurrent des modèles linéaires, c.-à-d. les variables réponses n'ayant pas une distribution normale des résidus. Dans cet atelier, vous apprendrez les distributions principales en fonction de la nature de la variable réponse, le concept de fonction de lien, et comment vérifier les suppositions de base de ces modèles.

# Workshop materials

Language | Slides | Bookdown | Script | GitHub 
:--------|:-------|:-----|:------ |:-------
EN | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=06&color=red&logo=html5)](https://r.qcbs.ca/workshop06/pres-en/workshop06-pres-en.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=book&message=06&logo=github)](https://r.qcbs.ca/workshop06/book-en/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=06&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop06/book-en/workshop06-script-en.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop06) 
FR | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=06&color=red&logo=html5)](https://r.qcbs.ca/workshop06/pres-fr/workshop06-pres-fr.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=livre&message=06&logo=github)](https://r.qcbs.ca/workshop06/book-fr/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=06&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop06/book-fr/workshop06-script-fr.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop06) 

> *Note: The wiki for this workshop was converted to Bookdown in March 2021. <br> The wiki pages for this workshop will no longer be updated (Archive: [EN](https://wiki.qcbs.ca/r_workshop7), [FR](https://wiki.qcbs.ca/r_atelier7)).*  

# License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

# Contributions 

Since 2014, several QCBS members contributed to consistently and collaboratively develop and update this workshop, as part of the *Learning and Development Award* from the Québec Centre for Biodiversity Science. They were:

|      2022 - 2021 - 2020     |      2019 - 2018 - 2017     |      2016 - 2015 - 2014      |
|:---------------------------:|:---------------------------:|:----------------------------:|
|    Pedro Henrique P. Braga  |      Azenor Bideault        |  Cédric Frenette Dussault    |
|       Katherine Hébert      |       Willian Vieira        |         Thomas Lamy          |
|        Alex Arkilanian      |  Pedro Henrique P. Braga    |        Zofia Taranu          |
|     Mathieu Vaillancourt    |      Marie Hélène Brice     |       Vincent Fugère         |
|        Laurie Maynard       |        Kevin Cazelles       |                              |
|       Esteban Góngora       |                             |                              |

# Development status

**Template** 

[![receive-from-template-and-dispatch-to-workflows](https://github.com/QCBSRworkshops/workshop06/workflows/receive-from-template-and-dispatch-to-workflows/badge.svg)](https://github.com/QCBSRworkshops/workshop06/actions?query=workflow%3Areceive-from-template-and-dispatch-to-workflows) 

**Building workshop materials**

Language | Slides | Book
:------- | :----- | :-----
EN  | [![render-presentation-en](https://github.com/QCBSRworkshops/workshop06/workflows/render-presentation-en/badge.svg)](https://github.com/QCBSRworkshops/workshop06/actions?query=workflow%3Arender-presentation-en) | [![render-book-en](https://github.com/QCBSRworkshops/workshop06/workflows/render-book-en/badge.svg)](https://github.com/QCBSRworkshops/workshop06/actions?query=workflow%3Arender-book-en)
FR   | [![render-presentation-fr](https://github.com/QCBSRworkshops/workshop06/workflows/render-presentation-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop06/actions?query=workflow%3Arender-presentation-fr) | [![render-book-fr](https://github.com/QCBSRworkshops/workshop06/workflows/render-book-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop06/actions?query=workflow%3Arender-book-fr)
