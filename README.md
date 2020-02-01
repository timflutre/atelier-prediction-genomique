<!-- pandoc README.md -f commonmark -t html -s -o README.html -->

# Atelier de prédiction et sélection génomique

Ce répertoire contient le matériel pédagogique utilisé lors de l'atelier de **prédiction et sélection génomique** organisé et animé depuis 2015 par Jacques David ([Montpellier SupAgro](http://supagro.fr/)) et Timothée Flutre ([INRAE](http://www.inra.fr/)).
Cet atelier se déroule dans le cadre des options [APIMET](http://www.agro-montpellier.fr/web/pages/?idl=19&page=216&id_page=630) (3e année du cursus d'ingénieur agronome) et SEPMET.

* Depuis 2016, Philippe Brabant ([AgroParisTech](http://www.agroparistech.fr/)) et ses collègues sont associés à cet atelier, dans le cadre de la spécialisation [PISTv](http://www.agroparistech.fr/Produire-et-innover-dans-les-systemes-techniques-vegetaux-PISTv) (3e année du cursus d'ingénieur).

* En 2017 et 2018, Friedrich Longin ([Université de Hohenheim](https://lsa-weizen.uni-hohenheim.de/)) a participé à l'atelier, en présentant ses travaux sur l'optimisation des schémas de sélection.

* En 2018, Julien Diot (étudiant en option [Data Science](https://www.supagro.fr/web/pages/idl=19&page=216&id_page=4276) de Montpellier SupAgro) a participé à l'atelier en tant que développeur principal de l'interface web du jeu sérieux ([code](https://sourcesup.renater.fr/www/plantbreedgame/)).

* Depuis 2019, l'encadrement de l'atelier a évolué, avec T. Flutre qui est parti à l'UMR [GQE-Le Moulon](http://moulon.inra.fr/) et de nouveaux collègues qui sont arrivés, comme Charlotte Brault (doctorante avec l'[IFV](https://www.vignevin.com/)) et Vincent Ségura (INRAE).

Le copyright concernant le matériel pédagogique de l'atelier appartient à l'INRAE, Montpellier SupAgro, AgroParisTech et/ou l'université de Hohenheim.
Afin de favoriser la collaboration pédagogique, le contenu des documents est sous licence Creative Commons Attribution-ShareAlike 4.0 International ([CC BY-SA 4.0](http://creativecommons.org/licenses/by-sa/4.0/)).

Les différentes versions du matériel pédagogique sont gérées avec le logiciel [git](https://git-scm.com/), et le dépôt central est hébergé sur [GitHub](https://github.com/timflutre/atelier-prediction-genomique).
Le contenu du dépôt évolue d'une année sur l'autre, mais le contenu d'une année donnée est récupérable via les [étiquettes](https://github.com/timflutre/atelier-prediction-genomique/tags) (*tags*).


# Reproductibilité

Un effort marqué a été fourni pour produire du matériel pédagogique reproductible.
Voici par exemple le mode opératoire pour reproduite le matériel pédagogique du document `premiers-pas`:

* téléchargez une version récente du logiciel [R](http://cran.univ-lyon1.fr/) (>= 3.0)

* téléchargez une version récente du logiciel [RStudio](http://www.rstudio.com/products/rstudio/download/)

* téléchargez le paquet [MASS](https://cran.r-project.org/package=MASS) via la commande suivante dans R: `install.packages("MASS")`

* téléchargez le fichier [premiers-pas.Rmd](https://github.com/timflutre/atelier-prediction-genomique/blob/master/premiers-pas.Rmd) à partir de GitHub (cliquez sur `Raw` en haut à droite, puis faites un "clic droit" et "Enregistrez-sous" pour le sauver dans votre dossier de travail); enlevez l'extension `.txt` si elle a été rajoutée automatiquement (il faut que le nom du fichier se termine par `.Rmd`).

* ouvrez RStudio, faites `File` puis `Open file...`, et sélectionnez le fichier Rmd que vous venez de sauver; si les accents s'affichent mal, cliquez à nouveau sur `File` puis `Reopen with encoding...` et choisissez `UTF-8`.

* cliquez sur `Knit HTML` (installez les paquets requis si ce n'est pas déjà fait); RStudio va mouliner pour convertir le fichier au format Rmd en un fichier au format HTML, et ce dernier devrait ensuite apparaître à l'écran; comme tout fichier HTML, vous pouvez aussi l'ouvrir avec tout navigateur web
