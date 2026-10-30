# `www/`

Shiny sert ce dossier à la racine du site : `style.css` s'y référence donc
simplement par son nom.

`style.css` ne contient que ce que bslib ne sait pas faire, et chaque règle y
dit **pourquoi** elle existe. Plusieurs d'entre elles corrigent un défaut qui
ne se voit qu'au navigateur : un tableau qui recouvre la note sous lui, une
ligne sélectionnée qui doit se lire comme une ligne retirée, un widget que la
grille comprime sous sa propre hauteur. Les retirer sans lire le commentaire,
c'est les refaire.

La feuille est servie **versionnée** (`feuille_versionnee()` dans `ui.R`) :
sans ça, un navigateur qui l'a déjà en cache garde l'ancienne après un
déploiement, et le défaut qu'on vient de corriger reste à l'écran.
