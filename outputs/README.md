# `outputs/`

Le cache local des données. **Rien ici n'est versionné** (cf. `.gitignore`) :
ce sont les chiffres réels de la brasserie, et le dépôt est public.

`R/connect.R` y dépose un `.RData` horodaté à chaque import réussi, et relit
au démarrage suivant **le plus récent de ceux qui s'y trouvent**, quelle que
soit sa date — c'est ce qui permet au dashboard de s'ouvrir en quelques
secondes. Si le dossier Drive en porte un plus frais, c'est celui-là qui est
téléchargé.

Pour forcer un import complet : `connexion_ou_creation(force_import = TRUE)`,
ou le bouton « refaire l'import » de la pastille de fraîcheur. Supprimer les
fichiers ne suffit plus : le dashboard ira chercher sur Drive.

Le dossier doit exister — d'où ce fichier, qui est aussi ce qui le fait
voyager avec le dépôt.
