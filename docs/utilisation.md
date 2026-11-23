# Utiliser le dashboard

Le tableau de bord de la brasserie Mazette répond à quatre questions, dans cet
ordre : **qu'est-ce qui se passe maintenant**, **qu'est-ce qui s'est passé**,
**combien ça a rapporté**, et **qu'est-ce qu'on prévoit**.

- [Se connecter, et ce qu'on voit](#se-connecter-et-ce-quon-voit)
- [La barre du haut](#la-barre-du-haut)
- [Les onglets](#les-onglets)
- [Le volet Compta](#le-volet-compta)
- [Astuces](#astuces)

---

## Se connecter, et ce qu'on voit

Un mot de passe par personne. Il ouvre un **profil**, c'est-à-dire une liste
d'onglets : on ne voit que ceux auxquels on a droit, les autres n'existent pas
dans la page. Votre nom s'affiche en haut à droite, à côté du bouton de
déconnexion.

| Profil | Onglets |
|---|---|
| `admin` | tout, comptabilité et coût du personnel compris |
| `support` | tout sauf Réservations |
| `service` | Maintenant, Ventes, Boissons, Nourriture, Réservations, Travail, Planning |
| `transfo` | Maintenant, Ventes, Boissons, Nourriture, Réservations |
| `public` | Maintenant, Historique, Année — pour présenter à l'extérieur |
| `invite` | Maintenant seulement |

Un onglet manque ? C'est votre profil : voir [maintenance](maintenance.md#mots-de-passe-et-profils).

## La barre du haut

- **La date des données**, à gauche de votre nom. Les chiffres affichés vont
  jusqu'à ce jour-là. Sa couleur dit si l'import du matin a tourné :

  | Couleur | Sens |
  |---|---|
  | beige | à jour (les données vont jusqu'à hier) |
  | ambre | 1 à 3 jours de retard : l'import n'a pas tourné |
  | rouge | 4 jours ou plus : il faut aller voir |

  Un **clic sur la date** propose deux gestes :
  - *Reprendre le dernier import* — quelques secondes : télécharge les données
    préparées le matin. C'est le geste courant.
  - *Refaire l'import* — quelques minutes, associés seulement : relit toutes
    les sources. À faire après avoir corrigé un classeur.

  Dans les deux cas l'écran se recharge et le mot de passe est redemandé.
  Si quelqu'un d'autre met les données à jour pendant que vous travaillez, la
  pastille passe à « nouvelles données » : rechargez quand vous voulez.

- **HTVA / TVAC** : toutes les ventes du dashboard suivent ce choix. La
  comptabilité, elle, est toujours hors TVA.

## Les onglets

### Maintenant

La page d'arrivée : le CA de la veille, de la semaine en cours et son
objectif, les cinq dernières semaines (chaque jour coloré selon son
objectif : **vert** atteint, **ambre** à plus de 90 %, **rouge** en dessous),
la progression du mois, et les produits en hausse ou en baisse par rapport à
la semaine précédente.

Tout y est arrêté à **la veille** : la caisse du jour n'est close que le soir.

### Ventes

La loupe sur une période.

- **Par périodes** : choisissez la *maille* (jour, semaine ou mois) et la
  fenêtre dans la barre latérale, puis **cliquez une barre** du graphe pour
  détailler cette période. On descend ensuite : sa composition (les heures
  d'un jour, les jours d'une semaine…), ce qui s'est vendu, puis — repliés —
  les tickets et les heures travaillées.
  - Une période coupée par la fenêtre (une semaine à cheval sur deux mois) est
    **hachurée** : son total est juste, mais elle ne se compare pas aux autres.
  - Les heures au jour et à la semaine sont les heures **pointées** ; au mois,
    le pointage, la paie et la comptabilité sont donnés côte à côte.
  - Les coûts et la marge n'y sont pas : ils ne se mesurent qu'au mois, dans
    Compta.
- **Par produit** : un produit suivi semaine après semaine, avec sa part du CA
  total et sa part dans sa catégorie.

### Historique

Le chiffre d'affaires sur toute la durée, par semaine, mois ou année, chaque
barre colorée selon son objectif.

### Année

L'année choisie face à son objectif et à l'année précédente, **à date** :
seuls les jours déjà écoulés comptent. Écart cumulé de CA à l'objectif et à
N-1, marge cumulée et son écart à N-1.

La marge vient de la comptabilité, qui est mensuelle : elle s'arrête au
dernier mois comptabilisé. Un bandeau prévient si des mois récents sont
douteux ou ont été écartés dans Compta.

### Boissons

- **Boissons** : la consommation d'une semaine, comparée à la précédente —
  litres par boisson, tendance des principales, heures et jours de
  consommation, formats servis.
- **Fûts** : le niveau de chaque bière en cours, sa date de fin prévue (avec
  une fourchette tôt / tard), et le rapport complet d'un brassin.

### Nourriture

- **Focaccias** : ventes de la semaine, options (fromage, viande, spicy) et
  leur évolution. La carte **Production** propose ce qu'il faut préparer pour
  la semaine qui vient, d'après les dernières semaines complètes ; tout y est
  modifiable, et la dernière ligne est libre.
- **Pizzwanze** : les soirées pizza, détectées toutes seules (au moins deux
  pizzas différentes vendues le même jour). Ventes par heure, par pizza, et
  l'historique des soirées ; les nouveautés de la carte sont marquées.

### Simulation

L'effet d'un changement de prix sur le CA, à quantités égales : choisissez une
période de référence, puis changez un prix dans le tableau ou appliquez un %
à toute une catégorie.

### Compta

Voir [plus bas](#le-volet-compta).

### Travail

- **Pilotage** : les heures posées et le CA par heure de service, mois par
  mois (cliquez une barre pour détailler la période par secteur), puis les
  créneaux types (midi, soir, Pizzwanze) comparés entre eux.
  - Les heures **variables** sont le service, un jour d'ouverture : elles
    suivent l'activité. Les heures **fixes** (cuisine, brasserie, support) ne
    la suivent pas.
  - Heures et coûts viennent de la **paie** dès qu'elle couvre le mois, sinon
    de l'estimation Horeko. La colonne *Source* le dit.
- **Sources** : les trois sources d'heures et de coût du travail côte à côte
  — paie, pointage Horeko, comptabilité. Un écart **constant** est une
  différence de périmètre (pécules, provisions…) ; un écart qui **dérive** est
  un problème de données.

### Planning

Les jours à venir, d'après le planning des shifts. Deux graphiques, une règle :
**la barre est ce qui est prévu, le trait est la référence**.

1. Heures de service planifiées contre les heures habituelles ce jour de
   semaine (ambre au-dessus : point d'attention).
2. CA habituel de ce jour de semaine contre l'objectif du jour.

Les heures fixes et les couverts réservés sont donnés au survol.

### Réservations

Salle et terrasse : les réservations à venir, des statistiques sur une
période, l'historique, et le lien entre couverts réservés et CA du jour.
Seules les réservations enregistrées sont connues : pas de taux de no-show.

### Comparaison

Plusieurs périodes côte à côte (semaines, mois ou années), avec leur CA, leur
objectif et — au mois et à l'année — leurs postes de gestion.

## Le volet Compta

Sept sous-onglets, dans l'ordre des questions qu'on se pose.

| Sous-onglet | La question |
|---|---|
| **Contrôle** | puis-je me fier à ces chiffres ? |
| **Exploitation** | combien, sur cette période ? |
| **Écarts** | pourquoi ça a bougé ? |
| **Évolution** | où ça va ? |
| **Comptabilité générale** | que dit le compte de résultat ? |
| **Écritures** | d'où ça sort ? |
| **Projection** | qu'aurait donné un mois incomplet ? |

### Contrôle

Le titre porte un compteur quand il y a des alertes. On y trouve :

- **les alertes d'intégrité**, sur tout l'historique. Un lien mène aux
  écritures en cause ;
- **le tableau des mois** : une ligne par mois, ses chiffres, un état et
  pourquoi.

  | État | Sens |
  |---|---|
  | À risque | chiffres invraisemblables, mois récent : export probablement incomplet |
  | En cours | le mois n'est pas fini |
  | Ancien, à vérifier | invraisemblable, mais vieux : peut-être un vrai mois creux |
  | Plausible | rien à signaler |
  | Validé / Écarté à la main | quelqu'un a vérifié (voir *Trancher*) |

  La colonne *Comptes* (nombre de comptes mouvementés) rougit quand un mois en
  a beaucoup moins que d'habitude : signe d'un export tronqué.

- **Écarter un mois** : **sélectionnez sa ligne**. Il disparaît de tous les
  autres sous-onglets (et de la marge de l'onglet Année), qui le rappellent
  par un bandeau gris. *Écarter les mois à risque* les sélectionne d'un coup.
  L'écartement ne dure que le temps de votre session.
- **Trancher un mois** : après avoir vérifié dans Odoo, dites si ses chiffres
  sont bons ou faux, avec un commentaire. La décision est enregistrée pour
  tout le monde. Elle tombe d'elle-même si les chiffres du mois changent
  ensuite (de nouvelles écritures arrivent). Le mois en cours ne se tranche
  pas.
- les comptes que le plan comptable ne sait pas classer, et les contrôles de
  lecture des écritures Odoo.

### Exploitation

Le cheminement du CA à la marge sur la période choisie (mois, trimestre ou
année), la structure des charges dans le temps, et le tableau par période.

- Les ratios sont rapportés au **CA seul** (pas aux autres produits) et
  colorés selon leurs bornes de confort : matières 30 / 35 %, travail
  35 / 42 %, frais généraux 20 / 28 %.
- Une cellule colorée du tableau s'écarte de plus de deux écarts de sa propre
  série : plus souvent une erreur de saisie qu'un fait de gestion.
- **La marge** affichée est, par défaut, la marge **avant amortissements** :
  CA + autres produits − matières − rémunérations − frais généraux + résultat
  financier. L'option **Marge** de la barre latérale passe à la marge **après
  amortissements**.

### Écarts

Une période contre une référence, au choix : la période précédente, la même
un an plus tôt, ou la médiane des périodes précédentes.

- **Le pont de marge** sépare l'**effet volume** (le CA a bougé, les poids non)
  de la **dérive de chaque poste** (son poids dans le CA a changé). La somme
  des barres redonne exactement l'écart. Le sous-onglet *Décomposition du
  pont* refait le calcul ligne par ligne.
- **Comptes qui expliquent l'écart** : lequel des comptes d'un poste est
  responsable.
- **Raboter la référence** (coché par défaut) : une période en cours (huit
  mois de 2026) est comparée aux **mêmes mois** de la référence (janvier-août
  2025), pas à l'année entière.

### Évolution

Un indicateur dans le temps : la **tendance**, avec sa zone habituelle (un
point rouge en sort défavorablement), et la **saisonnalité**, une courbe par
année.

### Comptabilité générale

Le compte de résultat, compte par compte, plusieurs périodes côte à côte, au
centime ; les soldes intermédiaires ; la première et la dernière période de
chaque compte.

### Écritures

Le grand livre, ligne par ligne, tel qu'Odoo le montre : recherche par texte,
dates, poste ou journal. Les pastilles de poste filtrent d'un clic. L'écart
débit/crédit doit être nul sur une sélection entière (il ne l'est pas sur une
sélection filtrée, qui coupe des pièces). *Exporter la sélection* donne un CSV
lisible par Excel. Les écritures datées de mois à venir (loyers,
amortissements programmés) y figurent, mais n'entrent dans aucun autre calcul.

### Projection

Pour un mois dont la comptabilité n'est pas complète (typiquement : les
rémunérations ne sont pas encore encodées), une estimation des postes
manquants :

| Poste | Estimé par |
|---|---|
| CA | les ventes de caisse du mois, recalées sur la comptabilité |
| Matières | le ratio matières / CA des mois de référence |
| Rémunérations | la paie, à défaut le pointage, à défaut le planning, recalés sur la comptabilité |
| Frais généraux | la médiane des mois de référence |

Chaque chiffre estimé porte « ≈ », sa méthode et sa base de calcul ; la carte
du bas dit, sur les mois connus, à quelle précision le modèle tombe. **Rien de
ce qui est calculé ici ne sort de cet onglet.**

## Astuces

- **Tout graphe s'agrandit** : l'icône en bas à droite d'une carte.
- **Les tableaux se trient** en cliquant l'en-tête, montants et dates dans le
  bon ordre.
- **Les infobulles** portent le détail : objectif, pourcentage, calcul d'une
  barre du pont, couverts réservés.
- **Un mois qui fait plonger une courbe** : allez voir dans Compta →
  Contrôle s'il est complet avant d'en tirer une conclusion.
- **Une alerte rouge du Contrôle** : suivez son lien, il ouvre les écritures
  en cause.
- **Sur téléphone** le dashboard s'installe comme une application (« Ajouter à
  l'écran d'accueil »).
