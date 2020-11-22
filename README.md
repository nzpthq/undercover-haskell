# undercover

## Description

Implémentation du jeu *Undercover* pour IRC. Ce robot n'est absolument pas sécurisé, à charge des modérateurs de prévenir l'anti-jeu.

## Prérequis

Le jeu nécessite une base de données de mots *words.db* (le nom est codé en dur dans la source). La base de données actuelle est privée (pour éviter les tricheries), mais n'hesitez pas à me contacter pour l'obtenir ! De plus, toute contribution est acceptée. Chaque ligne comporte une entrée constituée de plusieurs mots appartenants au même champs lexical, séparés par une espace.

## Compilation
Le code est packagé avec [stack](https://docs.haskellstack.org/en/stable/README/) et a été testé sur [NixOS](https://nixos.org/). La compilation et l'installation sont en théorie reproductibles et ne nécessitent que les commandes suivantes :

```
cd undercover-haskell
stack run
```

## Contact

irc.epiknet.org sur le salon #undercover !
