# Sistema de Tableaux para a Lógica Clássica Proposicional

Este projeto implementa um sistema de Tableaux para a Lógica Clássica Proposicional em Haskell. O objetivo é determinar a validade de uma fórmula proposicional dada, construindo uma árvore de prova ou refutação, seguindo regras lógicas específicas.

## Visão Geral

Um tableau é uma árvore com uma raiz rotulada por uma fórmula na Lógica Clássica Proposicional. Os nós dessa árvore são subfórmulas da raiz e são rotulados como "verdadeiro" ou "falso". A partir da raiz, novas fórmulas são criadas aplicando regras especificadas, gerando um ou dois novos ramos. Trata-se de uma prova por refutação: supõe-se que a fórmula de entrada é falsa e, caso haja contradições (ou seja, a mesma fórmula rotulada como verdadeira e falsa em nós diferentes de um mesmo ramo) em todos os ramos da árvore, a fórmula da raiz é uma tautologia (ou seja, toda tentativa de torná-la falsa resulta em uma contradição). Se todas as regras possíveis forem aplicadas e houver um ou mais ramos sem contradições, a fórmula da raiz é falsificável (um ramo é um caminho da raiz até uma folha).

## Entrada e Saída

- **Entrada:** Uma fórmula na Lógica Clássica Proposicional.
- **Saída:** Uma árvore de prova/refutação indicando se a fórmula é ou não válida.

## Regras

O sistema segue estas regras:

1. **v: a -> b ==> f: a / v: b**
2. **f: a -> b ==> v: a ; f: b**
3. **v: a & b ==> v: a ; v : b**
4. **f: a & b ==> f: a / f : b**
5. **v: a | b ==> v: a / v: b**
6. **f: a | b ==> f: a ; f: b**
7. **v: ~a ==> f: a**
8. **f: ~a ==> v: a**

### Notação

- **"/"** indica ramos diferentes.
- **";"** indica que a fórmula está no mesmo ramo.

### Aplicação das Regras

- Cada fórmula precisa ter uma regra aplicada uma única vez.
- Se uma fórmula A está no caminho até a raiz de duas ou mais folhas (e.g., B e C) e ainda não houve aplicação de regra sobre A, então caso uma regra seja aplicada em A, a conclusão deverá ser aplicada em todas as folhas (e.g., em B e C).
