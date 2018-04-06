## Algoritmo de Djkstra implementado em Haskell


O presente trabalho foi desenvolvido para a disciplina de Paradigmas ministrada na Universidade de Brasília pela professora Milene Serrano, com o intuito de explorar o paradigma funcional de programação.

* Iasmin Santos Mendes, 14/0041940

### Dijkstra-Haskell

O programa está orgnaizado em 4 módulos: Node, Edge, Dijstra e Input. Os três primeiros módulos são responsáveis pelo funcionamento do algorítmo, e o último pode ser alterado para modificar o grafo executado conforme o seu interesse.

Para executar o programa, deve-se seguir os seguintes passos:

1. Clonar o repositório
1. Acessar a pasta do repositório no seu computador
1. Executar o comando **ghci** pelo terminal
1. Dentro do console do haskell, execute **:l Dijkstra.hs**
1. Uma vez que os módulos foram carregados, execute **dijkstra <nome-do-no-de-origem> <lista-de-nos> <grafo>** que será calculado o custo para todos os nós da rede a partir do nó de origem selecionado.
  1. Com os inputs de teste que já estão definidos no modulo Input, a execução seria: **dijkstra node_a nodes graph**
