# Gramática em Forma normal de Chomsky

To do list:

- Todas as produções da gramática devem ter a forma A → BC ou A → a, onde A, B e C são símbolos não-terminais e a é um símbolo terminal.

(a varivel deriva em duas variaveis ou a veriavel deriva em um terminal)

- A gramática deve ter uma única regra inicial que seja um símbolo não-terminal.

- Não deve haver produções que produzem a cadeia vazia (ε), a menos que a regra inicial seja A → ε.
(a única variavel que pode derivar o vazio(ε) é a ínicial)

- Não deve haver produções unitárias, ou seja, produções da forma A → B, onde B é um símbolo não-terminal.

- Todas as produções devem ter exatamente dois símbolos não-terminais à direita, a menos que produzam um símbolo terminal ou a cadeia vazia.

- Todos os símbolos não-terminais da gramática devem ser alcançáveis a partir da regra inicial.

algoritmo:
    - Retirar o símbolo inicial do lado direito
    - Eliminar as produções "vazias" de baixo para cima
    - Eliminar as regras unitárias (que produzem apenas uma variaval) de cima para baixo
    - Substituir as regras que:
        - produzem variaval e terminais juntos
        - produzem mais de duas variaveis
        - produzem mais de um terminal