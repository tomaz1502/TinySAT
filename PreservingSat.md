
Seja $\phi$ uma formula. Podemos adicionar uma clausula $C$ a $\phi$ preservando
satisfatibilidade se $C$ é uma conclusão lógica das clausulas em $\phi$. Isso
significa que não é possível que $\phi$ seja UNSAT e $\phi \cup C$ seja SAT.
Logo, se demonstrarmos que $\phi \cup C$ é UNSAT, podemos concluir
que $\phi$ também é UNSAT.

Note que também é impossível que $\phi$ seja SAT e $\phi \cup C$ seja UNSAT,
mas isso não é útil no nosso contexto.

A ideia é ir adicionando clausulas até chegar na clausula vazia,
que fará com que a fórmula seja trivialmente UNSAT.

As clausulas que eu quero atribuir a cada nó devem preservar
satisfatibilidade da formula original.

Além disso, para um nó $u$ com atribuição parcial $\Gamma_n$ ($n$ sendo a profundidade de
$u$, que corresponde ao tamanho do prefixo das variáveis que foi adicionada),
a clausula $C$ que vamos adicionar deve satisfazer:
1. $C$ só contém variáveis de $1$ a $n$ (ou seja, todas já foram atribuídas por $\Gamma$).
2. Nenhum literal em $C$ é satisfeito por $\Gamma_n$.

No caso das folhas é fácil pois o algoritmo encontrou uma clausula que já
existia na fórmula original que satisfaz as duas condições.

No caso do nó interno, devemos usar alguma combinação envolvendo
as cláusulas atribuídas aos nós na sua sub-árvore. Por indução, podemos
assumir que todas elas satisfazem (1) e (2).

Seja $v$ um nó interno, $v_l$ e $v_r$ seus filhos, $C_l$ e $C_r$ as clausulas atribuídas
para $v_l$ e $v_r$ respectivamente. Seja $n$ a profundidade de $v$.

(Nota: $\bowtie_{x}$ indica resolução sobre a variável $x$)
Se $x_{n + 1} \in C_l$ e $\overline{x_{n + 1}} \in C_r$, podemos atribuir $C_l \bowtie_{x_{n + 1}} C_r$ a $v$. Essa clausula
certamente é uma conclusão lógica das que já foram atribuídas (pela validade
da resolução). Além disso, por indução, $C_l$ e $C_r$ so contém variáveis de $1$ até $n + 1$.
Dessa forma, a resolução irá eliminar a variável $x_{n + 1}$, fazendo com que a clausula
resultante só contenha variáveis de $1$ até $n$. Finalmente, se $\Gamma_{n}$ satisfizesse algum
literal em $C_l \bowtie_{x_{n + 1}} C_r$, esse literal também seria necessariamente satisfeito em
$C_l$ ou em $C_r$, o que é um absurdo por indução. Note que $C_l \bowtie_{x_{n + 1}} C_r \subseteq C_l \cup C_r$.

Note que não é possível que $x_{n + 1} \in C_l$ e $x_{n + 1} \in C_r$, pois cada branch escolhe
um valor diferente para $x_{n + 1}$. Se o branch da direita escolhe um valor negativo,
$x_{n + 1}$ não poderia pertencer a $C_r$, pois a condição (2) seria violada em $C_r$.

Se $x_{n + 1} \not\in C_l$, simplesmente atribuímos $C_l$ para $v$.


