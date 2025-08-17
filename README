# cache_simulator

Esse é o simulador de cache desenvolvido para a cadeira de Arquitetura e Organização de Computadores II do curso de Engenharia de Computação da UFPel, ministrado pelos Professores Júlio Carlos Balzano de Mattos e Marcelo Schiavon Porto.

## Uso
No terminal, execute o programa com argumentos no seguinte formato:
`$ ./cache_simulator <<numsets:bitsporbloco:associatividade>+> <entrada>`
e.g.
Para simular com a entrada de 10000 endereços, cache L1 de 128 bytes e cache L2 de 1KiB:
`$ ./cache_simulator 128:8:2 512:16:1 Endereços/bin_10000.txt`



Por enquanto, suportamos apenas entradas em texto limpo, um endereço a ser acessado por linha.

## Compilando
Uma instalação do [Steel Bank Common Lisp](https://www.sbcl.org/) com o [quicklisp](https://www.quicklisp.org/beta/) configurado é pré-requisito obrigatório.

Basta executar o comando
`$ sbcl --load compile.lisp`
que a imagem lisp será transformada em um (pesadíssimo) executável, o `cache_simulator`.

