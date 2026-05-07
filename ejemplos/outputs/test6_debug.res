Error: 'TAG' ya está definido
EXPR = (λx.(λy.(x))) a ((λz.(z)) b)
SUST[z:=b] = (λx.(λy.(x))) a b
SUST[x:=a] = (λy.(a)) b
SUST[y:=b] = a
a
