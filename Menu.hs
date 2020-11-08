main = do
    putStrLn("")
    putStrLn("1.-Serie Fibonacci")
    putStrLn("2.-Numeros del 1-10")
    putStrLn("3.-Factorial")
    putStrLn("4.-Desaparece numeros")
    putStrLn("5.-Palindromos")
    putStrLn("6.-Menu Calculadora")
    putStrLn("7.-Salir")
    n<-getLine
    casos n

casos n=do
    case n of
        "1"-> datosFibonacci
        "2"-> numeros 0
        "3"-> datosFactorial
        "4"-> metInit [0..10]
        "5"-> palindromos
        "6"-> menuC
        "7"->putStrLn("Bye, Vuelva pronto")
        _-> do
            putStrLn("Opcion no disponible")
            main

{---------1---------------------------------------------------------------------------------------------------------------------------------------}
datosFibonacci = do
        putStrLn("Posicion de la Secuencia Fibonacci: ")
        n <- getLine
        print("El numero es: "++show(fibonacci(read n)))
        main
        
fibonacci n = 
    if n==0
        then 
        0
    else if n==1
        then
        1
    else
        fibonacci(n-1) + fibonacci(n-2)

{---------2---------------------------------------------------------------------------------------------------------------------------------------}
numeros n = do
    if n<=10
        then do
            print n
            numeros (n+1)
    else    
        main

{---------3---------------------------------------------------------------------------------------------------------------------------------------}
datosFactorial = do
        putStrLn("Numero para ver el factorial: ")
        n <- getLine
        print("El Factorial es: "++show(factorial(read n)))
        main
        
factorial n = 
    if n==0 
        then 
        1
    else 
        n* factorial (n-1)

{---------4---------------------------------------------------------------------------------------------------------------------------------------}
metInit l = do
    if null l
        then
            main
    else do
        print(l)
        metInit (init l)

{---------5---------------------------------------------------------------------------------------------------------------------------------------}
palindromos = do
    putStrLn("Palabra para revisar: ")
    c <- getLine
    let res = c==reverse c

    if res==True
        then do
            putStrLn("Es palindromo: SI")
            main
    else do
         putStrLn("Es Palindromo: NO")
         main

{---------6---------------------------------------------------------------------------------------------------------------------------------------}
menuC = do
    putStrLn("")
    putStrLn("a.-Suma")
    putStrLn("b.-Resta")
    putStrLn("c.-Multiplicacion")
    putStrLn("d.-Division")
    putStrLn("z.-Salir")
    n<-getLine
    casosC n

casosC n = do
    case n of
        "a"-> suma
        "b"-> resta
        "c"-> multi
        "d"-> divi
        "z"-> do
            putStrLn("Bye, Vuelva pronto")
            main
        _ -> do 
            putStrLn("Opcion no disponible")
            menuC

suma=do
    putStrLn ("Numero 1 es:")
    x <- getLine
    putStrLn ("Numero 2 es:")
    y <- getLine
    let equis = read x::Int
    let ye = read y::Int
    let resu= equis + ye
    putStrLn("Resultado: "++show resu)
    menuC

resta=do
    putStrLn ("Numero 1 es:")
    x <- getLine
    putStrLn ("Numero 2 es:")
    y <- getLine
    let equis = read x::Int
    let ye = read y::Int
    let resu= equis - ye
    putStrLn("Resultado: "++show resu)
    menuC

multi=do
    putStrLn ("Numero 1 es:")
    x <- getLine
    putStrLn ("Numero 2 es:")
    y <- getLine
    let equis = read x::Int
    let ye = read y::Int
    let resu= equis * ye
    putStrLn("Resultado: "++show resu)
    menuC

divi= do
    putStrLn ("Numero 1 es:")
    x <- getLine
    putStrLn ("Numero 2 es:")
    y <- getLine
    let equis = read x::Int
    let ye = read y::Int
    let resu= div equis ye
    putStrLn("Resultado: "++show resu)
    menuC