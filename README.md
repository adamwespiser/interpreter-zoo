# Interpreter Zoo
Welcome to the Interpreter Zoo! A review of different interpreter/DSL schemes in
Haskell. Examples of different Monad schemes will discussed, their advantages,
disadvantages, and primary uses.  

Outline: 
1. Explcit parameter passing    
2. State monad evaluation    
3. Reader monad evalution    
4. monad transformer library    
5. Free Monad    
6. Extensible effects    


Evaluation Syntax: S-expression delimitted by parenthesis     
Evaluation Semnatics: Scheme w/  (let, lambda, apply) on (primitive int,
primitive bool, primitive list comprehension)     

Variable Scope Environment: Map of Text -> LispVal




Objective: A comparison of performance between Free Monad and mtl transformers

```Haskell
Free f a = { unFree :: f (Free f a) }
```

http://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern

# construction of free monad via individual functions
http://www.atamo.com/articles/free-monads-wont-detox-your-colon/

# two failed attempts at extensible effects
http://archive.is/K0R6y

# free monad benchmark 
https://github.com/feuerbach/freemonad-benchmark

# free monad benchmark write up
https://ro-che.info/articles/2014-06-14-extensible-effects-failed

# idris evaluation, explicit variable passing w/ state monad
https://github.com/idris-lang/Idris-dev/blob/ad1343827cb052d23196494cd24659d2a38c220a/src/Idris/Core/Evaluate.hs#L259

# Free Monad 
http://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern    
*note* there will need to be an additional 'Applicative' Instance declared for
(Free f).    


# purescript codegen, mtl style
https://github.com/purescript/purescript/blob/master/src/Language/PureScript/CodeGen/JS.hs


# Cofun with cofree comonads
http://dlaing.org/cofun/


