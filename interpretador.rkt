#lang eopl

;;--------------------------------------------------------------------------------------------------------------------------------------;;
;; Juan David Lopez Vanegas - 2243077                                                                                              ;;
;; Juan Manuel Moreno  -  
;; GitHub - https://github.com/Juanitotk25/Flp---taller-3.git
;;--------------------------------------------------------------------------------------------------------------------------------------;;

;-------------------------------------------------------------------------------------------------------------;
;;;; El interpretador ;;;;

;; <programa> :=  <expresion>
;;                un-programa (exp)

;; <expresion> := <numero>
;;                numero-lit (num)
;;             := <numero-negativo> ; extensión para definir números negativos
;;                "-" numero-lit (num) 
;;             := "\""<texto> "\""
;;                texto-lit (txt)
;;             := <identificador>
;;                var-exp (id)
;;             := (<expresion> <primitiva-binaria> <expresion>)
;;                primapp-bin-exp (exp1 prim-binaria exp2)
;;             := <primitiva-unaria> (<expresion>)
;;                primapp-un-exp (prim-unaria exp)
;;             := Si <expresion> "{" <expresion>  "}" "sino" "{" <expresion> "}"
;;                condicional-exp (test-exp true-exp false-exp)
;;             := declarar ({<identificador> = <expresion> ';' }*)) { <expresion> }
;;                variableLocal-exp (ids exps cuerpo)
;;             := procedimiento (<identificador>*(',') ) "{" <expresion> "}"
;;                procedimiento-ex (ids cuero)
;;             :=  "evaluar" expresion   (expresion *(",") )  finEval
;;                app-exp(exp exps)
;;             := declarar-rec  ({<identificador> ({<identificador>}*(,)) = <expresion>}*) { <expresion> }
;;                     <letrec-exp proc-names idss bodies bodyletrec>

;; <primitiva-binaria> := | + | ~ | / | quot | * | % | concat | > | < | >= | <= | != | == |
;; <primitiva-unaria> := | add1 | sub1 | neg |

;-------------------------------------------------------------------------------------------------------------;
;; Especificación léxica

;; Definición del escáner, el cual convierte el texto de entrada en tokens
;; que serán procesados por el parser.
(define scanner-spec-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("//" (arbno (not #\newline))) skip)
    (identifier
     ("@" letter (arbno letter digit)) symbol)
    (texto
     (letter (arbno (or letter digit "_"))) string)
    (number
     (digit (arbno digit)) number)
    (number
     (digit "." (arbno digit)) number)))

;; -------------------------------------------------------------------------------------------------------------
;; Especificación Sintática (gramática)

;; Definición de la gramática para el intérprete, que describe las reglas 
;; sobre cómo las expresiones pueden ser construidas y reconocidas.

(define grammar-interpreter
  ;; Progamas (formato por una expresión)
  '((program (expression) a-program)

    ;; Expresiones
    (expression (number) numero-lit)
    (expression ("-" number) neg-numero-lit) ; Negativo como una expresión adicional
    (expression ("\"" texto "\"") texto-lit)
    (expression (identifier) var-exp)
    (expression
     ("(" expression prim-binaria expression ")") primapp-bin-exp)
    (expression
     (prim-unaria "(" expression ")") primapp-un-exp)
    (expression
     ("Si" expression "{" expression "}" "sino" "{" expression "}") condicional-exp)
    (expression
     ("declarar" "(" (arbno identifier "=" expression ";") ")" "{" expression "}") variableLocal-exp)
    (expression
     ("procedimiento" "(" (separated-list identifier ",") ")" "{" expression "}") procedimiento-exp)
    (expression
     ("evaluar" expression "(" (separated-list expression ",") ")" "finEval") app-exp)
    (expression
     ("declarar-rec" "(" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) ")"  "{" (arbno expression)"}") 
                variableLocalRec-exp)

    ;; Primitiva binarias
    (prim-binaria ("+") prim-suma)
    (prim-binaria ("~") prim-resta)
    (prim-binaria ("/") prim-div)
    (prim-binaria ("quot") prim-ent-div)
    (prim-binaria ("*") prim-mult)
    (prim-binaria ("%") prim-mod)
    (prim-binaria ("concat") prim-concat)
    (prim-binaria (">") prim-mayor)
    (prim-binaria ("<") prim-menor)
    (prim-binaria (">=") prim-mayor-igual)
    (prim-binaria ("<=") prim-menor-igual)
    (prim-binaria ("!=") prim-diferente)
    (prim-binaria ("==") prim-igual)

    ;; Primitivas unarias
    (prim-unaria ("longitud") prim-long)
    (prim-unaria ("add1") prim-add1)
    (prim-unaria ("sub1") prim-sub1)
    (prim-unaria ("neg") prim-neg-bool)))

;; -------------------------------------------------------------------------------------------------------------
;; Tipos de datos para la sintaxis abstracta de la gramática

;; Se definen los tipos de datos (datatypes) para representar las construcciones sintácticas
;; de la gramática del intérprete, tales como literales numéricos, identificadores, 
;; primitivas binarias, unarias, y más.

(sllgen:make-define-datatypes scanner-spec-interpreter grammar-interpreter)

;; Función para mostrar los tipos de datos generados por el intérprete.
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-interpreter grammar-interpreter)))

;-------------------------------------------------------------------------------------------------------------;