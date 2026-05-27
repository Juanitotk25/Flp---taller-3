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
;; Parser, Scanner, Interfaz

;; El FrontEnd combina el análisis léxico y sintáctico para procesar un programa.
;; Es decir, toma una cadena de texto que representa el programa, lo pasa por el 
;; escáner (scanner) para obtener tokens, y luego por el parser para construir la 
;; estructura sintáctica de acuerdo a la gramática.
(define scan&parse
  (sllgen:make-string-parser scanner-spec-interpreter grammar-interpreter))

;; El Analizador Léxico (Scanner)
;; Esta función se encarga únicamente de la parte de escanear el código, es decir, 
;; convertir el código fuente en tokens, sin procesarlos.
(define just-scan
  (sllgen:make-string-scanner scanner-spec-interpreter grammar-interpreter))

;; El Interpretador (FrontEnd + Evaluación + señal para lectura )

;; El Interpretador combina el análisis léxico y sintáctico con la evaluación de expresiones.
;; Esta función realiza un ciclo de lectura-evaluación (REP loop), que permite 
;; ejecutar el código interactivo en línea, mostrando resultados a medida que se ejecutan las expresiones.
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      scanner-spec-interpreter
      grammar-interpreter)))

;-------------------------------------------------------------------------------------------------------------;
;;El Interprete

;; Esta función se encarga de evaluar un programa completo. Toma como entrada 
;; un programa (pgm) y lo evalúa dentro de un ambiente inicial, que es definido más abajo.

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

;; Ambiente inicial
;; init-env: Se define un ambiente inicial que asigna valores predeterminados a las variables
;; @a, @b, @c, @d, y @e. Estos valores pueden ser números o cadenas, y representan el entorno
;; en el cual las expresiones serán evaluadas al inicio de la ejecución del programa.
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;; eval-expression:
;; evalua la expresión en el ambiente de entrada.
;; Dependiendo del tipo de expresión (literal numérico, texto, variable, etc.), 
;; se aplican diferentes casos para evaluarla correctamente.
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ; Caso para literales numéricos
      (numero-lit (datum) datum)

      ; Caso para lgierales numéricos negativos
      (neg-numero-lit (datum) datum)

      ; Caso para literales de texto
      (texto-lit (textum) textum)

      ; Caso para variables  (identificadores)
      (var-exp (id) (apply-env env id))

      ; Caso para aplicaciones de primitivas binarias
      (primapp-bin-exp (rand1 prim rand2)
                       (let ((arg1 (eval-expression rand1 env))
                         (arg2 (eval-expression rand2 env)))
                         (apply-primitive-bin arg1 prim arg2)))

      ; Caso para aplicaciones de primitivas unarias
      (primapp-un-exp (prim rand)
                      (let ((arg (eval-expression rand env)))
                        (apply-primitive-un prim arg)))

      ; Caso para expresiones condicionales (Si - sino)
      (condicional-exp (text-exp true-exp false-exp)
                       (if (valor-verdad? (eval-expression text-exp env))
                           (eval-expression true-exp env)
                           (eval-expression false-exp env)))

       ; Caso para expresiones locales (declarar)
      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-rands exps env)))
                           (eval-expression cuerpo
                                            (extend-env ids args env))))

      ; Caso para expresiones de procedimiento (procedimiento)
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo env))

      ; Caso para aplicaciones de procedimientos (evaluar)
      (app-exp (exp exps)
               (let ((proc (eval-expression exp env))
                     (args (eval-rands exps env)))
                 (if (procval? proc)
                     (aplicar-procedimiento proc args)
                     (eopl:error 'eval-expresssion
                                 "Se esta aplicando algo que no es un procedimiento ~s" proc))))                                

      ; Caso para declaraciones recursivas (declarar-rec)
      (variableLocalRec-exp (proc-nombres ids cuerpos decl-cuerpo)
        (eval-expression-block decl-cuerpo
                         (extend-env-recursively proc-nombres ids cuerpos env)))
      (else 'expresión_no_reconocida))
    ))

;; -------------------------------------------------------------------------------------------------------------
;; Procedimientos
;; Definición del tipo de dato para procedimientos
;; procval: Define la estructura de los procedimientos (cerraduras)
;; Una cerradura es una función junto con el ambiente en el que fue definida.
;; Consta de una lista de identifiadores, el cuerpo de la expresión y el ambiente en el
;; que se definió
(define-datatype procval procval?
  (cerradura 
   (lista-ID (list-of symbol?))
   (exp expression?)
   (env environment?)))

;; aplicar-procedimiento: Aplica un procedimiento (cerradura) a una lista de argumentos.
;; proc: Procedimiento a aplicar (cerradura)
;; exps: Lista de argumentos que se pasan al procedimiento.
;; Evalúa el cuerpo del procedimiento en un ambiente extendido con los parámetros formales 
;; y sus valores correspondientes.
(define aplicar-procedimiento
  (lambda (proc exps)
    (cases procval proc
      (cerradura (lista-ID exp env)
                 (eval-expression exp (extend-env lista-ID exps env))))))

;-------------------------------------------------------------------------------------------------------------;
;; Funciones auxiliares para aplicar eval-expression a cada elemento de una lista de operandos (expresiones)

;; eval-rands: Evalúa cada expresión en una lista de operandos dentro del ambiente dado
;; rands: Lista de expresiones (operandos)
;; env: Ambiente en el cual se evaluarán las expresiones
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

;; eval-rand: Evalúa una única expresión en el ambiente dado
;; rand: Expresión a evaluar
;; env: Ambiente en el cual se evaluará la expresión
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))    

;; eval-expression-block: Evalúa una secuencia de expresiones en bloque
;; Si la lista de expresiones está vacía, devuelve '().
;; Si no, evalúa la cabeza de la lista y continúa evaluando el resto de la lista de forma recursiva.
(define eval-expression-block
  (lambda (expresiones env)
    (if (null? expresiones)
        '()
        (cons (eval-expression (car expresiones) env)
              (eval-expression-block (cdr expresiones) env)))))

;; -------------------------------------------------------------------------------------------------------------
;; Funciones para aplicar primitivas binarias

;; apply-primitive-bin: Aplica una operación binaria a dos argumentos
;; arg1: Primer argumento
;; prim: Operación binaria a aplicar
;; arg2: Segundo argumento
;; Se realiza la operación binaria dependiendo de la primitiva especificada.
(define apply-primitive-bin
  (lambda (arg1 prim arg2 )
    (cases prim-binaria prim
      (prim-suma () (+ arg1 arg2))
      (prim-resta () (- arg1 arg2))
      (prim-div () (/ arg1 arg2))
      (prim-ent-div () (quotient arg1 arg2)) ; Función dada por racket
      (prim-mult () (* arg1 arg2)) 
      (prim-mod () (remainder arg1 arg2))
      (prim-concat () (string-append arg1 arg2)) ; Función dada por racket
      (prim-mayor () (convert-num-bool-exp (> arg1 arg2)))
      (prim-menor () (convert-num-bool-exp (< arg1 arg2)))
      (prim-mayor-igual () (convert-num-bool-exp (>= arg1 arg2)))
      (prim-menor-igual () (convert-num-bool-exp (<= arg1 arg2)))
      (prim-diferente () (convert-num-bool-exp (not (equal? arg1 arg2))))
      (prim-igual () (convert-num-bool-exp (equal? arg1 arg2)))
      (else 'primarieta)))) ; Caso por defecto para una operación no reconocida

;; -------------------------------------------------------------------------------------------------------------
;; Funciones para aplicar primitivas unarias

;; apply-primitive-un: Aplica una operación unaria a un argumento
;; prim: Operación unaria a aplicar
;; arg: Argumento al que se aplica la operación unaria
(define apply-primitive-un
  (lambda (prim arg)
    (cases prim-unaria prim
      (prim-long () (string-length arg)) ; Función dada por racket
      (prim-add1 () (+ arg 1))
      (prim-sub1 () (- arg 1))
      (prim-neg-bool () (not arg))
      (else 'unarieta))))

;; -------------------------------------------------------------------------------------------------------------
;; Función auxiliar valor-verdad?

;; valor-verdad?: Determina si un valor dado corresponde a un valor booleano verdadero o falso.
;; Un valor es verdadero si no es cero, siguiendo la convención de ciertos lenguajes
(define valor-verdad?
  (lambda (x)
    (not (zero? x))))

;; convert-num-bool-exp: Convierte un valor booleano a 1 (verdadero) o 0 (falso).
;; Esta conversión es necesaria para representar las expresiones booleanas dentro del interpretador.
(define convert-num-bool-exp
  (lambda (x)
    (if (not x)
        0
        1)))

;; -------------------------------------------------------------------------------------------------------------
;; Definición del tipo de dato ambiente

;; environment: Tipo de dato que representa un ambiente.
;; Contiene tres variantes:
;; - empty-env-record: Un ambiente vacío.
;; - extended-env-record: Un ambiente extendido que contiene una lista de símbolos y valores.
;; - recursively-extended-env-record: Un ambiente recursivamente extendido, utilizado para procedimientos recursivos.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

;; scheme-value?: Función que valida los valores de los símbolos, siempre retorna #t ya que no se restringen tipos.
(define scheme-value? (lambda (v) #t))

;; -------------------------------------------------------------------------------------------------------------
;; Funciones para crear y manejar ambientes

;; empty-env: Crea un ambiente vacío.
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;; extend-env: Extiende un ambiente con una lista de símbolos y una lista de valores asociados.
;; syms: Lista de símbolos (variables).
;; vals: Lista de valores asociados a los símbolos.
;; env: Ambiente actual.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;; extend-env-recursively: Extiende un ambiente de manera recursiva para procedimientos.
;; proc-names: Nombres de los procedimientos.
;; idss: Lista de listas de identificadores de los procedimientos.
;; bodies: Cuerpos de los procedimientos.
;; old-env: El ambiente existente que se va a extender.
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;; -------------------------------------------------------------------------------------------------------------
;; Función apply-env: Buscar un símbolo en un ambiente

;; apply-env: Busca el valor asociado a un símbolo en el ambiente.
;; env: El ambiente en el que se busca el símbolo.
;; sym: El símbolo (variable) a buscar.
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "Error, la variable no existe" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))