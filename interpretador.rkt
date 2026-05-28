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

;; -------------------------------------------------------------------------------------------------------------
;; Funciones Auxiliares

;; list-find-position: Busca la posición de un símbolo en una lista de símbolos.
;; sym: El símbolo a buscar.
;; los: La lista de símbolos.
;; Devuelve la posición del símbolo en la lista, o #f si no se encuentra.
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; list-index: Encuentra el índice del primer elemento en una lista que satisface un predicado.
;; pred: Predicado que se aplica a los elementos de la lista.
;; ls: La lista en la que se busca.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

#|
; -------------------------------------------------------------------------------------------------------------
; Función @divisio nEntera
; -------------------------------------------------------------------------------------------------------------
; @divisionEntera: Realiza una división entera (sin decimales) entre dos números.
; x: El numerador de la división.
; y: El denominador de la división.
; Devuelve el cociente entero de la división entre x e y. Si x es menor que y, retorna 0.
; Se utiliza la operación de resta recursiva para realizar la división entera.
;
; Ejemplo de uso:
;    evaluar @divisionEntera(141, 10) finEval
; Resultado: 14
 
  declarar-rec (
        @divisionEntera(@x, @y) =
        Si (@x < @y) {0}
        sino {(1 + evaluar @divisionEntera((@x ~ @y), @y) finEval)}
        ) {
       evaluar @divisionEntera(141, 10) finEval
  }
; -------------------------------------------------------------------------------------------------------------
; Función @sumarDigitos (Primera versión, sin añadidos adicionales en el lenguaje)
; -------------------------------------------------------------------------------------------------------------
; @sumarDigitos: Calcula la suma de los dígitos de un número dado.
; n: El número cuyo dígitos serán sumados.
; Devuelve la suma de los dígitos de n.
;
; Esta versión usa la función @divisionEntera creada anteriormente para descomponer el número y sumar sus dígitos.
;
; Ejemplo de uso:
;    evaluar @sumarDigitos(147) finEval
; Resultado: 12 (1 + 4 + 7)


  declarar-rec (
       @divisionEntera(@x, @y) =
       Si (@x < @y) {0}
        sino {(1 + evaluar @divisionEntera((@x ~ @y), @y) finEval)}
        
       @sumarDigitos(@n)=
           Si (@n < 10) {
              @n
           } sino {
              ((@n ~ (evaluar @divisionEntera(@n, 10) finEval * 10)) + evaluar @sumarDigitos(evaluar @divisionEntera(@n, 10) finEval) finEval)
           }
  ) {
     evaluar @sumarDigitos(147) finEval
  }
  
  
; Se plantearon distintas versiones de la función optimizadas con añadidos al lenguaje:
; 
; Función @sumarDigitos (Versión usando "quot", expandido al lenguaje)
; -------------------------------------------------------------------------------------------------------------
; @sumarDigitos: Calcula la suma de los dígitos de un número utilizando la función quotient de Racket.
; n: El número cuyo dígitos serán sumados.
; Devuelve la suma de los dígitos de n.
;
; Esta versión utiliza la operación "quot" para realizar la división entera en lugar de la función recursiva.
; Esta se implemento como apartado adicional en el lenguaje
; 
;
; Ejemplo de uso:
;    evaluar @sumarDigitos(147) finEval
; Resultado: 12 (1 + 4 + 7)


  declarar-rec (
  
        @sumarDigitos(@n)=
           Si (@n < 10) {
               @n
           } sino {
               ((@n ~ ((@n quot 10) * 10)) + evaluar @sumarDigitos((@n quot 10)) finEval)
           }
  ) {
     evaluar @sumarDigitos(147) finEval
  }

; Función @sumarDigitos (Versión optimizada con "quot" y "%" expandidos al lenguaje)
; -------------------------------------------------------------------------------------------------------------
; @sumarDigitos: Calcula la suma de los dígitos de un número utilizando "quot" y "remainder" de Racket.
; n: El número cuyo dígitos serán sumados.
; Devuelve la suma de los dígitos de n.
;
; Esta versión optimiza el cálculo usando "quot" para la división entera y "%" para obtener el residuo, es decir, el último dígito.
; Eston son añadidos adicionales al lenguaje implementados mediante funciones de racket 
;
; Ejemplo de uso:
;    evaluar @sumarDigitos(147) finEval
; Resultado: 12 (1 + 4 + 7)


  declarar-rec (
  
       @sumarDigitos(@n)=
           Si (@n < 10) {
              @n
          } sino {
               ((@n % 10) + evaluar @sumarDigitos((@n quot 10)) finEval)
           }
  ) {
     evaluar @sumarDigitos(147) finEval
  }


; -------------------------------------------------------------------------------------------------------------
; Función @fact (Cálculo factorial recursivo)
; -------------------------------------------------------------------------------------------------------------
; @fact: Calcula el factorial de un número dado.
; x: El número cuyo factorial se va a calcular.
; Devuelve el factorial de x.
;
; La función evalúa si el número es mayor que cero, en cuyo caso se multiplica recursivamente por el factorial del
; número anterior hasta llegar a 1.
;
; Ejemplo de uso:
;    evaluar @fact(5) finEval     ; Resultado: 120
;    evaluar @fact(10) finEval    ; Resultado: 3,628,800
; En factorial se expandio la función recursiva para permitir la evaluación de varias expresiones esto funciona
; para hacer varias evaluaciones a una función definida.

 declarar-rec (
               @fact(@x) = 
                    Si @x {(@x * evaluar @fact(sub1(@x)) finEval)}
                    sino {1}
                    )
 {evaluar @fact(5) finEval
         evaluar @fact(10) finEval}


; -------------------------------------------------------------------------------------------------------------
; Función @potencia (Cálculo de una base elevada a un exponente)
; -------------------------------------------------------------------------------------------------------------
; @potencia: Calcula la potencia de un número base elevado a un exponente dado.
; base: El número que se va a elevar.
; exponente: El número al que se eleva la base.
; Devuelve el resultado de base^exponente.
;
; La función evalúa si el exponente es igual a 1, en cuyo caso devuelve la base.
; De lo contrario, multiplica la base por la llamada recursiva con el exponente reducido en 1.
;
; Ejemplo de uso:
;    evaluar @potencia(2, 4) finEval    ; Resultado: 16 (2^4)

declarar-rec (
  @potencia(@base, @exponente) = 
	Si (@exponente == 1) {@base}
	sino {(@base * evaluar @potencia(@base, sub1(@exponente)) finEval)}
	)
{evaluar @potencia(2, 4) finEval}


; -------------------------------------------------------------------------------------------------------------
; Función @sumarRango (Suma de un rango de números)
; -------------------------------------------------------------------------------------------------------------
; @sumarRango: Suma los números en el rango de inicio a fin, inclusive.
; inicio: El número inicial del rango.
; fin: El número final del rango.
; Devuelve la suma de todos los números desde inicio hasta fin.
;
; La función evalúa si el número de inicio es igual al número final. Si es así, retorna el valor del fin.
; De lo contrario, suma el número de inicio al resultado de una llamada recursiva con add1 (incrementa el inicio).
;
; Ejemplo de uso:
;    evaluar @sumarRango(2, 5) finEval   ; Resultado: 14 (2 + 3 + 4 + 5)


 declarar-rec (
   @sumarRango(@inicio, @fin) = 
        Si (@inicio == @fin) {@fin}
         sino {(@inicio + evaluar @sumarRango(add1(@inicio), @fin) finEval)}
         )
 {evaluar @sumarRango(2,5) finEval}




; -------------------------------------------------------------------------------------------------------------
; Decorador Básico (Añadir un saludo a la salida de un procedimiento)
; -------------------------------------------------------------------------------------------------------------
; @integrantes: Procedimiento que devuelve el nombre de los integrantes ("JuanD_y_JuanM").
; @saludar: Procedimiento que toma como entrada otro procedimiento y retorna un saludo concatenado con el resultado
; de dicho procedimiento.
;
; La función evalúa el saludo concatenando "Hola_" con el resultado del procedimiento pasado como argumento.
;
; Ejemplo de uso:
;    evaluar @decorate() finEval    ; Resultado: "Hola_JuanD_y_JuanM"


 declarar (
   @integrantes = procedimiento() {"JuanD_y_JuanM"};
   @saludar = procedimiento(@aProc) {procedimiento() {("Hola_" concat evaluar @aProc() finEval)}};
 )  {
 declarar (
 @decorate = evaluar @saludar(@integrantes) finEval;) // añadio un declarar adicional para almacenar la variable
 {evaluar @decorate() finEval}}


; -------------------------------------------------------------------------------------------------------------
; Decorador Expandido (Añadir un saludo personalizado a la salida de un procedimiento con texto adicional)
; -------------------------------------------------------------------------------------------------------------
; @integrantes: Procedimiento que devuelve el nombre de los integrantes ("JuanD_y_JuanM").
; @saludar: Procedimiento que toma como entrada otro procedimiento y retorna un saludo concatenado con el resultado
; de dicho procedimiento.
; @decorate: Procedimiento que evalúa el decorador y añade texto adicional al saludo.
;
; La función evalúa el saludo concatenando "Hola_" con el resultado del procedimiento pasado como argumento y le añade
; texto adicional (en este caso, "EstudiantesFLP").
;
; Ejemplo de uso:
;    evaluar @decorate("EstudiantesFLP") finEval    ; Resultado: "Hola_JuanD_y_JuanMEstudiantesFLP"


 declarar (
   @integrantes = procedimiento() {"JuanD_y_JuanM"};
   @saludar = procedimiento(@aProc) {procedimiento() {("Hola_" concat evaluar @aProc() finEval)}};
 ) {
 declarar (
 @decorate = procedimiento(@Exp) {( evaluar evaluar @saludar(@integrantes) finEval() finEval concat @Exp)};)
 {evaluar @decorate("EstudiantesFLP") finEval}}



 ; Función para probar el lenguaje en su primera versión
 declarar-rec (
    @fact(@x) = 
      Si @x {(@x * evaluar @fact(sub1(@x)) finEval)}
       sino {1}
  )
 {evaluar @fact(6) finEval}




;; -------------------------------------------------------------------------------------------------------------
;; Sección de Pruebas
;; Las pruebas cubren el análisis sintáctico, la evaluación de expresiones, las primitivas, los condicionales y el manejo de ambientes.

;; Constructor: scan&parse
;; Qué hace: Convierte una cadena de texto en un programa sintáctico del intérprete.
;; Para qué sirve: Verifica que la gramática reconozca correctamente las expresiones válidas.
;; Ejemplos de prueba:
;; 1. (scan&parse "42") => un-programa válido con numero-lit
;; 2. (scan&parse "(1 + 2)") => un-programa válido con primapp-bin-exp
;; 3. (scan&parse "Si 1 { 2 } sino { 3 }") => un-programa válido con condicional-exp
;; 4. (scan&parse "declarar(@x = 1;) { @x }") => un-programa válido con variableLocal-exp

;; Constructor: eval-program
;; Qué hace: Evalúa un programa completo en el ambiente inicial.
;; Para qué sirve: Permite obtener el resultado final de una expresión del lenguaje.
;; Ejemplos de prueba:
;; 1. (eval-program (scan&parse "42")) => 42
;; 2. (eval-program (scan&parse "@a")) => 1
;; 3. (eval-program (scan&parse "\"hola\"")) => "hola"

;; Constructor: apply-primitive-bin
;; Qué hace: Aplica una primitiva binaria a dos valores evaluados.
;; Para qué sirve: Ejecuta operaciones como suma, resta, comparación y concatenación.
;; Ejemplos de prueba:
;; 1. (apply-primitive-bin 1 (prim-suma) 2) => 3
;; 2. (apply-primitive-bin 7 (prim-resta) 4) => 3
;; 3. (apply-primitive-bin "a" (prim-concat) "b") => "ab"
;; 4. (apply-primitive-bin 3 (prim-mayor) 2) => 1

;; Constructor: apply-primitive-un
;; Qué hace: Aplica una primitiva unaria a un valor.
;; Para qué sirve: Ejecuta operaciones como add1, sub1, neg y longitud.
;; Ejemplos de prueba:
;; 1. (apply-primitive-un (prim-add1) 3) => 4
;; 2. (apply-primitive-un (prim-sub1) 3) => 2
;; 3. (apply-primitive-un (prim-long) "hola") => 4

;; Constructor: valor-verdad?
;; Qué hace: Determina si un número representa verdadero o falso.
;; Para qué sirve: Se usa en condicionales para decidir qué rama evaluar.
;; Ejemplos de prueba:
;; 1. (valor-verdad? 1) => #t
;; 2. (valor-verdad? 0) => #f
;; 3. (valor-verdad? 5) => #t

;; Constructor: convert-num-bool-exp
;; Qué hace: Convierte un booleano a 1 o 0.
;; Para qué sirve: Representa comparaciones booleanas dentro del intérprete.
;; Ejemplos de prueba:
;; 1. (convert-num-bool-exp #t) => 1
;; 2. (convert-num-bool-exp #f) => 0

;; Constructor: init-env
;; Qué hace: Crea el ambiente inicial del intérprete.
;; Para qué sirve: Provee valores predefinidos para @a, @b, @c, @d y @e.
;; Ejemplos de prueba:
;; 1. (apply-env (init-env) '@a) => 1
;; 2. (apply-env (init-env) '@d) => "hola"

;; Constructor: extend-env
;; Qué hace: Extiende un ambiente con variables nuevas y sus valores.
;; Para qué sirve: Permite implementar variables locales y parámetros de procedimientos.
;; Ejemplos de prueba:
;; 1. (apply-env (extend-env '(@x) '(5) (init-env)) '@x) => 5
;; 2. (apply-env (extend-env '(@x @y) '(2 3) (init-env)) '@y) => 3

;; Constructor: extend-env-recursively
;; Qué hace: Crea un ambiente recursivo para procedimientos.
;; Para qué sirve: Permite definir funciones que se llaman a sí mismas.
;; Ejemplos de prueba:
;; 1. (apply-env (extend-env-recursively '(@f) '((@x)) '((@x)) (init-env)) '@f) => una cerradura

|#

;; Ejecuta el interpretador al correr la aplicación
(interpretador)