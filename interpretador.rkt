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