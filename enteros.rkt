;.----------------.  .----------------.  .----------------.  .----------------.  .----------------. 
;| .--------------. || .--------------. || .--------------. || .--------------. || .--------------. |
;| |   ______     | || |  _________   | || |     ______   | || |   _____      | || |    ______    | |
;| |  |_   __ \   | || | |_   ___  |  | || |   .' ___  |  | || |  |_   _|     | || |   / ____ `.  | |
;| |    | |__) |  | || |   | |_  \_|  | || |  / .'   \_|  | || |    | |       | || |   `'  __) |  | |
;| |    |  ___/   | || |   |  _|  _   | || |  | |         | || |    | |   _   | || |   _  |__ '.  | |
;| |   _| |_      | || |  _| |___/ |  | || |  \ `.___.'\  | || |   _| |__/ |  | || |  | \____) |  | |
;| |  |_____|     | || | |_________|  | || |   `._____.'  | || |  |________|  | || |   \______.'  | |
;| |              | || |              | || |              | || |              | || |              | |
;| '--------------' || '--------------' || '--------------' || '--------------' || '--------------' |
; '----------------'  '----------------'  '----------------'  '----------------'  '----------------' 
;
;                    .----------------.  .----------------.  .----------------. 
;                   | .--------------. || .--------------. || .--------------. |
;                   | |     ______   | || |  _______     | || |      __      | |
;                   | |   .' ___  |  | || | |_   __ \    | || |     /  \     | |
;                   | |  / .'   \_|  | || |   | |__) |   | || |    / /\ \    | |
;                   | |  | |         | || |   |  __ /    | || |   / ____ \   | |
;                   | |  \ `.___.'\  | || |  _| |  \ \_  | || | _/ /    \ \_ | |
;                   | |   `._____.'  | || | |____| |___| | || ||____|  |____|| |
;                   | |              | || |              | || |              | |
;                   | '--------------' || '--------------' || '--------------' |
;                   '----------------'  '----------------'  '----------------'  
;
; ============
; enteros.rkt
; ============
; Autores:  Susana Morales Sánchez
;           Álvaro Sanz Sanz
;
; Fecha:    08/05/2014
;
; Descripción Funcional: 
; Codificación de la artimetica en modulo r:
;       - reducción a representante canónico
;       - aritmética: suma, producto, resta, inverso (si existiera)
; Definicion operaciones de matrices 2x2 en Zp 
;       - suma y producto
;       - determinante
;       - decisión inversibilidad y cáculo inversa 
;       - potencias de matrices  
;*************************************************************************************************

; Booleanos. Son los únicos lambda-términos no currificados.

(define true (lambda (x y) x))

(define false (lambda (x y) y))

(define neg (lambda (x) (x false true)))
                         
(define and (lambda (x y) (x y false)))

(define or (lambda (x y) (x true y)))

; Pares ordenados:
              
(define par (lambda (x)
              (lambda (y)
                (lambda (f) (f x y)))))

(define primero (lambda (p) (p true)))

(define segundo (lambda (p) (p false)))

;;;;; Combinador de punto fijo Y:

(define Y
  (lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v)))))))

;;;;;; Orden en naturales y test de nulidad:

(define esmenoroigualnat (lambda (n)
                             (lambda (m)
                                (escero ((restanat n) m)))))
                         
(define esmayoroigualnat (lambda (n)
                            (lambda (m)
                               (escero ((restanat m) n)))))
                         
(define esmenornat (lambda (n)
                     (lambda (m)
                       (and ((esmenoroigualnat n) m) (noescero ((restanat m) n))))))

(define esmayornat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) (noescero ((restanat n) m))))))

(define esigualnat (lambda (n)
                     (lambda (m)
                       (and ((esmayoroigualnat n) m) ((esmenoroigualnat n) m)))))

(define escero (lambda (n)
                 ((n (lambda (x) false)) true)))

(define noescero (lambda (n)
                    (neg (escero n))))

; Aritmética natural. Se define también "comprobar" para poder hacer pruebas. Se definen algunos naturales para hacer comprobaciones. Se escriben en francés para distinguirlos de los enteros 
; que se escribirán en español.

(define zero (lambda (f)
               (lambda (x) x)))

(define sucesor (lambda (n)
                  (lambda (f)
                    (lambda (x)
                     (f((n f) x))))))

(define un (sucesor zero))

(define deux (sucesor un))

(define trois (sucesor deux))

(define quatre (sucesor trois))

(define cinq (sucesor quatre))

(define six (sucesor cinq))

(define sept (sucesor six))

(define huit (sucesor sept))

(define neuf (sucesor huit))

(define dix (sucesor neuf))

(define onze (sucesor dix))

(define douze (sucesor onze))

(define treize (sucesor douze))

(define quatorze (sucesor treize))

(define quinze (sucesor quatorze))

(define seize (sucesor quinze))

(define dix-sept (sucesor seize))

(define dix-huit (sucesor dix-sept))

(define dix-neuf (sucesor dix-huit))

(define vingt (sucesor dix-neuf))

;; Comprobar

(define comprobar (lambda (n)
                    ((n (lambda (x) (+ 1 x))) 0)))

;; Suma naturales

(define sumnat (lambda (n)
                 (lambda (m)
                   ((n (lambda (x) (sucesor x))) m))))

;; Producto naturales

(define prodnat (lambda (n)
                   (lambda (m)
                     (lambda (f)
                       (lambda (x) ((m (n f)) x))))))
                    
;; Predecesor y resta 

(define prefn (lambda (f)
                (lambda (p)
                  ((par (f (primero p))) (primero p)))))

(define predecesor (lambda (n)
                     (lambda (f)
                       (lambda (x)
                            (segundo ((n ((lambda (g)
                                             (lambda (p) ((prefn g) p))) f)) ((par x) x)))))))
                         
(define restanat (lambda (n)
                     (lambda (m)
                        ((m (lambda (x) (predecesor x))) n))))                                                 

;; Resto de la división euclídea. Si el divisor es cero, devuelve false.

(define restonataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                 (lambda (x)
                    ((((esmayoroigualnat x) m)  
                        (lambda (no_use)
                            (f ((restanat x) m))
                        )
                        (lambda (no_use)
                            x
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
))

(define restonat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((restonataux n) m))) zero))))

;; Cociente de la división euclídea. Al igual que el resto, devuelve false si se divide por cero.

(define cocientenataux
    (lambda (n)
        (lambda (m)
            ((Y (lambda (f)
                (lambda (x)
                    ((((esmayoroigualnat x) m)  
                        (lambda (no_use)
                            (sucesor (f ((restanat x) m)))  
                        )
                        (lambda (no_use)
                            zero
                        )
                    )
                        zero)    ; Pasa zero como argumento de no_use
                )
            ))
                n)  ; Pasa n como el valor inicial de x.
        )
    )
)

(define cocientenat (lambda (n)
                      (lambda (m)
                        (((escero m) (lambda (no_use) false) (lambda (no_use) ((cocientenataux n) m))) zero))))


;; Máximo común denominador:

(define mcdnat
    (lambda (n)
        (lambda (m)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((escero y)  
                       (lambda (no_use)
                            x
                        ) 
                       (lambda (no_use)
                            ((f y)((restonat x) y)) 
                        )
                        
                    )
                        zero)    ; Pasa zero como argumento de no_use
                ))
            ))
                n) ; Pasa n como el valor inicial de x.
          m)       ; Pasa m como el valor inicial de y.
    )
))

;;;;;; Definición de algunos enteros. Se codifican los enteros mediante pares de naturales: el par (m,n) es una representación de m-n. Es obvio que varios
;;;;;; pares codifican el mismo entero. Por ejemplo, (7,5)=(9,7). Por lo tanto, los enteros se definen como el conjunto cociente de NxN mediante la relación 
;;;;;; de equivalencia R dada por
;;;;;;
;;;;;;                     (m,n) R (m',n') si y solo si m-n=m'-n'

(define cero ((par zero) zero))

(define -uno ((par zero) un))

(define -dos ((par zero) deux))

(define -tres ((par zero) trois))

(define -cuatro ((par zero) quatre))

(define -cinco ((par zero) cinq))

(define -seis ((par zero) six))

(define -siete ((par zero) sept))

(define -ocho ((par zero) huit))

(define -nueve ((par zero) neuf))

(define -diez ((par zero) dix))

(define -once ((par zero) onze))

(define -doce ((par zero) douze))

(define -trece ((par zero) treize))

(define -catorce ((par zero) quatorze))

(define -quince ((par zero) quinze))

(define -dieciseis ((par zero) seize))

(define -diecisiete ((par zero) dix-sept))

(define -dieciocho ((par zero) dix-huit))

(define -diecinueve ((par zero) dix-neuf))

(define -veinte ((par zero) vingt))

(define uno ((par un) zero))

(define dos ((par deux) zero))

(define tres ((par trois) zero))

(define cuatro ((par quatre) zero))

(define cinco ((par cinq) zero))

(define seis ((par six) zero))

(define siete ((par sept) zero))

(define ocho ((par huit) zero))

(define nueve ((par neuf) zero))

(define diez ((par dix) zero))

(define once ((par onze) zero))

(define doce ((par douze) zero))

(define trece ((par treize) zero))

(define catorce ((par quatorze) zero))

(define quince ((par quinze) zero))

(define dieciseis ((par seize) zero))

(define diecisiete ((par dix-sept) zero))

(define dieciocho ((par dix-huit) zero))

(define diecinueve ((par dix-neuf) zero))

(define veinte ((par vingt) zero))

;;;;; Orden, valor absoluto y tests de nulidad, positividad y negatividad. 
;;;
;;; (m,n) > (m',n') si y solo si m+n' > m'+n e igual con el resto

(define esmayoroigualent (lambda (r)
                           (lambda (s)
                             ((esmayoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r)))))) 

(define esmenoroigualent (lambda (r)
                           (lambda (s)
                             ((esmenoroigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmayorent (lambda (r)
                           (lambda (s)
                             ((esmayornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esmenorent (lambda (r)
                           (lambda (s)
                             ((esmenornat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define esigualent (lambda (r)
                           (lambda (s)
                             ((esigualnat ((sumnat (primero r)) (segundo s))) ((sumnat (primero s)) (segundo r))))))

(define absoluto (lambda (r)
                    (((esmayoroigualnat (primero r)) (segundo r)) ((par ((restanat (primero r)) (segundo r))) zero) ((par ((restanat (segundo r)) (primero r))) zero))))

(define negativo (lambda (r)
                   ((esmenorent r) cero)))

(define positivo (lambda (r)
                   ((esmayorent r) cero)))

(define esceroent (lambda (r)
                     ((esigualnat (primero r)) (segundo r))))
                      
(define noesceroent (lambda (r)
                       (neg (esceroent r))))

;;;;; Reducción a representante canónico de la clase de equivalencia.

(define reducir (lambda (r)
                  (((esmayoroigualnat (primero r)) (segundo r)) 
                        ((par ((restanat (primero r)) (segundo r))) zero)
                        ((par zero) ((restanat (segundo r)) (primero r))))))

;;;;; Aritmética entera. La respuesta está siempre dada por el representante canónico de la clase de equivalencia. 

(define testenteros (lambda (r)
                      (- (comprobar (primero r)) (comprobar (segundo r)))))

(define sument (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat (primero r)) (primero s))) ((sumnat (segundo r)) (segundo s)))))))

(define prodent (lambda (r)
                  (lambda (s)
                    (reducir ((par ((sumnat ((prodnat (primero r)) (primero s))) ((prodnat (segundo r)) (segundo s))))
                          ((sumnat ((prodnat (primero r)) (segundo s))) ((prodnat (segundo r)) (primero s))))))))                       

(define restaent (lambda (r)
                   (lambda (s)
                     (reducir ((par ((sumnat (primero r)) (segundo s))) ((sumnat (segundo r)) (primero s)))))))

;; Lo siguiente reduce la división de enteros a división de naturales. Si m mayor o igual que 0 y n> 0, y si q y r son cociente y resto de la división de m entre n, se tiene
;;  m  = q       * n        + r
;;  m  = (-q)    * (-n)     + r
;; -m  = (-(q+1))* n        + (n-r)
;; -m  = (q+1)   * (-n)     + (n-r),
;; siempre y cuando el resto no sea cero. Cuando el divisor es cero, la función cocienteent devuelve false.

(define cocienteent_aux (lambda (r)
                          (lambda (s)
                            ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))

; Caso1: resto cero. Si m= q*n, entonces -m= (-q)*n, -m = q* (-n) y m= (-q)*(-n).

(define cocienteentaux-caso1 (lambda (r)
                               (lambda (s)
                                  ((or (and ((esmayoroigualent r) cero) (positivo s)) (and (negativo r) (negativo s))) ((par ((cocientenat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                                       ((par zero) ((cocientenat (primero (absoluto r))) (primero (absoluto s))))))))
                              
; Caso 2: resto no nulo

(define cocienteentaux-caso2 (lambda (r)
                                (lambda (s)
                                    (((esmayoroigualent r) cero) ((positivo s) ((par ((cocienteent_aux r) s)) zero) ((par zero) ((cocienteent_aux r) s)))
                                                                 ((positivo s) ((par zero) (sucesor ((cocienteent_aux r) s))) ((par (sucesor ((cocienteent_aux r) s))) zero))))))
; Cociente cuando no hay división por cero

(define cocienteentaux (lambda (r)
                         (lambda (s)
                           ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) ((cocienteentaux-caso1 r) s) ((cocienteentaux-caso2 r) s)))))

; Cociente considerando la división por cero

(define cocienteent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((cocienteentaux r) s))) zero))))

; Resto. Si se divide por cero, devuelve false

(define restoentaux1 (lambda (r)
                        (lambda (s)
                          ((or (and ((esmayoroigualent r) cero) (positivo s)) (and ((esmayoroigualent r) cero) (negativo s))) ((par ((restonat (primero (absoluto r))) (primero (absoluto s)))) zero)
                                                                                                           ((par ((restanat (primero (absoluto s)))((restonat (primero (absoluto r))) (primero (absoluto s))))) zero)))))

(define restoentaux (lambda (r)
                       (lambda (s)
                          ((escero ((restonat (primero (absoluto r))) (primero (absoluto s)))) cero ((restoentaux1 r) s)))))

(define restoent (lambda (r)
                      (lambda (s)
                        (((esceroent s) (lambda (no_use) false) (lambda (no_use) ((restoentaux r) s))) zero))))

;; Como mcd (r,s)=mcd(|r|,|s|), se tiene

(define mcdent (lambda (r)
                 (lambda (s)
                   ((par ((mcdnat (primero (absoluto r))) (primero (absoluto s)))) zero))))


;; PRACTICA.

;; Reducción a representante canónico.
;; ejemplos.- 
;;    ((reduce nueve) tres))
;;    (testenteros ((reduce nueve) tres)))
(define reduce (lambda (p)
                 (lambda (r)
                   ((restoent p) r))))

;; Operación aritmética Suma.
;; ejemplos.- 
;;    (((suma nueve) seis) tres)
;;    (testenteros (((suma nueve) seis) tres))
(define suma (lambda (p)
               (lambda (q)
                 (lambda (r)
                      ((reduce ((sument p) q)) r)))))

;; Operación aritmética Producto.
;; ejemplos.- 
;;    (((producto nueve) tres) cuatro)
;;    (testenteros (((producto nueve) tres) cuatro))
(define producto (lambda (p)
                    (lambda (q)
                      (lambda (r)
                        ((reduce ((prodent p) q)) r)))))

;; Operación aritmética Resta.
;; ejemplos.- 
;;    (((resta nueve) tres) cuatro)
;;    (testenteros (((resta nueve) tres) cuatro))
(define resta (lambda (r)
                    (lambda (s)
                      (lambda (t)
                        ((reduce ((restaent r) s)) t)))))


;; Estudio inversibilidad.
;; ejemplos.- 
;; ((esinversible nueve) dos)
;;    ((esinversible nueve) tres)

(define esinversible (lambda (p)
                       (lambda (r)
                         ((esigualent ((mcdent p) r)) uno))))



;;Calculo del inverso de "a" módulo "b"
;(x z) será (a 1)
;(y w)      (b 0)
;; ((inverso tres) siete)
(define inverso             
    (lambda (a)                          
        (lambda (b)
            (((((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                       (lambda(z)
                        (lambda(w)
                          (((neg((esinversible x)y))
                            (lambda (no_use) false)    ;si no es inversible devuelve false 
                            ((esceroent x)  
                             (lambda (no_use)
                              w     ;w será el inverso
                             )
                             ((esceroent y)  
                             (lambda (no_use)
                               z     ;z será el inverso
                             )                    
                             (((esmayorent x) y)        ;; x=((sument x) ((prodent y)  ((restaent cero) ((cocienteent x) y))) )
                       (lambda (no_use)                ;; z=((sument z) ((prodent w) ((restaent cero) ((cocienteent x) y))) )
                           ((((f ((sument x) ((prodent y)  ((restaent cero) ((cocienteent x) y))))) y) ((sument z) ((prodent w) ((restaent cero) ((cocienteent x) y))))) w)
                        )
                       (lambda (no_use)   ;; y=((sument y) ((prodent x)  ((restaent cero) ((cocienteent y) x))) )
                                          ;; w=((sument w) ((prodent z) ((restaent cero) ((cocienteent y) x))) )
                            ((((f x ) ((sument y) ((prodent x)  ((restaent cero) ((cocienteent y) x))) )) z) ((sument w) ((prodent z) ((restaent cero) ((cocienteent y) x))) ))) 
                        ))
                        )
                        )
                        cero)    ; Pasa cero como argumento de no_use
                ))))
            ))
                a) ; Pasa a como el valor inicial de x.
          b)       ; Pasa b como el valor inicial de y.
    uno) ; Pasa 1 como valor inicial de z
  cero) ; Pasa 0 como valor inicial de w
)))


;; Definición de una Matriz. Siendo a, b,c y d enteros.
(define matriz
  (lambda (a)
    (lambda (b)
      (lambda (c)
         (lambda (d)
           ((par ((par a) b)) ((par c) d)))))))


;; Lista una matriz.
;; (listmatriz M3)
(define listmatriz
  (lambda (m)
    (list (testenteros(primero (primero m))) (testenteros(segundo (primero m))) (testenteros(primero (segundo m))) (testenteros(segundo (segundo m))))))

;; Lista una matriz con formato ((f11 f12) (f21 f22))
;; (flistmatriz M3)
(define flistmatriz
  (lambda (m)
    (list (list (testenteros(primero (primero m))) (testenteros(segundo (primero m)))) (list (testenteros(primero (segundo m))) (testenteros(segundo (segundo m)))))))


;; Matrices de ejemplo.
(define M1  ((par ((par uno) dos)) ((par uno) uno)))

(define M4((par ((par uno) dos)) ((par tres) cuatro)))

(define M2 ((par ((par dos) tres)) ((par cuatro) cinco)))

(define M3 ((par ((par cuatro) cinco))  ((par dos) tres)))

(define M5 ((par ((par cinco) cero)) ((par cero) cero)))

(define M0 ((par ((par cero) cero)) ((par cero) cero)))

(define MI ((par ((par uno) cero)) ((par cero) uno)))


;;Matrices en Zp
;;Reducir matrices a Zp
;; (listmatriz ((reducematriz M3) tres))
(define reducematriz (lambda (m)
                       (lambda (p)
                         ((par ((par ((reduce (primero (primero m)))p)) ((reduce (segundo (primero m)))p)) ) ((par ((reduce (primero (segundo m)))p)) ((reduce (segundo (segundo m)))p)) ))))
                               

;; Suma de Matrices en Zp.
;; (listmatriz (((summat M1) M2) diez))
(define summat (lambda (r)
                   (lambda (s)
                     (lambda (p)
                       ((reducematriz((par ((par((sument (primero (primero r))) (primero (primero s)))) ((sument (segundo (primero r))) (segundo (primero s)))))
                             ((par((sument (primero (segundo r))) (primero (segundo s)))) ((sument (segundo (segundo r))) (segundo (segundo s)))))) p)))))


;; Producto de Matrices en Zp.
;; (listmatriz (((prodmat M1) M2) tres))  
(define prodmat (lambda (r)
                    (lambda (s)
                      (lambda (p)
                       ((reducematriz ((par ((par ((sument ((prodent (primero (primero r))) (primero (primero s)))) ((prodent (segundo (primero r))) (primero (segundo s))))) 
                                           ((sument ((prodent (primero (primero r))) (segundo (primero s)))) ((prodent (segundo (primero r))) (segundo (segundo s))))))
                             ((par ((sument ((prodent (primero (segundo r))) (primero (primero s)))) ((prodent (segundo (segundo r))) (primero (segundo s))))) 
                                           ((sument ((prodent (segundo (primero r))) (segundo (primero s)))) ((prodent (segundo (segundo r))) (segundo (segundo s))))))) p)
                        ))))

;; Determinante en Zp
;; (testenteros ((detmat M2) tres))
(define detmat (lambda (r)
                   (lambda (p)
                     ((reduce((restaent ((prodent (primero (primero r))) (segundo (segundo r)))) ((prodent (primero (segundo r))) (segundo (primero r))))) p))))
                     
;; Decisión sobre inversibilidad de una matriz
;;((esinversiblemat M1) tres)
;; ((esinversiblemat ((((matriz cuatro)dos)uno)tres)) siete)
;; ((esinversiblemat ((((matriz cuatro)dos)uno)tres)) quince)
(define esinversiblemat (lambda (r)
                          (lambda (p)
                            (and (noesceroent((detmat r)p)) ((esigualent ((mcdent ((detmat r)p)) p)) uno)))))

;; Cálculo del rango de una matriz
;; (testenteros ((rango M5)tres))
;; (testenteros ((rango M1)tres))
;; (testenteros ((rango M0)tres))
(define rango (lambda (r)
                (lambda (p)
                   ((noesceroent ((detmat r) p)) dos ((and (and (esceroent (primero(primero r))) (esceroent (primero(segundo r))))
                                                                   (and (esceroent (segundo(primero r))) (esceroent (segundo(segundo r)))))
                                                                     cero
                                                                     uno) )  )))
                                   

;; Cálculo de inversa de una matriz en Zp
;; siendo la matriz r =(a b)  r-1 = 1/((detmat r)p) (d  -b)
;;                     (c d)                        (-c  a)

;; (listmatriz((matrizinversa ((((matriz cuatro)dos)uno)tres)) siete))

(define matrizinversa             
    (lambda (r)                          
        (lambda (p)
            (((neg((esinversiblemat r)p)) (lambda (no_use) false) ;;si no es inversible devuelve false
                   (lambda (no_use) ((reducematriz ((par ((par ((prodent ((inverso ((detmat r) p)) p)) (segundo (segundo r)))) ((prodent ((inverso ((detmat r) p)) p)) ((restaent cero) (segundo(primero r))))))
                                                         ((par ((prodent ((inverso ((detmat r) p)) p)) ((restaent cero) (primero (segundo r))))) ((prodent ((inverso ((detmat r) p)) p)) (primero(primero r)))))) p))
            ) cero) ;pasa cero como argumento de no_use
        )))
                                          
                                 

;; Cálculo de potencias (naturales) de matrices. 
;; Este cálculo se tiene que hacer usando el algoritmo binario para el cálculo de potencias, también conocido como exponenciación binaria.
;; Siendo r =(a b)
;;           (c d),se calcula la potencia n de la matriz r en Zp

;;(listmatriz (((potenciamatriz M1) cinq) veinte))
;;(flistmatriz (((potenciamatriz M1) zero) veinte))
;;(flistmatriz (((potenciamatriz MI) cinq) veinte))
;;(flistmatriz (((potenciamatriz M1) deux) veinte))

(define potenciamatriz             
    (lambda (r)                          
        (lambda (n)
          (lambda (p)
            ((((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                       (lambda (z)
                         (((escero y)
                            (lambda (no_use) MI) 
                          (((esigualnat y) un)
                            (lambda (no_use) ((reducematriz x) z))    ;si n es 1 devuelve r en Zp
                            ((escero ((restonat y)deux))   ; si n es par:
                             (lambda (no_use)               ; r potencia n/2 por r potencia n/2
                               (((prodmat (((f x ) ((cocientenat y) deux)) z)) (((f x ) ((cocientenat y) deux)) z)) z)
                              )                    
                             (lambda (no_use) ;; si n es impar: r por r potencia n-1
                               (((prodmat x) (((f x ) ((restanat y) un)) z)) z) 
                              )
                          )))
                        cero)    ; Pasa cero como argumento de no_use
                ))
            )))
                r) ; Pasa r como el valor inicial de x.
          n)       ; Pasa n como el valor inicial de y.
    p) ; Pasa p como valor inicial de z.
))))
