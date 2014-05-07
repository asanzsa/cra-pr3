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


;; ###########################
;; COMPROBAR A PARTIR DE AQUÍ.
;; ###########################

;; Estudio inversibilidad.
;; ejemplos.- 

;;    ((esinversible nueve) tres)

(define esinversible (lambda (p)
                       (lambda (r)
                         ((esigualent ((mcdent p) r)) uno))))


; algoritmo de euclides extendido.
;http://www.dc.uba.ar/materias/algI/2013/cuat2/Material/Clase%2010
;http://rinconmatematico.com/foros/index.php?topic=26742.0
;http://www.dma.fi.upm.es/java/matematicadiscreta/aritmeticamodular/congruencias.html#3



;; Definición de una Matriz. Siendo a y b enteros (anteriormente definimos un entero como un par de naturales)
(define matriz
  (lambda (a)
    (lambda (b)
      ((par a) b)))
)

;; comprueba si la matriz es correcta.
(define testmatrices (lambda (r)
                       (- (testenteros (primero r)) (testenteros (segundo r))))
)



;; Matrices de ejemplo.

(define MD1 ((par ((par cinq) un)) ((par deux) trois)))

(define MD2 ((par ((par deux) trois)) ((par cinq) un)))

(define M1 ((par dos) uno)
)
(define M2 ((par tres) uno)
)
(define M3 ((par cuatro) uno)
)
(define M4 ((matriz cinco) uno)
)
(define M5 ((matriz seis) uno)
)
(define M6 ((matriz siete) uno)
)
(define M7 ((matriz ocho) uno)
)
(define M8 ((matriz nueve) uno)
)
(define M9 ((matriz diez) uno)
)
(define M12 ((matriz veinte) ocho)
)

;; Suma de Matrices en Zp.

(define summat (lambda (r)
                   (lambda (s)
                     (lambda (p)
                       ((par ((reduce ((sument (primero r)) (primero s))) p)) ((reduce((sument (segundo r)) (segundo s))) p) )))))


;; Producto de Matrices en Zp.
(define prodmat (lambda (r)
                    (lambda (s)
                      (lambda (p)
                       ((par ((reduce((par ((sumnat ((prodnat (primero (primero r))) (primero (primero s)))) ((prodnat (segundo (primero r))) (primero (segundo s))))) 
                                           ((sumnat ((prodnat (primero (primero r))) (segundo (primero s)))) ((prodnat (segundo (primero r))) (segundo (segundo s)))))) p))
                             ((reduce((par ((sumnat ((prodnat (primero (segundo r))) (primero (primero s)))) ((prodnat (segundo (segundo r))) (primero (segundo s))))) 
                                           ((sumnat ((prodnat (segundo (primero r))) (segundo (primero s)))) ((prodnat (segundo (segundo r))) (segundo (segundo s)))))) p))
                        ))))
;; Determinante en Zp

(define detmat (lambda (r)
                   (lambda (p)
                     ((reduce(reducir((par ((prodnat (primero (primero r))) (segundo (segundo r)))) ((prodnat (primero (segundo r))) (segundo (primero r)))))) p))))
                     
;; Decisión sobre inversibilidad y cálculo de inversa y del rango.

(define esinversiblemat (lambda (r)
                          (lambda (p)
                            (and (noesceroent((detmat r)p)) ((esigualent ((mcdent ((detmat r)p)) p)) uno)))))


;;Calculo del inverso por fuerza bruta
(define inverso
    (lambda (s)
        (lambda (r)
            (((Y (lambda (f)
                   (lambda (x)
                     (lambda(y)
                      (((esceroent y)  
                       (lambda (no_use)
                            cero     ;devuelve cero si no tiene inverso
                        )
                       ((esigualent ((restoent ((prodent x) y)) y)) un) 
                       (lambda (no_use)
                            y
                        )
                       (lambda (no_use)
                            ((f x)((restaent y) un)) 
                        )
                        
                    )
                        cero)    ; Pasa cero como argumento de no_use
                ))
            ))
                s) ; Pasa s como el valor inicial de x.
          r)       ; Pasa r como el valor inicial de y.
    )
))

;; Cálculo de potencias (naturales) de matrices. 
;; Este cálculo se tiene que hacer usando el algoritmo binario para el cálculo de potencias, también conocido como exponenciación binaria.


