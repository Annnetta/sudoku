;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname projet_sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t write repeating-decimal #f #t none #f () #f)))
; Hanna Parshuto
; Sudoku solver

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercice 1 - Gestion d'ensembles d'entiers

;Exercice 1-1)

; fonction qui cherche le minimum de liste
(define minimum; -> nombre minimal d'une liste
  (lambda (l); l: liste de nombres non vide
    (if (null? (cdr l)) (car l)
              (if (> (car l) (cadr l)) (minimum (cdr l))
                  (minimum (cons (car l) (cddr l)))))))    

;tests:
;(minimum '(2 5 9 6 1 2 5))-> 1
;(minimum '(1 2 5 7 0 8))-> 0
  

; fonction qui supprime element x de liste
(define supprime; -> la liste sans element qui est egal a x
  (lambda ( x l); x: element qu'il faut supprimer, l: liste des elements
    (if (null? l) l
        (if (eq? x (car l)) (cdr l)
            (cons (car l) (supprime x (cdr l))
                  )))))

;Tests:
;(supprime 5 '(1 2 4 5 8 7))->(1 2 4 8 7)
;(supprime 7 '(1 2 4 5 8 7))-> (1 2 4 5 8)


; fonction qui fait une liste triée 
(define trier;-> une liste de nbs triée par croissance
  (lambda (l); l:liste des nbs
    (if (null? l) l
        (cons (minimum l) (trier (supprime (minimum l) l)))))); il faut toujours construire element minimal d'une liste
                                                              ;au debut de la liste 
                                                              ;dans une liste triée sans element minimal
 
;Tests:
;(trier '( 5 7 9 6 4))-> (4 5 6 7 9)
;(trier '(1 11 58 64 22 10 1 2 3 4))-> (1 1 2 3 4 10 11 22 58 64)
    


;fonction qui supprime les doublons d'une liste triee des nbs
(define RetirerDoublon;-> liste triée des nbs sans répétitios
  (lambda (l); l: liste triée des nbs
    (if (null? (cdr l)) (list (car l))
        (let ((RD (RetirerDoublon (cdr l))))
        (if (equal? (car l) (cadr l)) RD
            (cons (car l) RD))))))

;tests:
;(RetirerDoublon '(1 1 2 3 4 10 11 22 58 64))-> (1 2 3 4 10 11 22 58 64)
;(RetirerDoublon '(1 1 2 3 4 4 4 5 5 7 8 8 9 10))-> (1 2 3 4 5 7 8 9 10)


; fonction qui renvoie ensemble a partir d'une liste de nbs
(define creerEnsemble;-> la liste des nbs triés sans répétitios
  (lambda (l); l: liste pas triée des nbs (la liste qcn des nbs)
    (RetirerDoublon (trier l))))

;tests:
;(creerEnsemble '(1 2 5 7 8 9 5 2 1 4 1 2 5 4 1 2))->(1 2 4 5 7 8 9)
;(creerEnsemble '(0 1 2 0 1 2 0 1 2 0 1 2 0 1 2))-> (0 1 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercice 1-2)

; fonction qui renvoie l’ensemble correspondant  à l'union  de  deux  ensembles
(define union; ->  l’ensemble correspondant  à l'union  de  deux  ensembles
  (lambda (ens1 ens2); ens1, ens2: deux ensembles
    (creerEnsemble (append ens1 ens2))))

;(union '(1 2 3 4 5 6) '( 2 4 5 7))->(1 2 3 4 5 6 7)
;(union '(1 2 3 4 5) '(1 2 3 4 5 7 8 10 11 12))-> (1 2 3 4 5 7 8 10 11 12)


; Exercice 1-3)

; fonction qui renvoie l’ensemble correspondant à l'intersection de deux ensembles
(define Intersection;-> ensemble avec les elements qui sont presents en deux ensembles
  (lambda (ens1 ens2); ens1, ens2 : deux ensembles
    (inter (trier (append ens1 ens2)))))

(define inter; -> ensemble avec les elements qui sont presents en deux ensembles 
  (lambda (l); liste construit de deux ensembles
    (if (null? (cdr l)) '()
        (if (equal? (car l) (cadr l)) (cons (car l)(inter (cdr l)))
            (inter (cdr l))))))

;tests:
; (Intersection '(1 2 3 4 5) '( 2 3))-> (2 3)
; (Intersection '(1 2 3 4 5) '(1 2 3 4 5 7 8 9))-> (1 2 3 4 5)



; Exercice 1-4)

; fonction qui renvoie l’ensemble correspondant à la difference de deux ensembles
(define Difference;-> ensemble avec les elements qui sont differents de deux ensembles( ens1 privé d'ens2)
  (lambda (ens1 ens2) ; ens1, ens2 : deux ensembles
    (supp (Intersection ens1 ens2) ens1)))

(define supp; -> la liste sans les elements de l1
  (lambda ( l1 l2);  l1: les elements qu'il faut supprimer, l2: liste des elements(ens1)
    (if (null? l1) l2
        (if (eq? (car l1) (car l2)) (supp (cdr l1) (cdr l2))
            (cons (car l2) (supp l1 (cdr l2))
                  )))))


;tests:
;(Difference '(1 2 3 4 5) '( 2 3))-> (1 4 5)
;(Difference '(1 2 3 4 5) '(1 2 3 4 5 7 8 9))-> ()
;(Difference '(1 2 3 4 5 6) '( 2 4 5 7))-> (1 3 6)


;ou

; fonction qui renvoie l’ensemble correspondant à la difference de deux ensembles
(define difference ; -> ensemble avec les elements qui sont differents de deux ensembles( ens1 privé d'ens2)
  (lambda (ens1 ens2 ) ; ens1, ens2 : deux ensembles
    (if (null? ens1) '()
        (if (null? ens2) ens1
        (if (eq? (car ens1) (car ens2))
            (difference (cdr ens1) (cdr ens2))
            (cons (car ens1) (difference (cdr ens1) ens2)))))))

;(difference '(1 2 3 4 5) '( 2 3))-> (1 4 5)
;(difference '(1 2 3 4 5) '(1 2 3 4 5 7 8 9))-> ()
;(difference '(1 2 3 4 5 6) '( 2 4 5 7))-> (1 3 6)



    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
  
; Exercice 2- Sudoku

; #1
  
; opération qui définit la variable, dont l'indificateur (nom) est 'grille'
; et valeur est tous les lignes du sudoku où les chiffres connus sont préservés
; et les cases vides sont remplacées par 0.

(define grille
               '((5 3 0 0 7 0 0 0 0)
                 (6 0 0 1 9 5 0 0 0)
                 (0 9 8 0 0 0 0 6 0)
                 (8 0 0 0 6 0 0 0 3)
                 (4 0 0 8 0 3 0 0 1)
                 (7 0 0 0 2 0 0 0 6)
                 (0 6 0 0 0 0 2 8 0)
                 (0 0 0 4 1 9 0 0 5)
                 (0 0 0 0 8 0 0 7 9)))

(define grille1
  '((1 0 0 4)
    (4 3 0 0)
    (0 0 2 1)
    (0 1 0 3)))


;fonction qui renvoie la taille de sudoku.
(define sudokuSize;-> nombre qui correspond a la taille de sudoku
  (lambda (s); s:sudoku
    (length (car s ))))

;(sudokuSize grille)-> 9
;(sudokuSize grille1)-> 4



;fonction qui renvoie la taille d'une region de sudoku
(define RegionSize;-> entier positif qui correspond a la taille d'une region du sudoku
  (lambda (s); s: sudoku
    (sqrt ( sudokuSize s))))

;(RegionSize grille)-> 3
;(RegionSize grille1)-> 2



;fonction qui renvoie la liste avec les candidats possible pour une case de la sudoku 
(define cand;-> liste de nbs
  (lambda (n); n: entier positif
    (if (= n 0 ) '()
        (cons n (cand (- n 1))))))

;tests
;(cand 15)-> (15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)



;fonction qui renvoie l'ensemble des candidats possible pour une case de sudoku
(define Candidats;-> ensemble de nbs
  (lambda (s); s:sudoku
      (trier(cand (sudokuSize s))))) ; appel chez fonction cand et triage des nombres dans une ordre croissant(avec fctn trier)

;test
;(Candidats grille)-> (1 2 3 4 5 6 7 8 9)
;(Candidats grille1)-> (1 2 3 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #2


; fonction qui renvoie des elements de i-eme ligne qui ne sont pas egaux a 0
(define ligne; -> ensemble des nombres 
  (lambda (i s); i: entier (1<= i <=sodokuSize)
    (supprime 0 (creerEnsemble (recup i s))))); fonction creerEnsemple donne les elements de la i-eme ligne
                                                    ;mais avec 0. C'est pourquoi on doit utiliser fonction supprime
                                                    ;pour supprimer 0.

;fonction permettant de recuperer le i-eme element d'une liste
(define recup; -> liste des nombres
  (lambda (i l); i: entier (1<= i <= sodokuSize) l:liste de nbs
    (if (null? l) l
        (if (= i 1) (car l)
            (recup (- i 1) (cdr l))))))

;tests pour recup
;(recup 5 '(1 5 4 3 2 1))-> 2
;(recup 2 grille)-> (6 0 0 1 9 5 0 0 0)

;(recup 3 grille1)-> (0 0 2 1)


; tests pour ligne
; (ligne 2 grille)-> (1 5 6 9)
; (ligne 9 grille)-> (7 8 9)
; (ligne 1 grille)-> (3 5 7)

;(ligne 3 grille1)-> (1 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #3

; fonction qui renvoie ensemble des elements de j-eme cologne sans 0
(define cologne; -> ensemble des nombres 
  (lambda (j s); j: entier (1<= j <=sudokuSize); s:sudoku
    (supprime 0 (creerEnsemble (clgn j s))))); 


;fonction permettant de recuperer le j-eme element de chaque sous-liste
(define clgn;-> liste de nbs
  (lambda (j l); ; j: entier (1<= j <=sudokuSize); l: liste de sous-listes de nbs
    (if (null? l) '()
    (cons (recup j (car l)) (clgn j (cdr l))))))


;tests pour clgn
;(clgn 2 '( (1 2) (1 2) (2 3) (4 5) (6 7)))-> (2 2 3 5 7)
;(clgn 3 grille)-> (0 0 8 0 0 0 0 0 0)
;(clgn 5 grille)-> (7 9 0 6 0 2 0 1 8)

;(clgn 2 grille1)-> (0 3 0 1)


;tests pour cologne
;(cologne 3 grille)-> (8)
;(cologne 5 grille)-> (1 2 6 7 8 9)

;(cologne 2 grille1)-> (1 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #4

;fonction qui détermine quelle région (indice de région) il faut choisir pour i-ème ligne et j-ème cologne

(define IndiceRegion;  -> entier (1<= n <=sudokuSize)qui renvoie numero de la region
 (lambda (i j s); i,j: entiers (1<= i,j <=sudokusize)(i-numero de la ligne (j-cologne)); s- sudoku
   (if ( and (= (modulo i (RegionSize s)) 0) (= (modulo j (RegionSize s)) 0))
               (Ind (quotient i (RegionSize s)) (quotient j (RegionSize s)) s)
   (if (= (modulo i (RegionSize s)) 0) (Ind (quotient i (RegionSize s)) ( + (quotient j (RegionSize s)) 1) s)
       (if (= (modulo j (RegionSize s)) 0) (Ind (+ 1 (quotient i (RegionSize s))) ( quotient j (RegionSize s)) s)
                     (Ind (+ 1 (quotient i (RegionSize s))) ( + 1 (quotient j (RegionSize s))) s))))))

(define Ind;-> entier (1<= n <=RegionSize)qui renvoie numero de la region
  (lambda (I J s);i,j: entiers (1<= I,J <= Regionsize)(i-numero de la ligne (j-cologne)) s- sudoku
    (+ J (* (RegionSize s) (- I 1)))))


;tests:
;(IndiceRegion 5 2 grille)-> 4
;(IndiceRegion 5 5 grille)-> 5
;(IndiceRegion 3 6 grille)-> 2
;(IndiceRegion 9 9 grille)-> 9
;(IndiceRegion 6 1 grille)-> 4

;(IndiceRegion 4 4 grille1)-> 4
;(IndiceRegion 1 4 grille1)-> 2

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #5

;fonction qui renvoie l'indice de la ligne et de la cologne de la premiere case de la region
(define RegionIndice;-> liste de deux nbs (i j), ou 1ere nombre - numero de la ligne, 2-eme - de la cologne
  (lambda (k s); k: Indice region; s:sudoku
    (if (= (modulo k (RegionSize s)) 0) (list (- (* (quotient k (RegionSize s)) (RegionSize s)) (- (RegionSize s) 1))
                                          ( - (sqr (RegionSize s)) (- (RegionSize s) 1)))
        (list (+ 1 (* (RegionSize s) (quotient k (RegionSize s))))
             (- (* (RegionSize s) (modulo k (RegionSize s))) (- (RegionSize s) 1))))))

;tests
;(RegionIndice 2 grille)-> (1 4)
;(RegionIndice 6 grille)-> (4 7)
;(RegionIndice 7 grille)-> (7 1)

;(RegionIndice 3 grille1)-> (3 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #6


;fonction qui renvoie l'ensemble des valeurs non-vide
(define Region;-> ensemble sans 0
  (lambda (k s); k: entier positif (numero de la region). s:sudoku
    (supprime 0; appel chez fction supprime qui supprime 0
              (creerEnsemble; appel chez fct creerEnsemble pour creer ensemble d'une liste de nbs
               (Reg1 (cadr (RegionIndice k s)); appel chez une fct Reg1 où:
                                            ;1er argument - numero de cologne de laquelle il faut commencer retirer les nombres
                     (+ (cadr (RegionIndice k s)) (- (RegionSize s) 1)); 2eme - numero de la cologne jusq'a laquelle il faut retirer les nbs(l'inclus)
                     (car (RegionIndice k s)); 3eme - numero de la ligne dans laquelle il faut commemcer retirer des nmbrs
                     0       ;4eme compteur
                     s))))); 5eme sudoku


; tests
;(Region 5 grille)-> (2 3 6 8)
;(Region 1 grille)-> (3 5 6 8 9)
;(Region 3 grille)-> (6)

;(Region 1 grille1)-> (1 3 4)


; fonction qui renvoie la liste de nombres qui se touvent dans une region 
(define Reg1; -> liste de nbs
  (lambda (n m x c s); n: numero de cologne de laquelle il faut commencer retirer les nombres
                   ; m: numero de la cologne jusq'a laquelle il faut retirer les nombres(l'inclus)
                   ; x: numero de la ligne dans laquelle il faut commemcer retirer des nmbrs
                   ; c: compteur
    (if (= c (RegionSize s)) '(); quand c sera egale a region de la sudoku, il n'y a plus besoin de retirer les nombres
    (append (elementsentre n m (recup x s)); appel chez fonction elementsentre qui recupere des nms de n à m
            (Reg1 n m (+ 1 x) (+ c 1) s))))); appel recursif ou on change la ligne (+ 1) et on augmente la compteur
          
;(Reg1 4 6 4 0 grille)-> (0 6 0 8 0 3 0 2 0)   = Region 5
;(Reg1 1 3 1 0 grille)-> (5 3 0 6 0 0 0 9 8)   = Region 1


; fonction renvoie les elements d'une liste  compris entre les indice n et m
(define elementsentre;-> liste de nmbs
  (lambda (n m l); n: indice des lequel il faut commencer retirer les elements
                 ; m: indice sur lequel il faut s'arreter
                 ; l: liste des elements
    (if (null? l) l
        (if (= n m) (list (car l))
        (if (= n 1) (cons (car l) (elementsentre n (- m 1) (cdr l)))
            (elementsentre (- n 1) (- m 1) (cdr l)))))))

;(elementsentre 3 5 '(1 2 3 4 5 6 7 8))-> (3 4 5)
;(elementsentre 3 7 '(1 2 3 4 5 6 7 8)) -> (3 4 5 6 7)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #7

;fonction qui a partir d'une case de sudoku definit par i et j donne les candidats possible pour ce cas
;d'ensemble construit de ligne i, cologne j, et region (i j)
(define Possibles;-> ensemble de nbs
  (lambda (i j s); i: numero de la ligne 1<= i<= SizeSudoku;   j:numero de la cologne  1<= j<= SizeSudoku ;s: sudoku
    (if (not (zero? (cellule i j s)));appel chez fct cellule qui renvoi le chiffre qui deja existe dans sudoku(0 ou 1-9) 
        (list (cellule i j s)); si fct cellule ne donne pas 0(càd la case n'est pas vide)
                                    ;il faut renvoyer la list du chiffre qui a renvoye fct cellule
                                    ;si fct cellule donne 0 (càd la case est vide)il faut faire appel chez fct Difference
        (Difference (Candidats s) ;qui prend comme des arguments: 1-ere - ensemble de tous les candidats im- et possibles    
                    (creerEnsemble (append (ligne i s) ;2eme- ensemble des elements IMpossibles (qui existe dans un ligne/region/cologne)
                              (cologne j s)      
                               (Region (IndiceRegion i j s) s)))
              )))) 


;tests: 
;(Possibles 5 2 grille)->(2 5)
;(Possibles 5 5 grille)-> (5)
;(Possibles 9 9 grille)-> (9) ---- case n'etait pas egale 0
;(Possibles 3 3 grille)-> (8) -----//--

;(Possibles 1 2 grille1)-> (2)


;fonction qui renvoi le chiffre qui est placé dans i-eme sous-liste (i-eme ligne)
;et sur j-eme place dans cette sous-liste (cologne j)    
(define cellule; -> nombre (de 0 à sizesudoku)
  (lambda (i j s); i:ligne de sudoku, j:cologne, l:liste de sous-listes (grille)
    (if (= i 1) (recup j (car s))
            (cellule (- i 1) j (cdr s)))))

;(cellule 5 4 grille)-> 8
;(cellule 6 1 grille)-> 7
;(cellule 9 9 grille)-> 9
;(cellule 5 5 grille)-> 0

;(cellule 1 1 grille1)->1
;(cellule 4 1 grille1)->0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #8


;fonction qui a partir de la grille donne nouveau sudoku,ou tous les elements sont remplaces par ensembles
;et apres si cet ensemble contient 1 element - l'ecrit, si plusieurs - 0 (1er fois resolution de sudoku)
(define solvestep;-> liste de sous-listes egales a sudokusize (sudoku) avec premier fois de resolution
  (lambda (s);-> liste de sous-listes egales a sudokusize (sudoku)
    (etape (place s 1 1 s)))); appel chez fct place donne sudoku, ou tous les elements sont remplaces par ensembles
                           ;1 et 1 - sont compteurs de i et j
                           ;appel chez fct etape renvoie sudoku sans ensembles poue chaque element
    
;(solvestep grille)->
;((5 3 0 0 7 0 0 0 0)
; (6 0 0 1 9 5 0 0 0)
; (0 9 8 0 0 0 0 6 0)
; (8 0 0 0 6 0 0 0 3)
; (4 0 0 8 5 3 0 0 1)
; (7 0 0 0 2 0 0 0 6)
; (0 6 0 0 0 7 2 8 4)
; (0 0 0 4 1 9 0 3 5)
; (0 0 0 0 8 0 0 7 9))



;fonction qui remplace 0 par les elements d'un ensemble ijk, calculant d'abord une place de 0 dans la liste (son i et j)
(define place;-> liste de sous-listes de nbs
  (lambda (l i j s); l: liste de sous-listes de nbs; i,j - numero de la ligne et cologne(et aussi compteurs);s :sudoku
    (if (null? l) l        
        (if (list? (car l)) (cons (place (car l) i j s) (place (cdr l) (+ 1 i) j s)); si 1er sous-ensemble est une liste 
                                                     ;il faut changer 0 par les ensembles des elem. possibles,
                                                     ;et construire cela dans la reste de la liste avec elements aussi changés
                                                     ;quand on change la ligne(sous-ensemble) - on ajoute 1 a i;
            (cons (Possibles i j s) (place (cdr l) i (+  1 j) s)))))); quand on change cologne
                                                    ;(o est dans la meme sous-ensemble) - on ajoute 1 a j, i ne change pas
            

;tests
;(place '((5 3 0) (6 0 0) (0 9 8)) 1 1 '((5 3 0) (6 0 0) (0 9 8)))-> (((5) (3) (1 2 4)) ((6) (2 4 7) (2 4 7)) ((1 2) (9) (8)))
; ou: (Possibles 1 3)-> (1 2 4)    (Possibles 2 2)-> (2 4 7)  (Possibles 2 3)-> (2 4 7)    (Possibles 3 1)-> (1 2)

;(place '((5 3 0) (6 0 0) (0 9 8) (8 0 0) (4 0 0) (7 0 0 ) (0 6 0 ) ( 0 0 0) (0 0 0)) 1 1 grille->
 ;(((5) (3) (1 2 4))
 ;((6) (2 4 7) (2 4 7))
 ;((1 2) (9) (8))
 ;((8) (1 2 5) (1 2 5 9))
 ;((4) (2 5) (2 5 6 9))
 ;((7) (1 5) (1 3 5 9))
 ;((1 3 9) (6) (1 3 4 5 7 9))
 ;((2 3) (2 7 8) (2 3 7))
 ;((1 2 3) (1 2 4 5) (1 2 3 4 5)));;;;pour region 1,4,7

;(place grille 1 1 grille)-> 
; (((5) (3) (1 2 4) (2 6) (7) (2 4 6 8) (1 4 8 9) (1 2 4 9) (2 4 8))
; ((6) (2 4 7) (2 4 7) (1) (9) (5) (3 4 7 8) (2 3 4) (2 4 7 8))
; ((1 2) (9) (8) (2 3) (3 4) (2 4) (1 3 4 5 7) (6) (2 4 7))
; ((8) (1 2 5) (1 2 5 9) (5 7 9) (6) (1 4 7) (4 5 7 9) (2 4 5 9) (3))
; ((4) (2 5) (2 5 6 9) (8) (5) (3) (5 7 9) (2 5 9) (1))
; ((7) (1 5) (1 3 5 9) (5 9) (2) (1 4) (4 5 8 9) (4 5 9) (6))
; ((1 3 9) (6) (1 3 4 5 7 9) (3 5 7) (3 5) (7) (2) (8) (4))
; ((2 3) (2 7 8) (2 3 7) (4) (1) (9) (3 6) (3) (5))
; ((1 2 3) (1 2 4 5) (1 2 3 4 5) (2 3 5 6) (8) (2 6) (1 3 4 6) (7) (9)))




;fonction qui reecrit les ensembles de sous listes ou il y a 1 element par cet element, ou il y a plusieurs - par 0 (pour plusieur sous-listes, comme dans sudoku)
(define etape;-> liste de sous-listes
  (lambda (l);-> liste de sous-listes de sous-listes
    (if (null? l) l
    (cons (transformation (car l)) (etape (cdr l))))))

;tests:
;(etape '(((1 3 9) (6) (1 3 4 5 7 9) (3 5 7) (3 5) (7) (2) (8) (4))
;((2 3) (2 7 8) (2 3 7) (4) (1) (9) (3 6) (3) (5))))-> ((0 6 0 0 0 7 2 8 4) (0 0 0 4 1 9 0 3 5))



;fonction qui reecrit les ensembles de la liste; si il y a 1 element- par cet element, si il y a plusieurs - par 0
(define transformation;-> liste de nbs
  (lambda (l);-> liste de sous-listes d'ensembles
    (if (null? l) l
        (if (= (length (car l)) 1) (append (car l) (transformation (cdr l)))
            (cons 0 (transformation (cdr l)))))))

;tests
;(transformation '((7) (1 5) (1 3 5 9) (5 9) (2) (1 4) (4 5 8 9) (4 5 9) (6)))-> (7 0 0 0 2 0 0 0 6)
;(transformation '((8) (1 2 5) (1 2 5 9) (5 7 9) (6) (1 4 7) (4 5 7 9) (2 4 5 9) (3)))-> (8 0 0 0 6 0 0 0 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; #9

;fonction qui resout le sudoku
(define Resoudre;-> sudoku resolu
  (lambda (s);s : sudoko qu'il faut resoudre
    (if (member? #true (existe s)) (Resoudre (solvestep s)); si il existe 0 dans n'importe quel case - appel recursive
        s))); sinon - renvoie le sudoku

    


;(Resoudre grille)->>
;((5 3 4 6 7 8 9 1 2)
; (6 7 2 1 9 5 3 4 8)
; (1 9 8 3 4 2 5 6 7)
; (8 5 9 7 6 1 4 2 3)
; (4 2 6 8 5 3 7 9 1)
; (7 1 3 9 2 4 8 5 6)
; (9 6 1 5 3 7 2 8 4)
; (2 8 7 4 1 9 6 3 5)
; (3 4 5 2 8 6 1 7 9))


; fonction qui verifie si il y a les 0 dans un sudoku
(define existe;-> liste de booleennes
  (lambda (s); s: sudoku
    (map (lambda (s) (member? 0 s)) s)))

;(existe        '((5 3 0 0 7 0 0 0 0)
;                 (6 0 0 1 9 5 0 0 0)
;                 (0 9 8 0 0 0 0 6 0)
;                 (8 0 0 0 6 0 0 0 3)
;                 (4 0 0 8 0 3 0 0 1)
;                 (7 0 0 0 2 0 0 0 6)
;                 (0 6 0 0 0 0 2 8 0)
;                 (0 0 0 4 1 9 0 0 5)
;                 (0 0 0 0 8 0 0 7 9)))-> (#true #true #true #true #true #true #true #true #true)


;(existe '((5 3 4 6 7 8 9 1 2)
; (6 7 2 1 9 5 3 4 8)
; (1 9 8 3 4 2 5 6 7)
; (8 5 9 7 6 1 4 2 3)
; (4 2 6 8 5 3 7 9 1)
; (7 1 3 9 2 4 8 5 6)
; (9 6 1 5 3 7 2 8 4)
; (2 8 7 4 1 9 6 3 5)
; (3 4 5 2 8 6 1 7 9)))-> (#false #false #false #false #false #false #false #false #false)


 ;test Resoudre pour une autre Sudoku

;(Resoudre   '((0 0 0 1 5 8 2 7 0)
;              (0 0 0 0 0 0 0 0 0)
;              (8 7 6 2 9 0 0 3 1)
;              (0 0 8 5 0 6 3 0 4)
;              (0 4 0 0 0 0 0 5 0)
;              (6 0 5 4 0 9 8 0 0)
;              (7 5 0 0 6 3 4 2 9)
;              (0 0 0 0 0 0 0 0 0)
;              (0 8 9 7 4 2 0 0 0)))
;((4 9 3 1 5 8 2 7 6)
; (5 1 2 6 3 7 9 4 8)
; (8 7 6 2 9 4 5 3 1)
; (1 2 8 5 7 6 3 9 4)
; (9 4 7 3 8 1 6 5 2)
; (6 3 5 4 2 9 8 1 7)
; (7 5 1 8 6 3 4 2 9)
; (2 6 4 9 1 5 7 8 3)
; (3 8 9 7 4 2 1 6 5))


; (Resoudre grille1)->
                     ;((1 2 3 4)
                      ;(4 3 1 2)
                      ;(3 4 2 1)
                      ;(2 1 4 3))





                 