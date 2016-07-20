\\ AJY 2016-01-09
\\
\\ From George F Luger: ARTIFICIAL INTELLIGENCE, 4th Edition
\\ Pearson--Addison-Wesley, ISBN 0-201-64866-0
\\
\\ The basic linear associator follows the idea of the Hebbian
\\ ANN, with the learning constant set at C == 1.0.
\\ Therefore, the network weight initialization formula is as follows:
\\
\\ W = Y1 * X1 + Y2 * X2 + ... + Yt * Xt
\\
\\ where the vector pairs <Xi, Yi> are the training data.
\\
\\ Usage:
\\
\\ First load Ramil Farkshatov's defstruct package:
\\
\\ (load "defstruct.shen")
\\
\\ Secondly, load Ramil Farkshatov's FOR loop macro:
\\
\\ (load "for.shen")
\\
\\ Load the array aux functions package:
\\
\\ (load "array.shen")
\\
\\ After that, load and run this file:
\\
\\ (load "assc.shen")
\\ 
\\ and run the exercise functions.
\\
\\ Here we work in the Hamming space, ie. all variables in {-1, 1}.


\\ The vector associations <Xi, Yi>, to be stored in the
\\ linear associator network, Luger p. 455:


(set x-y (@v (@p (@v 1 -1 -1 -1 <>) (@v -1 1 1 <>))
             (@p (@v -1 -1 -1 1 <>) (@v 1 -1 1 <>))
             <>))


\\ Define the Linear Associator ANN
\\ Note by author: We could add one field to the defstruct--
\\ namely, the name of the ANN neuron


(defstruct assc
  (nr-inputs number)
  (inputs-vec vector)
  (weights-vec vector)
  (activation-level number)
  (treshold-function (number --> number))
  (neuron-output number))



\\ Calling (transfer-function N) where N is a neuron, is equivalent
\\ with firing the neuron N, ie. computing its output


(define transfer-function
  { assc --> number }
  N ->
    (let
      AL (activation-level N)
      _  (assc-activation-level-> N AL)
      F  (assc-treshold-function N)
      _  (assc-neuron-output-> N (F (assc-activation-level N)))
      (assc-neuron-output N)))


(define activation-level
  { assc --> number }
  N -> (activation-level-h N 1 0))


(define activation-level-h
  { assc --> number --> number --> number }
  N Counter Sum ->
    Sum where (> Counter (assc-nr-inputs N))
  N Counter Sum ->
    (activation-level-h
      N (+ 1 Counter)
        (+ Sum (* (<-vector (assc-inputs-vec N) Counter)
                  (<-vector (assc-weights-vec N) Counter)))))


\\ The assc treshold function -- the signum function (modified)
\\ This also is called bipolar linear tresholding.

(define treshold
  { number --> number }
  Activation -> 1 where (>= Activation 0)
  Activation -> -1)



\\ Define the linear associator network for the example in the Section
\\ 10.5.4. on Pages 452--456 of Luger.  Figure 10.21 on Page 455.
\\ The network has 4 inputs and 3 outputs: therefore, it has 3
\\ neurons, with each neuron having 4 inputs.

\\ Calculate the weight matrix of the linear associator network
\\ according to the definition formula:

\\ X1 -- X2 / Y pairs from Luger, Page 455:

(set y1x1 (outer-product (snd (<-vector (value x-y) 1))
                         (fst (<-vector (value x-y) 1))))

(set y2x2 (outer-product (snd (<-vector (value x-y) 2))
                         (fst (<-vector (value x-y) 2))))

(set w (array-sum (value y1x1) (value y2x2)))

(output-array (value w))

\\ Now make the association network ANN:

(set assc-ann (@v
                   (mk-assc
                       4
                       (vector 4)
                       (<-vector (value w) 1)
                       0
                       (function treshold)
                       0)
                   (mk-assc
                       4
                       (vector 4)
                       (<-vector (value w) 2)
                       0
                       (function treshold)
                       0)
                   (mk-assc
                       4
                       (vector 4)
                       (<-vector (value w) 3)
                       0
                       (function treshold)
                       0)

                   <>))




\\ ------------------------------------------------------------
\\ The function to test the computed linear associator ANN:
\\
\\ Result: 100% correct training as in the Luger textbook
\\

(define test-assc
  { --> (list A) }
  ->
    (let
      _ (output "~%Give a sequence of 4 Hamming integers: ")
      InL (lineread)
      _ (output "~%Numbers: ~A~%" InL)
      I (list2vec InL)
      _ (output "~%Vector: ~A~%" I)
      N1 (<-vector (value assc-ann) 1)
      N2 (<-vector (value assc-ann) 2)
      N3 (<-vector (value assc-ann) 3)
      _ (assc-inputs-vec-> N1 I)
      _ (assc-inputs-vec-> N2 I)
      _ (assc-inputs-vec-> N3 I)
      O1 (transfer-function N1)
      O2 (transfer-function N2)
      O3 (transfer-function N3)
      _ (output "~%~%Output of ANN: ~A~%" [O1 O2 O3])
      []))


\\ ------------------------------------------------------------
\\


(define list2vec
  { (list A) --> (vector A) }
  L ->
    (let
      Ln (length L)
      VEC (vector Ln)
      (list2vec-h L VEC Ln 1)))


(define list2vec-h
  { (list A) --> vector --> number --> number
     --> (vector A) }
  L VEC Ln Indx ->
    VEC where (> Indx Ln)
  L VEC Ln Indx ->
    (let
      _ (vector-> VEC Indx (hd L))
      (list2vec-h (tl L) VEC Ln (+ 1 Indx))))
