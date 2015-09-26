(ns sicp.pair
  (:require
   [clojure.test :refer [is are deftest]]
   [clojure.core.typed
    :refer [All
            Any
            Atom1
            Atom2
            EmptySeqable
            IFn
            Int
            NonEmptySeqable
            Option
            Pred
            Rec
            Seqable
            TFn
            U
            Var1
            ann
            ann-protocol
            defalias
            letfn>
            loop
            ]
    :as typed
    ]))


(ann ^:no-check clojure.test/test-var [(Var1 [-> nil]) -> nil])


(typed/defprotocol
    [[a :variance :covariant]
     [b :variance :covariant]]
  IPair
  (set-car! [p :- (IPair a b) x :- a] :- Any)
  (set-cdr! [p :- (IPair a b) x :- b] :- Any)
  (car [p :- (IPair a b)] :- a)
  (cdr [p :- (IPair a b)] :- b)
  )


(typed/ann-datatype
 [[a :variance :covariant]
  [b :variance :covariant]]
 Pair
 [_car :- (Atom1 a)
  _cdr :- (Atom1 b)]
 :unchecked-ancestors #{(IPair a b)}
 )


(declare pair?)
(deftype Pair [_car _cdr]
 IPair
 (set-car! [self x] (reset! _car x))
 (set-cdr! [self x] (reset! _cdr x))
 (car [self] @_car)
 (cdr [self] @_cdr)
 java.util.Map$Entry
 (getKey [self] (car self))
 (getValue [self] (cdr self))
 Object
 (toString [self]
   (letfn> [impl :- [Any -> String]
            (impl [p]
                  (if (pair? p)
                    (str " " (car p) (impl (cdr p)))
                    (if (nil? p)
                      ""
                      (str " . " p))))]
     (str "("
          (car self)
          (impl (cdr self))
          ")")))
 (equals [self other]
   (and (pair? other)
        (= (car self) (car other))
        (= (cdr self) (cdr other))))
)

(ann caar (All [a] [(Pair (Pair a Any) Any) -> a]))
(ann cadr (All [a] [(Pair Any (Pair a Any)) -> a]))
(ann cdar (All [a] [(Pair (Pair Any a) Any) -> a]))
(ann cddr (All [a] [(Pair Any (Pair Any a)) -> a]))
(defn caar [p] (-> p car car))
(defn cadr [p] (-> p cdr car))
(defn cdar [p] (-> p car cdr))
(defn cddr [p] (-> p cdr cdr))

(ann caaar (All [a] [(Pair (Pair (Pair a Any) Any) Any) -> a]))
(ann caadr (All [a] [(Pair Any (Pair (Pair a Any) Any)) -> a]))
(ann cadar (All [a] [(Pair (Pair Any (Pair a Any)) Any) -> a]))
(ann caddr (All [a] [(Pair Any (Pair Any (Pair a Any))) -> a]))
(defn caaar [p] (-> p car car car))
(defn caadr [p] (-> p cdr car car))
(defn cadar [p] (-> p car cdr car))
(defn caddr [p] (-> p cdr cdr car))
(ann cdaar (All [a] [(Pair (Pair (Pair Any a) Any) Any) -> a]))
(ann cdadr (All [a] [(Pair Any (Pair (Pair Any a) Any)) -> a]))
(ann cddar (All [a] [(Pair (Pair Any (Pair Any a)) Any) -> a]))
(ann cdddr (All [a] [(Pair Any (Pair Any (Pair Any a))) -> a]))
(defn cdaar [p] (-> p car car cdr))
(defn cdadr [p] (-> p cdr car cdr))
(defn cddar [p] (-> p car cdr cdr))
(defn cdddr [p] (-> p cdr cdr cdr))

(ann caaaar (All [a] [(Pair (Pair (Pair (Pair a Any) Any) Any) Any) -> a]))
(ann caaadr (All [a] [(Pair Any (Pair (Pair (Pair a Any) Any) Any)) -> a]))
(ann caadar (All [a] [(Pair (Pair Any (Pair (Pair a Any) Any)) Any) -> a]))
(ann caaddr (All [a] [(Pair Any (Pair Any (Pair (Pair a Any) Any))) -> a]))
(defn caaaar [p] (-> p car car car car))
(defn caaadr [p] (-> p cdr car car car))
(defn caadar [p] (-> p car cdr car car))
(defn caaddr [p] (-> p cdr cdr car car))
(ann cadaar (All [a] [(Pair (Pair (Pair Any (Pair a Any)) Any) Any) -> a]))
(ann cadadr (All [a] [(Pair Any (Pair (Pair Any (Pair a Any)) Any)) -> a]))
(ann caddar (All [a] [(Pair (Pair Any (Pair Any (Pair a Any))) Any) -> a]))
(ann cadddr (All [a] [(Pair Any (Pair Any (Pair Any (Pair a Any)))) -> a]))
(defn cadaar [p] (-> p car car cdr car))
(defn cadadr [p] (-> p cdr car cdr car))
(defn caddar [p] (-> p car cdr cdr car))
(defn cadddr [p] (-> p cdr cdr cdr car))
(ann cdaaar (All [a] [(Pair (Pair (Pair (Pair Any a) Any) Any) Any) -> a]))
(ann cdaadr (All [a] [(Pair Any (Pair (Pair (Pair Any a) Any) Any)) -> a]))
(ann cdadar (All [a] [(Pair (Pair Any (Pair (Pair Any a) Any)) Any) -> a]))
(ann cdaddr (All [a] [(Pair Any (Pair Any (Pair (Pair Any a) Any))) -> a]))
(defn cdaaar [p] (-> p car car car cdr))
(defn cdaadr [p] (-> p cdr car car cdr))
(defn cdadar [p] (-> p car cdr car cdr))
(defn cdaddr [p] (-> p cdr cdr car cdr))
(ann cddaar (All [a] [(Pair (Pair (Pair Any (Pair Any a)) Any) Any) -> a]))
(ann cddadr (All [a] [(Pair Any (Pair (Pair Any (Pair Any a)) Any)) -> a]))
(ann cdddar (All [a] [(Pair (Pair Any (Pair Any (Pair Any a))) Any) -> a]))
(ann cddddr (All [a] [(Pair Any (Pair Any (Pair Any (Pair Any a)))) -> a]))
(defn cddaar [p] (-> p car car cdr cdr))
(defn cddadr [p] (-> p cdr car cdr cdr))
(defn cdddar [p] (-> p car cdr cdr cdr))
(defn cddddr [p] (-> p cdr cdr cdr cdr))

(typed/defn get-new-pair [] :- (Pair nil nil)
  (->Pair (typed/atom :- nil nil)
          (typed/atom :- nil nil)))


(ann ^:no-check my-cons (All [a b] [a b -> (Pair a b)]))
(defn my-cons [a b]
  (let [new (get-new-pair)]
    (set-car! new a)
    (set-cdr! new b)
    new))


(defalias List (TFn [[a :variance :covariant]]
                    (Rec [this] (Pair a (Option this)))))


(declare my-reverse)

(ann ^:no-check reverse-list
     (All [a] (IFn [nil -> nil]
                   [(NonEmptySeqable a) -> (List a)])))
(defn reverse-list [xs]
  (loop [xs xs
         ret nil]
    (if-let [xs (seq xs)]
      (recur (rest xs)
             (my-cons (first xs) ret))
      ret)))

(ann ^:no-check my-list
     (All [a] (IFn [-> nil]
                   [a a * -> (List a)])))
(defn my-list
  {:test #(do
            (is (= (my-list)
                   nil))
            (is (= (my-list 1 2 3)
                   (my-cons 1 (my-cons 2 (my-cons 3 nil))))))}
  [& xs]
  (my-reverse (reverse-list xs)))


(ann pair? (Pred (Pair Any Any)))
(defn pair? [x]
  (instance? Pair x))


(ann any? (All [a] [[a -> Boolean] (U Any (List a)) -> Boolean]))
(defn any?
  {:test #(do
            (is (any? odd? (my-list 2 4 6 1 7)))
            (is (not (any? odd? (my-list 2 4 6)))))}
  [pred x]
  (and (pair? x)
       (or (pred (car x))
           (recur pred (cdr x)))))


(ann ^:no-check my-reverse
     (All [a] (IFn [nil -> nil]
                   [(List a) -> (List a)])))
(defn my-reverse
  {:test #(do
            (are [in out] (= in out)
              (my-reverse nil)
              nil
              (my-reverse (my-list 1))
              (my-list 1)
              (my-reverse (my-list 1 2))
              (my-list 2 1)))}
  [p]
  (loop [in p
         out nil]
    (if (nil? in)
      out
      (recur (cdr in)
             (my-cons (car in)
                      out)))))


(ann ^:no-check reverse-map
     (All [a b c]
          (IFn [[a -> b] nil -> nil]
               [[a -> b] (List a) -> (List b)]
               [[a b -> c] nil (List b) -> nil]
               [[a b -> c] (List a) nil -> nil]
               [[a b -> c] (List a) (List b) -> (List c)]
               )))
(defn reverse-map
  ([f p]
   (loop [p p
          ret nil]
     (if (nil? p)
       ret
       (recur (cdr p)
              (my-cons (f (car p))
                       ret)))))
  ([f p1 p2]
   (loop [p1 p1
          p2 p2
          ret nil]
     (if (or (nil? p1)
             (nil? p2))
       ret
       (recur (cdr p1)
              (cdr p2)
              (my-cons (f (car p1)
                          (car p2))
                       ret))))))


(ann my-map (All [a b c]
                 (IFn [[a -> b] -> [[c b -> c] -> [c a -> c]]]
                      [[a -> b] nil -> nil]
                      [[a -> b] (List a) -> (List b)]
                      [[a b -> c] nil (List b) -> nil]
                      [[a b -> c] (List a) nil -> nil]
                      [[a b -> c] (List a) (List b) -> (List c)]
                      )))
(defn my-map
  {:test #(do
            (are [in out] (= in out)
              (my-map inc nil)
              nil
              (my-map inc (my-list 1 2))
              (my-list 2 3)
              (my-map + (my-list 1 2) (my-list 3 4 5))
              (my-list 4 6)))}
  ([f] (typed/fn xf [rf :- [c b -> c]]
         (typed/fn xrf [ret :- c x :- a]
           (rf ret (f x)))))
  ([f p] (my-reverse (reverse-map f p)))
  ([f p1 p2] (my-reverse (reverse-map f p1 p2))))

(ann foldl
     (All [a b c]
          (IFn [[b a -> b] b (Option (List a)) -> b]
               [[[c b -> c] -> [c a -> c]] [c b -> c] c (Option (List a)) -> c]
               )))
(defn foldl
  ([rf init xs]
   (if (nil? xs)
     init
     (recur rf (rf init (car xs)) (cdr xs))))
  ([xf rf init xs] (foldl (xf rf) init xs)))

(ann foldr (All [a b] [[a b -> b] (Option (List a)) b -> b]))
(defn foldr
  ([rf xs init]
   (if (nil? xs)
     init
     (rf (car xs) (foldr rf (cdr xs) init)))))

(ann flip (All [a b c] [[a b -> c] -> [b a -> c]]))
(defn flip [f] (typed/fn [y :- b x :- a] (f x y)))

(ann foldr-tc (All [a b] (IFn
                          [[a b -> b] nil b -> b]
                          [[a b -> b] (List a) b -> b]
                          )))
(defn foldr-tc
  ([rf xs init] (foldl (flip rf) init (my-reverse xs))))

(ann ^:no-check my-concat (All [a] [(Option (List a)) * -> (Option (List a))]))
(defn my-concat
  {:test #(do
            (are [x y out] (= (my-concat x y) out)
              nil nil nil
              nil (my-list 1) (my-list 1)
              (my-list 1) nil (my-list 1)
              (my-list 1 2) (my-list 3 4) (my-list 1 2 3 4))
            (is (= (my-concat (my-list 1) (my-list 2) (my-list 3))
                   (my-list 1 2 3))))}
  [& ps]
  (foldr-tc (typed/fn [x :- (List a) y :- (List a)] (foldr-tc my-cons x y))
            (apply my-list ps)
            nil))

(ann length (All [a] [(Option (List a)) -> Int]))
(defn length [p]
  (loop [p :- (Option (List a)) p
         n :- Int 0]
    (if (nil? p)
      n
      (recur (cdr p) (inc n)))))


(ann print-my-list [(List Any) -> nil])
(defn print-my-list [l]
  (let [head (car l)
        more (cdr l)]
    (print (str head " "))
    (if (pair? more)
      (recur more)
      (print "\n"))))
