(ns tnoc.rsa
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as string])
  (:import (java.util Random)
           (clojure.lang BigInt)))

(defn string-to-bigints [string bytesize]
  "Splits up the input into BigInts whose length is bounded by bytesize."
  (->> (.getBytes string)
       (partition bytesize bytesize [])
       (map #(BigInt/fromBigInteger (BigInteger. (byte-array %))))))

(defn bigints-to-string [bigints]
  "Takes a sequence of BigInts and reproduces a string from the constituent bytes."
  (->> bigints
       (mapcat #(.toByteArray (.toBigInteger %)))
       (byte-array)
       (String.)))

(defn mod-exp [x y n]
  "Computes (x ^ y) % n using the repeated squaring method."
  (if (zero? y)
    1
    (let [result (-> (mod (*' x x) n) (mod-exp (quot y 2) n))]
      (if (even? y)
        result
        (mod (*' result x) n)))))

(defn euclid [a b]
  "Returns a vector [x y gcd] such that x*a + y*b = gcd."
  (if (zero? b)
    [1 0 a]
    (let [k (quot a b)
          [x y gcd] (euclid b (- a (* k b)))]
      [y (- x (* y k)) gcd])))

(defn gcd [a b]
  "The greatest common divisor of a and b."
  (nth (euclid a b) 2))

(defn invert [a n]
  "Assuming 0 < a < n and a and n are relatively prime
  finds the multiplicative inverse of a in Z_n."
  (let [x (first (euclid a n))]
    (+' x (*' n (inc (quot x n))))))

(defn rand-bigint [n]
  "Returns a random BigInt in the in the range 1 (inclusive) to n (exclusive)."
  (let [rnd (Random.)]
    (->> (repeatedly #(BigInteger. (.bitLength (bigint n)) rnd))
         (filter #(< 0 % n))
         (first))))

(defn miller-rabin [p]
  "Applies the Miller-Rabin primality test once,
  yielding a false positive with probability <= 1/2."
  (if (< 1 p)
    (let [a (rand-bigint p)]
      (cond
        (not= 1 (gcd a p)) false
        (not= 1 (mod-exp a (dec p) p)) false
        :else (loop [k (dec p)]
                (if (= 1 (mod-exp a k p))
                  (if (even? k)
                    (recur (/ k 2))
                    true)
                  (= (dec p) (mod-exp a k p))))))))

(defn prime? [p]
  "Tests if n is prime using the Miller-Rabin test,
  yielding a false positive with probability <= 2^-100."
  (if (< 1 p)
    (every? identity (repeatedly 100 #(miller-rabin p)))))

(defn probable-prime
  "Generates a prime number in the range [0, 2^bitsize - 1]
  with a chance of 2^-100 of returning a composite number.
  Optionally an integer e > 1 may be provided, in which case the
  returned prime p won't have the property that p-1 is divisible by e."
  ([bitsize]
   (let [rnd (Random.)]
     (loop [possible-prime (BigInteger. bitsize rnd)
            attempts 1]
       (if (prime? possible-prime)
         (do #_(println "after " attempts " attempts")
           possible-prime)
         (recur (inc possible-prime) (inc attempts))))))
  ([bitsize e]
   (loop [p (probable-prime bitsize)]
     (if (zero? (mod (dec p) e))
       (recur (probable-prime bitsize))
       p))))

(defn make-keys [bitsize e]
  "Generates a map containing the private keys :p and :q (both prime),
  as well as the public keys :N (= p * q) and :e."
  (let [p (probable-prime bitsize e)
        q (probable-prime bitsize e)]
    {:p p :q q :N (*' p q) :e e}))

(defn encrypt [string N e]
  "Encrypts a string by converting it into a sequence of BigInts,
  each of which is raised to the power of e in arithmetic done mod N."
  (->> (quot (.bitLength (bigint N)) 16)
       (string-to-bigints string)
       (map #(mod-exp % e N))))

(defn encrypt-with-new-keys [string bitsize e]
  "Encrypts a string using two new prime numbers whose length is given by bitsize
  and a public exponent of e. Returns a map containing both the public and private keys,
  as well as the encrypted string."
  (let [{N :N :as keys} (make-keys bitsize e)]
    (assoc keys :encryption (encrypt string N e))))

(defn decrypt
  "Decrypts a sequence of BigInts by finding the multiplicative inverse of e
  in the group Z_{(p-1)*(q-1)} and raising the BigInts to that power.
  The decrypted BigInts are then concatenated bytewise and cast to a string."
  ([encryption p q e]
   (let [d (invert e (*' (dec p) (dec q)))
         N (*' p q)]
     (->> encryption
          (map #(mod-exp % d N))
          (bigints-to-string))))
  ([{encryption :encryption p :p q :q e :e}]
   (decrypt encryption p q e)))

(load "rsa_spec")
