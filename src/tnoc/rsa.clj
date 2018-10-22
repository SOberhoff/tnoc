(ns tnoc.rsa
  (:require [tnoc.utils :refer [euclid prime? mod-exp]]
            [clojure.spec.alpha :as spec]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.string :as str])
  (:import (java.util Random)
           (clojure.lang BigInt)))

(defn string-to-bigints [string bytesize]
  "Splits up the input into BigInts whose number of bytes are bounded by bytesize."
  (->> (.getBytes string)
       (partition bytesize bytesize [])
       (map #(BigInt/fromBigInteger (BigInteger. (byte-array %))))))

(defn bigints-to-string [bigints]
  "Takes a sequence of BigInts and reproduces a string from the constituent bytes."
  (->> bigints
       (mapcat #(.toByteArray (.toBigInteger %)))
       (byte-array)
       (String.)))

(defn invert [a n]
  "Assuming 0 < a < n and a and n are relatively prime
  finds the multiplicative inverse of a in Z_n."
  (let [x (first (euclid a n))]
    (+' x (*' n (inc (quot x n))))))

(defn probable-prime
  "Generates a prime number in the range [0, 2^bitsize - 1]
  with a chance of 2^-100 of returning a composite number.
  Optionally an integer e > 1 may be provided, in which case the
  returned prime p won't have the property that p-1 is divisible by e."
  ([bitsize]
   (->> (Random.)
        (BigInteger. bitsize)
        (iterate inc)
        (filter prime?)
        (first)))
  ([bitsize e]
   (->> (repeatedly (partial probable-prime bitsize))
        (filter #(not (zero? (mod (dec %) e))))
        (first))))

(defn make-keys [bitsize e]
  "Generates a map containing the private keys :p and :q (both prime),
  as well as the public keys :N (= p * q) and :e."
  (let [p (probable-prime bitsize e)
        q (probable-prime bitsize e)]
    {:p p :q q :N (*' p q) :e e}))

(defn encrypt [string N e]
  "Encrypts a string by converting it into a sequence of BigInts,
  each of which is raised to the power of e in arithmetic done mod N."
  (->> (/ (.bitLength (bigint N)) 8)
       (#(if (integer? %) (dec %) (int %)))
       (string-to-bigints string)
       (map #(mod-exp % e N))))

(defn encrypt-with-new-keys [string bitsize e]
  "Encrypts a string using two new prime numbers whose length is given by bitsize
  and a public exponent e. Returns a map containing both the public and private keys,
  as well as the encrypted string."
  (let [{N :N :as keys} (make-keys bitsize e)]
    (assoc keys :encryption (encrypt string N e))))

(defn decrypt
  "Decrypts a sequence of BigInts by finding the multiplicative inverse of e
  in the group Z_N and raising the BigInts to that power.
  The decrypted BigInts are then concatenated bytewise and cast to a string."
  ([encryption p q e]
   (let [d (invert e (*' (dec p) (dec q)))
         N (*' p q)]
     (->> encryption
          (map #(mod-exp % d N))
          (bigints-to-string))))
  ([{encryption :encryption p :p q :q e :e}]
   (decrypt encryption p q e)))
