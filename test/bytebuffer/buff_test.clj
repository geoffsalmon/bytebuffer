(ns bytebuffer.buff-test
  (:use [bytebuffer.buff] :reload-all)
  (:use [clojure.test])
  (:import (java.nio ByteBuffer ByteOrder)))

; choose values that have a different value for each byte
(def b (byte 0x1))
(def s 0x302)
(def i 0x7060504)
(def l 0x0f0e0d0c0b0a0908)

;; compute powers of 2
(def pows
  (conj
   (vec (reduce (fn [list _] (conj list (* 2 (peek list)))) [1] (range 62)))
   ;; 2^63
   0x8000000000000000N))

;; computer powers of 2 minus 1
(def pows-1
  {8 0xFF
   16 0xFFFF
   32 0xFFFFFFFF
   64 0xFFFFFFFFFFFFFFFF})

(defn pow2 [e] (nth pows e))

(deftest test-packing
  "Check that buffers filled with pack are identical to those
filled callin the Java put* methods"
  (is (=
       (doto (ByteBuffer/allocate 100)
         (.put b)
         (.putShort s)
         (.putInt i)
         (.putLong l)
         (.flip)
         )
       (.flip (pack (byte-buffer 100) "bsil" b s i l))))
  )

(deftest test-packing-error
  (is (thrown? Exception
               (pack (byte-buffer 100) "x" 1)))

  (is (thrown? Exception
               (pack (byte-buffer 100) "bb" 1)))

  (is (thrown? Exception
               (pack (byte-buffer 100) "b" 1 1)))
  )

(deftest test-unpacking-error
  (let [buff (.flip (pack (byte-buffer 100) "i" 1))]
    (is (thrown? Exception
                 (unpack buff "x")))
    
    )
  )

(deftest test-unpacking
  "Check that unpack is the inverse of pack"
  (let [vals [b s i l]
        fmt "bsil"
        buff (.flip (apply pack (byte-buffer 100) fmt vals))]
    (is (=
         vals
         (unpack buff fmt)
         )))
  )

(deftest test-signed-unsigned
  (let [max-unsigned (vals pows-1)
        mid-unsigned (map #(pow2 %) [7 15 31 63])
        min-signed (map - mid-unsigned)
        
        buff
        (.flip
         (apply pack (byte-buffer 100)
                "bsilbsilbsilbsil"
                
                (concat
                 (repeat 4 -1)
                 max-unsigned
                 mid-unsigned
                 min-signed
                 )
               ))]

    ; unpack as signed variables
    (is (= (repeat 8 -1)
           (unpack buff "bsilbsil")))
    
    (is (= (apply concat (repeat 2 min-signed))
           (unpack buff "bsilbsil")))

    (.position buff 0) ; reread buffer from start

    ; unpack as unsigned variables
    (is (= (apply concat (repeat 2 max-unsigned))
           (unpack buff "BSILBSIL")))
    
    (is (= (apply concat (repeat 2 mid-unsigned))
           (unpack buff "BSILBSIL")))
    )
  )

(def b15 (pow2 15))
(def b16 (pow2 16))
(def b31 (pow2 31))
(def b32 (pow2 32))
(def b63 (pow2 63))

(defn- pack-flip [fmt & vars]
  (.flip (apply pack (byte-buffer 100) fmt vars))
  )

(deftest test-take-bytes
  (let [buff (pack-flip "bbbbbb" 0 0 -128 -128 -1 -1)]
    (is (= 0 (take-byte buff)))
    (is (= 0 (take-ubyte buff)))
  
    (is (= -128 (take-byte buff)))
    (is (= 128 (take-ubyte buff)))
  
    (is (= -1 (take-byte buff)))
    (is (= 255 (take-ubyte buff)))))

(deftest test-take-short
  (let [buff (pack-flip "ssssss" 0 0 (- b15) (- b15) -1 -1)]

    (is (= 0 (take-short buff)))
    (is (= 0 (take-ushort buff)))
  
    (is (= (- b15) (take-short buff)))
    (is (= b15 (take-ushort buff)))
  
    (is (= -1 (take-short buff)))
    (is (= (- b16 1) (take-ushort buff)))))

(deftest test-take-int
  (let [buff (pack-flip "iiiiii" 0 0 (- b31) (- b31) -1 -1)]
    
    (is (= 0 (take-int buff)))
    (is (= 0 (take-uint buff)))
  
    (is (= (- b31) (take-int buff)))
    (is (=  b31 (take-uint buff)))
  
    (is (= -1 (take-int buff)))
    (is (= (- b32 1) (take-uint buff)))))

(deftest test-take-long
  (let [buff (pack-flip "llllll" 0 0 (- b63) (- b63) -1 -1)]
    
    (is (= 0 (take-long buff)))
    (is (= 0 (take-ulong buff)))
  
    (is (= (- b63) (take-long buff)))
    (is (= b63 (take-ulong buff)))
  
    (is (= -1 (take-long buff)))
    (is (= (pows-1 64) (take-ulong buff)))))


(deftest test-bit-pack
  (is (= 0x120428 (pack-bits 4 1 4 2 4 0 4 4 8 0x28)))


  (is (= 2r11010001 (pack-bits 2 3 2 true 3 false 1 true)) "use booleans for values 0 and 1")

  (is (thrown? Exception
               (pack-bits 4 1 4 5 4)) "field without value")
  
  (is (thrown? Exception
               (pack-bits 4 1 4 -5 4 2)) "negative values")

  (is (thrown? Exception
               (pack-bits 4 1 0 0 4 2)) "zero bit lengths")

  (is (thrown? Exception
               (pack-bits 4 1 -10 5 4 2)) "negative bit lengths")

  (is (thrown? Exception
               (pack-bits 4 1 4 16 4 2)) "insufficient bit lengths")
  )

(deftest test-bit-unpack
  (is (= [0 0 0x12 0x34 0x5] (unpack-bits 0x12345 3 9 8 8 4)))
  
  (is (= [0x12 0x5] (unpack-bits 0x12345 8 -8 4)) "Skip bits")

  (is (= [2r1010 true false 2r01] (unpack-bits 2r10101001 4 \b \b 2)) "bits as booleans")
  
  (is (= [0x12 0x34 0x5] (unpack-bits 0x12345 0 0 8 8 0 4)) "Weird 0 bit length. Should this be an error instead?")
  )

(deftest slice
  (let [buff (pack-flip "bbbbbb" 10 11 12 13 14 15)]
    (is (thrown? IndexOutOfBoundsException (slice-off buff 20)))
    (let [b1 (slice-off buff 4)]
      (is (= [10 11 12 13] (unpack b1 "bbbb")))
      (is (zero? (.remaining b1)))

      (is (thrown? IndexOutOfBoundsException (slice-off buff 4)))
      (is (= [14 15] (unpack buff "bb")))
      )
    ))
