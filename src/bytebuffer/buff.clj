(ns
    #^{
       :doc "Library for packing and unpacking binary data. Simplifies working
with java.nio.ByteBuffer objects.

Notable features:

1. Handles signed and unsigned values pleasantly. Usually reading or
writing unsigned fields with ByteBuffers is a pain because Java
doesn't have unsigned primitives. The take-* functions return a long
which is big enough to handle the entire unsigned range of most
types. To handle large unsigned longs, the take-ulong function can
return either a long or a clojure.lang.BigInt. Similarly, although
there are not separate signed and unsigned version, the put-*
functions will accept any number type and truncate it so both positive
and negative numbers can be stored. TODO: Should add an overflow check
to avoid overflows in put-* functions?

2. Provides pack and unpack functions inspired by Python's struct
module. Use simple format strings, similar to printf's, to define how
fields are layed out in the buffer.

Usage:
 (pack buff \"isbb\" 123 43 23 3) ; puts an int, a short and two bytes into buff
 (.flip buff) ; assuming nothing else was written to the buffer
 (unpack buff \"isbb\") => (123 43 24 3)

3. Provides pack-bits and unpack-bits functions for working with bit
fields within numbers. These are useful for pulling apart flag fields.
"
       :see-also [["http://java.sun.com/j2se/1.5.0/docs/api/java/nio/ByteBuffer.html" "java.nio.ByteBuffer Documentation"]]
}

  bytebuffer.buff
  (:import (java.nio ByteBuffer ByteOrder))
  )

(defn byte-buffer
  "Creates a ByteBuffer of capacity bytes"
  [capacity]
  (ByteBuffer/allocate capacity))

#_(def ^{:tag :dynamic
       :private true
       :doc "The current buffer. Use with-buffer to bind this."}
    *byte-buffer* nil)

(def ^:dynamic *byte-buffer* nil)

(defmacro with-buffer
  "Sets the buffer currently being used by the put-* and take-*
functions which do not take buffers."
  [buffer & body]
  `(binding [*byte-buffer* ~buffer]
     ~@body))

(defn put-byte
  "Puts a byte into the buffer"
  ([val]
     (put-byte *byte-buffer* val))
  ([^ByteBuffer buff val]
     (.put buff (.byteValue val)))
  )

(defn put-short
  "Puts a short (2 bytes) into the buffer"
  ([val]
     (put-short *byte-buffer* val))
  ([^ByteBuffer buff val]
     (.putShort buff (.shortValue val)))
  )

(defn put-int
  "Puts an int (4 bytes) into the buffer"
  ([val]
     (put-int *byte-buffer* val))
  ([^ByteBuffer buff val]
     (.putInt buff (.intValue val)))
  )

(defn put-long
  "Puts a long (8 bytes) into the buffer"
  ([val]
     (put-long *byte-buffer* val))
  ([^ByteBuffer buff val]
     (.putLong buff (.longValue val)))
  )

(defn take-byte
  "Takes a signed byte from the buffer"
  (^long []
     (take-byte *byte-buffer*))
  (^long [^ByteBuffer buff]
         (long (.get buff)))
  )

(defn take-ubyte
  "Takes an unsigned signed byte from the buffer"
  (^long []
     (take-ubyte *byte-buffer*))
  (^long [^ByteBuffer buff]
     (bit-and 0xFF (long (.get buff))))
  )

(defn take-short
  "Takes a signed short (2 bytes) from the buffer"
  (^long []
     (take-short *byte-buffer*))
  (^long [^ByteBuffer buff]
         (long (.getShort buff)))
  )

(defn take-ushort
  "Takes a unsigned short (2 bytes) from the buffer"
  (^long []
     (take-ushort *byte-buffer*))
  (^long [^ByteBuffer buff]
     (bit-and 0xFFFF (long (.getShort buff))))
  )

(defn take-int
  "Takes a signed int (4 bytes) from the buffer"
  (^long []
     (take-int *byte-buffer*))
  (^long [^ByteBuffer buff]
     (.getInt buff))
  )
  
(defn take-uint
  "Takes a unsigned int (4 bytes) from the buffer"
  (^long []
     (take-uint *byte-buffer*))
  (^long [^ByteBuffer buff]
     (bit-and 0xFFFFFFFF (long (.getInt buff))))
  )

(defn take-long
  "Takes a signed long (8 bytes) from the buffer"
  (^long []
     (take-long *byte-buffer*))
  (^long [^ByteBuffer buff]
     (.getLong buff))
  )
  
(defn take-ulong
  "Takes a unsigned long (8 bytes) from the buffer"
  ([]
     (take-ulong *byte-buffer*))
  ([^ByteBuffer buff]
         (let [l (.getLong buff)]
           (if (>= l 0)
             l
             ;; add 2^64 to treat the negative 64bit 2's complement
             ;; num as unsigned.
             (+ 18446744073709551616N (bigint l)))))
  )


(defn ^ByteBuffer slice-off [^ByteBuffer buff len]
  "Create a new bytebuffer by slicing off the first len bytes. Also
consumes the bytes in the given buffer."
  (if (> len (.remaining buff))
    (throw (IndexOutOfBoundsException. (str "Cannot slice-off " len " bytes of remaining " (.remaining buff) " bytes.") ))
    (let [rdbuf (-> buff (.slice) (.limit len))]
      (.position buff (+ (.position buff) len)) ; advance the actual buffer
      rdbuf
      ))
  ) 

(defn- pack-one [buff fmt val]
  (condp = fmt
        \b (put-byte buff val)
        \s (put-short buff val)
        \i (put-int buff val)
        \l (put-long buff val)
        (throw (IllegalArgumentException. (str "Unknown format symbol \"" fmt \")))
        ))

(defn pack
  "Puts one or more numbers for vals into buff using field sizes
  determined by the characters in the fmt sequence. Valid characters are
b - byte
s - short
i - int
l - long

The number of characters in fmt must match the number of numbers in vals.

Usage: (pack buff \"isbb\" 123 43 23 3) ; puts an int, a short and two bytes into buff

Returns buff."
  [buff fmt & vals]
  (when-not (= (count fmt) (count vals))
    (throw (IllegalArgumentException. "pack error. Number of format symbols must match number of values.")))

  (doseq [[f val] (partition 2 (interleave fmt vals))]
    (pack-one buff f val))
  buff
  )

(defn- unpack-one [buff fmt]
  (condp = fmt
        \b (take-byte buff)
        \B (take-ubyte buff)
        \s (take-short buff)
        \S (take-ushort buff)
        \i (take-int buff)
        \I (take-uint buff)
        \l (take-long buff)
        \L (take-ulong buff)
        (throw (IllegalArgumentException. (str "Unknown format symbol \"" fmt \")))
        ))

(defn unpack
  "Returns a sequence of one or more numbers taken from buff. The
  number and type of numbers taken is determined by fmt which is a
  sequence of characters. Valid characters in fmt:

b - byte
B - unsigned byte
s - short
S - unsigned short
i - int
I - unsigned int
l - long
L - unsigned long"  
  [buff fmt]
  (doall (map (partial unpack-one buff) fmt))
  )

(defn- bit-val [x]
  (if (instance? Boolean x)
    (if x 1 0)
    x
    )
  )

(defn pack-bits
  "Packs multiple numbers into a single number using explicit bit lengths.

fields => bit-length value

The value can also be a boolean. true is stored as 1, false as 0"
  ([] 0)
  ([& fields]
     (when-not (zero? (mod (count fields) 2))
       (throw (IllegalArgumentException. (str "pack-bits Last field does not have a value.")))
       )

     (reduce (fn [acc [num-bits val-in]]
               (let [val (bit-val val-in)]
                 (when-not (pos? num-bits)
                   (throw (IllegalArgumentException.
                           (str "pack-bits: Invalid bit length " num-bits ". Must be positive."))))
               
                 (when (neg? val)
                   (throw (IllegalArgumentException.
                           (str "pack-bits: Invalid value " val ". Must be non-negative."))))
               
                 ; ensure specified bit length was big enough
                 (when-not (= val
                              (bit-and val (dec (bit-shift-left 1 num-bits)))
                          
                              )
                   (throw (IllegalArgumentException.
                           (str "pack-bits: Invalid value " val ". Must fit in " num-bits " bits as specified.")))
                   )
               
                 (-> acc
                     (bit-shift-left num-bits)
                     (bit-or val)
                     ))
               )
             0 (partition 2 fields))
     )
  )

(defn unpack-bits
  "Pulls apart a number into a list of fields of various bit lengths.
Pass a non-positive bit length to skip that many bits without adding a
corresponding value to the result list.

Passing a field length of 1 will add either a 0 or a 1 to the
resulting sequence. To get a boolean value instead, pass \\b."
  [x & bit-lengths]
  (loop [val x
         results '()
         ; iterate bit lengths in reverse to continually pull 
         ; values from the low order bits
         rbit-lens (reverse bit-lengths)]

    (if (= '() rbit-lens)
      results

      (let [bits (first rbit-lens)]
        (cond
         (= \b bits) ; grab single bit as boolean
         (recur (bit-shift-right val 1)
                (cons (= 1 (bit-and val 1)) results)
                (rest rbit-lens)
                )
         
         (pos? bits)
         (recur (bit-shift-right val bits)
                (cons
                 (bit-and val (dec (bit-shift-left 1 bits)))
                 results)
                (rest rbit-lens)
                )
         :else ; skip bits
         (recur (bit-shift-right val (- bits))
                results
                (rest rbit-lens)
                )
         )
        )
      )
    )
  )

(defn bin [x]
  "Returns x as a binary number in a string"
  (.toString (bigint x) 2)
  )
