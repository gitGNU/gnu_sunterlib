(define-interface banana-interface
  (export

   (banana-error? (proc (:value) :boolean))

   (unknown-byte-error? (proc (:value) :boolean))
   (unknown-byte-error-byte (proc (:value) :exact-integer))
   (unknown-byte-error-profile (proc (:value) :value))

   (unsupported-type-error? (proc (:value) :boolean))
   (unsupported-type-error-type (proc (:value) :string))
   (unsupported-type-error-value (proc (:value) :value))

   :profile
     (make-profile
      (proc (:string (proc (:value) :value) :pair) :value))
     (extend-profile
      (proc (:value :string (proc (:value) :value) :pair) :value))
     (profile? (proc (:value) :boolean))
     (profile-name (proc (:value) :string))
     (profile-encoder (proc (:value)
                            (proc (:value) :value)))
     (profile-decoder-table (proc (:value) :value))
     (profile-super-profile (proc (:value) :value))

   profile/none

   ;; Do these next two really need to stay here?
   (etb? (proc (:exact-integer) :boolean))

   (read-element! (proc (:input-port :value) :value))

   (decode (proc (:value &opt :value) :value))
   (encode (proc (:value &opt :value) :value))))

(define-interface banana-extras-interface
  (export

   ;; These can be used for other profiles as lengths and such.
   posint->byte-vector
   byte-vector->posint

   ;; Generally just for debugging or manual testing.
   prettify-byte
   prettify-byte-vector))
