(define-interface treaps-interface
  (export make-treap
          treap-get
          treap-delete!
          treap-get-min
          treap-get-max
          treap-delete-min!
          treap-delete-max!
          treap-empty?
          treap-depth 
          treap-clear!
          treap-put! 
          treap-for-each-ascending
          treap-for-each-descending
          treap-debugprint debugprint))
