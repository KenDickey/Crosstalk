
;; "/home/kend/SiS/Temp/Collection.scm" (translated)

(begin
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'add:
    (lambda (self newObject)
      (perform: self 'subclassResponsibility)))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'asArray
    (lambda (self)
      (call/cc
        (return)
        (let ((result nil) (i nil))
          (set! result
            (perform:with: Array 'new: (perform: self 'size)))
          (set! i 0)
          (perform:with:
            self
            'do:
            (lambda (each)
              (perform:with:with:
                result
                'at:put:
                (set! i (perform:with: i '+ 1))
                each)))
          (return result)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'asByteArray
    (lambda (self)
      (call/cc
        (return)
        (let ((result nil) (i nil))
          (set! result
            (perform:with:
              ByteArray
              'new:
              (perform: self 'size)))
          (set! i 0)
          (perform:with:
            self
            'do:
            (lambda (each)
              (perform:with:with:
                result
                'at:put:
                (set! i (perform:with: i '+ 1))
                each)))
          (return result)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'asSet
    (lambda (self)
      (call/cc
        (return)
        (let ((aSet nil))
          (set! aSet
            (perform:with: Set 'new: (perform: self 'size)))
          (perform:with:
            self
            'do:
            (lambda (each) (perform:with: aSet 'add: each)))
          (return aSet)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'collect:
    (lambda (self aBlock)
      (call/cc
        (return)
        (let ((newCollection nil))
          (set! newCollection
            (perform: (perform: self 'species) 'new))
          (perform:with:
            self
            'do:
            (lambda (each)
              (perform:with:
                newCollection
                'add:
                (perform:with: aBlock 'value: each))))
          (return newCollection)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'detect:ifNone:
    (lambda (self aBlock exceptionBlock)
      (call/cc
        (return)
        (perform:with:
          self
          'do:
          (lambda (each)
            (perform:with:
              (perform:with: aBlock 'value: each)
              'ifTrue:
              (lambda () (return each)))))
        (return (perform: exceptionBlock 'value)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'do:
    (lambda (self aBlock)
      (perform: self 'subclassResponsibility)))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'emptyCheck
    (lambda (self)
      (perform:with:
        (perform: self 'isEmpty)
        'ifTrue:
        (lambda () (perform: self 'errorEmptyCollection)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'errorEmptyCollection
    (lambda (self)
      (perform:with:
        self
        'error:
        "this collection is empty")))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'errorNotFound
    (lambda (self)
      (perform:with:
        self
        'error:
        "Object is not in the collection.")))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'includes:
    (lambda (self anObject)
      (call/cc
        (return)
        (perform:with:
          self
          'do:
          (lambda (each)
            (perform:with:
              (perform:with: anObject '= each)
              'ifTrue:
              (lambda () (return true)))))
        (return false))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'isEmpty
    (lambda (self)
      (call/cc
        (return)
        (return
          (perform:with: (perform: self 'size) '= 0)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'printOn:
    (lambda (self aStream)
      (perform:with:
        aStream
        'nextPutAll:
        (perform:with:
          (perform: (perform: self 'class) 'name)
          '|,|
          " ("))
      (perform:with:
        self
        'do:
        (lambda (element)
          (perform:with: element 'printOn: aStream)
          (perform: aStream 'space)))
      (perform:with: aStream 'nextPut: #\))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'remove:
    (lambda (self oldObject)
      (call/cc
        (return)
        (return
          (perform:with:with:
            self
            'remove:ifAbsent:
            oldObject
            (lambda () (perform: self 'errorNotFound)))))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'remove:ifAbsent:
    (lambda (self oldObject anExceptionBlock)
      (perform: self 'subclassResponsibility)))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'select:
    (lambda (self aBlock)
      (call/cc
        (return)
        (let ((newCollection nil))
          (set! newCollection
            (perform: (perform: self 'species) 'new))
          (perform:with:
            self
            'do:
            (lambda (each)
              (perform:with:
                (perform:with: aBlock 'value: each)
                'ifTrue:
                (lambda ()
                  (perform:with: newCollection 'add: each)))))
          (return newCollection)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'size
    (lambda (self)
      (call/cc
        (return)
        (let ((count nil))
          (set! count 0)
          (perform:with:
            self
            'do:
            (lambda (each)
              (set! count (perform:with: count '+ 1))))
          (return count)))))
  (perform:with:with:
    Collection
    'addSelector:withMethod:
    'sum
    (lambda (self)
      (call/cc
        (return)
        (let ((total nil) (seed nil))
          (set! total
            (set! seed
              (perform:with:with:
                self
                'detect:ifNone:
                (lambda (x) true)
                (lambda () (return 0)))))
          (perform:with:
            self
            'do:
            (lambda (el)
              (set! total (perform:with: total '+ el))))
          (return (perform:with: total '- seed))))))
  (perform:with:with:
    (perform: Collection 'class)
    'addSelector:withMethod:
    'with:
    (lambda (self anObject)
      (call/cc
        (return)
        (let ((newCollection nil))
          (set! newCollection (perform: self 'new))
          (perform:with: newCollection 'add: anObject)
          (return newCollection)))))
  (perform:with:with:
    (perform: Collection 'class)
    'addSelector:withMethod:
    'with:with:
    (lambda (self firstObject secondObject)
      (call/cc
        (return)
        (let ((newCollection nil))
          (set! newCollection (perform: self 'new))
          (perform:with: newCollection 'add: firstObject)
          (perform:with: newCollection 'add: secondObject)
          (return newCollection)))))
  (perform:with:with:
    (perform: Collection 'class)
    'addSelector:withMethod:
    'with:with:with:
    (lambda (self firstObject secondObject thirdObject)
      (call/cc
        (return)
        (let ((newCollection nil))
          (set! newCollection (perform: self 'new))
          (perform:with: newCollection 'add: firstObject)
          (perform:with: newCollection 'add: secondObject)
          (perform:with: newCollection 'add: thirdObject)
          (return newCollection))))))

;;   -- e o f ---    ;;
