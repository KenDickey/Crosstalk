
;; "/home/kend/SiS/Temp/Set.scm" (translated)

(begin
  (perform:with:with:with:
    Collection
    'newSubclassName:iVars:cVars:
    'Set
    #('tally 'array)
    nil)
  (perform:with:with:
    Set
    'addSelector:withMethod:
    '=
    (lambda (self aSet)
      (call/cc
        (return)
        (perform:with:
          (perform:with: aSet 'isKindOf: Set)
          'ifFalse:
          (lambda () (return false)))
        (perform:with:
          (perform:with:
            (perform: self 'size)
            '=
            (perform: aSet 'size))
          'ifFalse:
          (lambda () (return false)))
        (perform:with:
          self
          'do:
          (lambda (each)
            (perform:with:
              (perform:with: aSet 'includes: each)
              'ifFalse:
              (lambda () (return false)))))
        (return true))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'add:
    (lambda (self newObject)
      (call/cc
        (return)
        (let ((index nil))
          (perform:with:
            (perform:with: newObject '== nil)
            'ifTrue:
            (lambda ()
              (perform:with:
                self
                'error:
                "Sets cannot meaningfully contain nil as an element")))
          (set! index
            (perform:with: self 'findElementOrNil: newObject))
          (perform:with:
            (perform:with:
              (perform:with: array 'at: index)
              '==
              nil)
            'ifTrue:
            (lambda ()
              (perform:with:with:
                self
                'atNewIndex:put:
                index
                newObject)))
          (return newObject)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'asArray
    (lambda (self)
      (call/cc
        (return)
        (let ((s nil))
          (set! s
            (perform:with:
              WriteStream
              'on:
              (perform:with: Array 'new: (perform: self 'size))))
          (perform:with:
            self
            'do:
            (lambda (el) (perform:with: s 'nextPut: el)))
          (return (perform: s 'contents))))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'asSet
    (lambda (self) (call/cc (return) (return self))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'atNewIndex:put:
    (lambda (self index anObject)
      (perform:with:with:
        array
        'at:put:
        index
        anObject)
      (set! tally (perform:with: tally '+ 1))
      (perform: self 'fullCheck)))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'collect:
    (lambda (self aBlock)
      (call/cc
        (return)
        (let ((newSet nil))
          (perform:with:
            (perform:with: tally '= 0)
            'ifTrue:
            (lambda () (return (perform:with: Set 'new: 2))))
          (set! newSet
            (perform:with: Set 'new: (perform: self 'size)))
          (perform:with:
            array
            'do:
            (lambda (each)
              (perform:with:
                (perform:with: each '== nil)
                'ifFalse:
                (lambda ()
                  (perform:with:
                    newSet
                    'add:
                    (perform:with: aBlock 'value: each))))))
          (return newSet)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'copy
    (lambda (self)
      (call/cc
        (return)
        (return
          (perform:with:
            (perform: self 'basicCopy)
            'withArray:
            (perform: array 'basicCopy))))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'do:
    (lambda (self aBlock)
      (perform:with:
        (perform:with: tally '= 0)
        'ifTrue:
        (lambda () (call/cc (return) (return self))))
      (perform:with:
        array
        'do:
        (lambda (element)
          (perform:with:
            (perform:with: element '== nil)
            'ifFalse:
            (lambda ()
              (perform:with: aBlock 'value: element)))))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'findElementOrNil:
    (lambda (self anObject)
      (let ((index nil))
        (set! index
          (perform:with: self 'scanFor: anObject))
        (perform:with:
          (perform:with: index '> 0)
          'ifTrue:
          (lambda () (call/cc (return) (return index))))
        (perform:with:
          self
          'error:
          "There is no free space in this set!"))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'fixCollisionsFrom:
    (lambda (self index)
      (let ((length nil)
            (oldIndex nil)
            (newIndex nil)
            (element nil))
        (set! oldIndex index)
        (set! length (perform: array 'size))
        (perform:with:
          (lambda ()
            (perform:with:with:
              (perform:with: oldIndex '= length)
              'ifTrue:ifFalse:
              (lambda () (set! oldIndex 1))
              (lambda ()
                (set! oldIndex (perform:with: oldIndex '+ 1))))
            (perform:with:
              (set! element
                (perform:with: self 'keyAt: oldIndex))
              '==
              nil))
          'whileFalse:
          (lambda ()
            (set! newIndex
              (perform:with: self 'findElementOrNil: element))
            (perform:with:
              (perform:with: oldIndex '= newIndex)
              'ifFalse:
              (lambda ()
                (perform:with:with:
                  self
                  'swap:with:
                  oldIndex
                  newIndex))))))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'fullCheck
    (lambda (self)
      (perform:with:
        (perform:with:
          (perform:with: (perform: array 'size) '- tally)
          '<
          (perform:with:
            (perform:with: (perform: array 'size) '// 4)
            'max:
            1))
        'ifTrue:
        (lambda () (perform: self 'grow)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'grow
    (lambda (self)
      (let ((oldElements nil))
        (set! oldElements array)
        (set! array
          (perform:with:
            Array
            'new:
            (perform:with:
              (perform: array 'size)
              '+
              (perform:with: (perform: array 'size) 'max: 2))))
        (set! tally 0)
        (perform:with:
          oldElements
          'do:
          (lambda (each)
            (perform:with:
              (perform:with: each '== nil)
              'ifFalse:
              (lambda ()
                (perform:with: self 'noCheckAdd: each))))))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'includes:
    (lambda (self anObject)
      (call/cc
        (return)
        (return
          (perform:with:
            (perform:with:
              array
              'at:
              (perform:with: self 'findElementOrNil: anObject))
            '~~
            nil)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'init:
    (lambda (self n)
      (set! array (perform:with: Array 'new: n))
      (set! tally 0)))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'keyAt:
    (lambda (self index)
      (call/cc
        (return)
        (return (perform:with: array 'at: index)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'noCheckAdd:
    (lambda (self anObject)
      (perform:with:with:
        array
        'at:put:
        (perform:with: self 'findElementOrNil: anObject)
        anObject)
      (set! tally (perform:with: tally '+ 1))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'remove:ifAbsent:
    (lambda (self oldObject aBlock)
      (call/cc
        (return)
        (let ((index nil))
          (set! index
            (perform:with: self 'findElementOrNil: oldObject))
          (perform:with:
            (perform:with:
              (perform:with: array 'at: index)
              '==
              nil)
            'ifTrue:
            (lambda () (return (perform: aBlock 'value))))
          (perform:with:with: array 'at:put: index nil)
          (set! tally (perform:with: tally '- 1))
          (perform:with: self 'fixCollisionsFrom: index)
          (return oldObject)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'scanFor:
    (lambda (self anObject)
      (call/cc
        (return)
        (let ((element nil) (start nil) (finish nil))
          (set! start
            (perform:with:
              (perform:with:
                (perform: anObject 'hash)
                '|\\\\|
                (perform: array 'size))
              '+
              1))
          (set! finish (perform: array 'size))
          (perform:with:with:
            start
            'to:do:
            finish
            (lambda (index)
              (perform:with:
                (perform:with:
                  (perform:with:
                    (set! element (perform:with: array 'at: index))
                    '==
                    nil)
                  'or:
                  (lambda () (perform:with: element '= anObject)))
                'ifTrue:
                (lambda () (return index)))))
          (perform:with:with:
            1
            'to:do:
            (perform:with: start '- 1)
            (lambda (index)
              (perform:with:
                (perform:with:
                  (perform:with:
                    (set! element (perform:with: array 'at: index))
                    '==
                    nil)
                  'or:
                  (lambda () (perform:with: element '= anObject)))
                'ifTrue:
                (lambda () (return index)))))
          (return 0)))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'size
    (lambda (self) (call/cc (return) (return tally))))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'swap:with:
    (lambda (self oneIndex otherIndex)
      (perform:with:with:
        array
        'swap:with:
        oneIndex
        otherIndex)))
  (perform:with:with:
    Set
    'addSelector:withMethod:
    'withArray:
    (lambda (self anArray) (set! array anArray)))
  (perform:with:with:
    (perform: Set 'class)
    'addSelector:withMethod:
    'new
    (lambda (self)
      (call/cc
        (return)
        (return (perform:with: self 'new: 4)))))
  (perform:with:with:
    (perform: Set 'class)
    'addSelector:withMethod:
    'new:
    (lambda (self nElements)
      (call/cc
        (return)
        (let ((initialSize nil))
          (perform:with:with:
            (perform:with: nElements '<= 0)
            'ifTrue:ifFalse:
            (lambda () (set! initialSize 1))
            (lambda ()
              (set! initialSize
                (perform:with:
                  (perform:with:
                    (perform:with: nElements '+ 1)
                    '*
                    4)
                  '//
                  3))))
          (return
            (perform:with:
              (perform: self 'basicNew)
              'init:
              initialSize)))))))

;;   -- e o f ---    ;;
