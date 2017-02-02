
(st->scm "Object ~> exampleWithNumber: x
 [   | y |
    true & false not & (nil isNil) ifFalse: [self halt].
    y := self size + super size.
    #($a #a \"a\" 1 1.0)
        do: [ :each |
            Transcript show: (each class name);
                       show: ' '].
    ^x < y
 ]" )

(debug-parser #true)
(trace-parse-methods #true)

(st-file->scm (string-append st-kernel-prefix "Collection.st"))

(st->scm "  start := anObject hash \\ self array size + 1.")