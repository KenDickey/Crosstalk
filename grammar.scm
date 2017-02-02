;; Method Grammar from the ANSI ST Standard
;; <method definition> changed for Candle-alike syntax.

(define st-grammar
  '((<method definition> 
	(<class-name> "~>" <message pattern> 
	"["
	[<temporaries> ]
	[<statements>]
	"]"))
    (<message pattern>
              (<unary pattern>)
              (<binary pattern>)
              (<keyword pattern>))
    (<unary pattern>
            (unarySelector))
    (<binary pattern>
             (binarySelector <method argument>))
    (<keyword pattern>
              ("keyword" <method argument> <keywordTail>))  ;;+
    (<keywordTail>
              ("keyword" <method argument> <keywordTail>)
              ())
    (<temporaries>
              ("|" <temporary variable list> "|")
              ())
    (<temporary variable list>
              ("identifier" <identiferList>);; *
              ())
    (<identiferList>
              ("identifier" <identiferList>)
              ("identifier"))

    (<initializer definition>
	     (<temporaries>)
	     (<temporaries><statements>)
             (<statements>))

    (<block constructor>
            ("[" <block body> "]"))
    (<block body>
             (<block arguments> <initializer definition>)
             (<initializer definition>))
    (<block arguments>
            (<block argument> <blockArgTail>)
            (<blockArgTail>))
    (blockArgTail>
            (<blockArgument> <blockArgTail>)
            ("|"))
    (<block argument>
            (":blockArg"))

    (<statements> 
	     (<return statement> ".")
	     (<return statement>)
             (<expression> "." <statements>)
             (<expression>))

    (<return statement>
             ("^" <expression>))

    (<expression>
	     (<assignment>)
	     (<basic expression>))
    (<assignment>
     (<assignment target> ":=" <expression>))
    (<basic expression>
          (<primary> <messages> <cascaded messages>)
          (<primary>)
          (<assignment target> ":=" "identifier"))
    (<primary>
          ("identifier")
	  (<literal>)
	  (<block constructor>)
	  ( "(" <expression> ")" ))

<messages> ::=
	   (<unary message>+ <binary message>* [<keyword message>] ) |
	   (<binary message>+ [<keyword message>] ) |
	   <keyword message>
<unary message> ::= unarySelector
<binary message> ::= binarySelector <binary argument>
<binary argument> ::= <primary> <unary message>*
<keyword message> ::= (keyword <keyword argument> )+
<keyword argument> ::= <primary> <unary message>* <binary message>*
<cascaded messages> ::= (";" <messages>)*

<literal> ::=
	  <number literal> |
	  <string literal> |
	  <character literal> |
	  <symbol literal> |
	  <selector literal> |
	  <array literal>

<number literal> ::= ["-"] <number>
<number> ::= integer | float | scaledDecimal

<array literal> ::= "#(" <array element>* ")"
<array element> ::= <literal> | identifier



;;;    		     --- E O F ---			;;;
